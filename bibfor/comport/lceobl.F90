subroutine lceobl(ndim, typmod, imate, crit, epsm,&
                  deps, vim, option, sigm, vip,&
                  dsidep, iret)
!
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1501
    implicit none
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/dfmdf.h"
#include "asterfort/dfpdf.h"
#include "asterfort/diago3.h"
#include "asterfort/lceob1.h"
#include "asterfort/lceob2.h"
#include "asterfort/lceob3.h"
#include "asterfort/lceobb.h"
#include "asterfort/meobl1.h"
#include "asterfort/meobl2.h"
#include "asterfort/meobl3.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/sigeob.h"
    character(len=8) :: typmod(*)
    character(len=16) :: option
    integer :: ndim, imate, iret
    real(kind=8) :: epsm(6), deps(6), vim(7), crit(*)
    real(kind=8) :: sigm(6), vip(7), dsidep(6, 6)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
! IN  EPSM    : DEFORMATION EN T- REPERE GLOBAL
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> SIG DSIDEP VIP
!                 RAPH_MECA      -> SIG        VIP
! OUT SIGM     : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES
!                 1 A 6   -> TENSEUR D'ENDOMMAGEMENT DE TRACTION
!                 7       -> ENDOMMAGEMENT DE COMPRESSION
! OUT DSIDEP  : MATRICE TANGENTE DEFO
! OUT         : IRET CODE RETOUR
! ----------------------------------------------------------------------
!
    aster_logical :: rigi, resi, elas, reinit
    aster_logical :: total, tot1, tot2, tot3, dbloq
    integer :: ndimsi, i, j, k, l, p, q, m, n, t(3, 3)
    integer :: bdim, r1(6), r2(6)
!
    integer :: intmax
    real(kind=8) :: toler
!
    real(kind=8) :: treps, treb, eps(6), cc(6)
    real(kind=8) :: kron(6), mult, seuil, rk, rk1, rk2, un, deux
    real(kind=8) :: rac2, bobo(6, 6), zozo(6, 6), zaza(6, 6)
    real(kind=8) :: rtemp1, rtemp2, rtemp3, rtemp4, rtemp5, rtemp6
    real(kind=8) :: e, nu, alpha, lambda, mu, ecrob, ecrod
    real(kind=8) :: b(6), bm(6), br(6)
    real(kind=8) :: dm, d, vecb(3, 3), valb(3), tolb, trepsm
    real(kind=8) :: interm(3, 3), epi(6), bmr(6), epsr(6)
    real(kind=8) :: vecbr(3, 3), valbr(3), binter(6), ad
    real(kind=8) :: deltab(6), deltad, dsisup(6, 6)
    real(kind=8) :: interb(3, 3), epib(6)
    real(kind=8) :: dsiint(6, 6), dsimed(6, 6)
    real(kind=8) :: vecepm(3, 3), valepm(3)
!
    integer :: icodre(6)
    character(len=16) :: nomres(6)
    real(kind=8) :: valres(6)
!
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
    un=1.d0
    deux=2.d0
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
    r1(1)=2
    r1(2)=3
    r1(3)=1
    r1(4)=6
    r1(5)=4
    r1(6)=5
!
    r2(1)=3
    r2(2)=1
    r2(3)=2
    r2(4)=5
    r2(5)=6
    r2(6)=4
!
    rac2=sqrt(deux)
    tolb=1.d-2
    mult=0.d0
!=====================================================================
!                            INITIALISATION
! ====================================================================
!
!---------------------------------------------------
! -- OPTION ET MODELISATION
!---------------------------------------------------
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    ndimsi = 2*ndim
    total =.false.
    reinit=.false.
!
!---------------------------------------------------
! -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES
!---------------------------------------------------
    nomres(1) = 'E'
    nomres(2) = 'NU'
    call rcvala(imate, ' ', 'ELAS', 0, ' ',&
                [0.d0], 2, nomres, valres, icodre,&
                1)
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (un+nu) / (un - deux*nu)
    mu = e/(deux*(un+nu))
!-------------------------------------------------
! -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
!-------------------------------------------------
    nomres(1) = 'ALPHA'
    nomres(2) = 'K0'
    nomres(3) = 'K1'
    nomres(4) = 'K2'
    nomres(5) = 'ECROB'
    nomres(6) = 'ECROD'
    call rcvala(imate, ' ', 'ENDO_ORTH_BETON', 0, ' ',&
                [0.d0], 6, nomres, valres, icodre,&
                1)
    alpha = valres(1)
    rk = valres(2)
    rk1 = valres(3)
    rk2 = valres(4)
    ecrob = valres(5)
    ecrod = valres(6)
!
    toler=crit(3)
    intmax=int(crit(1))
!
    elas=.true.
!
!---------------------------------------------
!---INITIALISATION MATRICE TANGENTE
!---------------------------------------------
    call r8inir(36, 0.d0, dsidep, 1)
    call r8inir(36, 0.d0, dsisup, 1)
!
!-------------------------------------------------
! -- DEFORMATIONS
!-------------------------------------------------
    if (resi) then
        do 1 k = 1, ndimsi
            eps(k) = epsm(k) + deps(k)
  1     continue
    else
        do 2 k = 1, ndimsi
            eps(k) = epsm(k)
  2     continue
        elas=.true.
    endif
!
    if (ndim .lt. 3) then
        do 456 i = ndimsi+1, 6
            eps(i)=0.d0
            epsm(i)=0.d0
            deps(i)=0.d0
456     continue
    endif
!
    do 301 i = 4, 6
        eps(i)=eps(i)/rac2
        epsm(i)=epsm(i)/rac2
        deps(i)=deps(i)/rac2
301 end do
!-------------------------------------------------
! -- ENDOMMAGEMENT DANS LE REPERE GLOBAL
!-------------------------------------------------
    do 3 i = 1, 3
        bm(i) = un-vim(i)
        b(i) = bm(i)
  3 end do
    do 300 i = 4, 6
        bm(i) = -vim(i)
        b(i) = bm(i)
300 end do
    dm=vim(7)
    d=dm
!
!------------------------------------------------------------------
!-- VERIFICATION SUR LES VALEURS PROPRES DE L ENDOMMAGEMENT
!-- DE TRACTION ET SUR L ENDOMMAGEMENT DE COMPRESSION POUR BLOQUAGE
!-- EVENTUEL DE L EVOLUTION
!------------------------------------------------------------------
    call diago3(b, vecb, valb)
    bdim=3
    do 201 i = 1, 3
        if (valb(i)-tolb .le. 0.d0) then
            bdim=bdim-1
        endif
201 end do
    dbloq=.false.
    if (d-(un-tolb) .ge. 0.d0) then
        dbloq=.true.
    endif
!
!-------------------------------------------------
!--DEFINITION DU SEUIL----------------------------
!-------------------------------------------------
!
    call diago3(epsm, vecepm, valepm)
    trepsm=eps(1)+eps(2)+eps(3)
    if (trepsm .gt. 0.d0) then
        trepsm=0.d0
    endif
    seuil=rk-rk1*trepsm*(atan2(-trepsm/rk2,un))
!
    if (resi) then
!----------------------------------------------------------------
!----CAS OU LES 3 VALEURS PROPRES D ENDO TRACTION SONT NON NULLES
!----------------------------------------------------------------
!
        if (bdim .eq. 3) then
            call lceob3(intmax, toler, eps, bm, dm,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, bdim, b, d, mult,&
                        elas, dbloq, iret)
!--VERIF SUR ENDO FINAL POUR VOIR SI ENDO DEPASSE 1 OU PAS
!-- SI ENDO DEPASSE 1 PLUS QUE TOLERANCE ON PASSE DANS LCEOBB
!-- QUI DECOUPE L INCREMENT DE CHARGE POUR ALLER DOUCEMENT A ENDO=1
!
            call diago3(b, vecb, valb)
!
            do 101 i = 1, 3
                if (valb(i) .lt. 0) then
                    reinit=.true.
                endif
                if (valb(i)-tolb .le. 0.d0) then
                    valb(i)=tolb-r8prem()
                    total=.true.
                endif
101         continue
            if (d .gt. 1.d0) then
                reinit=.true.
            endif
            if (un-d-tolb .le. 0.d0) then
                d=un-tolb+r8prem()
                dbloq=.true.
            endif
!
            if (.not.reinit) then
                if (total) then
                    call r8inir(6, 0.d0, b, 1)
                    do 212 i = 1, 3
                        do 213 j = i, 3
                            do 214 k = 1, 3
                                b(t(i,j))=b(t(i,j))+vecb(i,k)*valb(k)*&
                                vecb(j,k)
214                         continue
213                     continue
212                 continue
                endif
            else
                call lceobb(intmax, toler, epsm, deps, bm,&
                            dm, lambda, mu, alpha, ecrob,&
                            ecrod, rk, rk1, rk2, b,&
                            d, mult, elas, dbloq, iret)
            endif
!
!----------------------------------------------------------------
!----CAS OU 1 VALEUR PROPRE EST NULLE----------------------------
!----------------------------------------------------------------
!
        else if (bdim.eq.2) then
!
!-- ON RESTREINT L ESPACE CAR L ENDO N EVOLUE PLUS DANS 2 DIRECTIONS
!
            call r8inir(9, 0.d0, interm, 1)
            call r8inir(6, 0.d0, epi, 1)
            do 202 i = 1, 3
                do 203 l = 1, 3
                    do 204 k = 1, 3
                        interm(i,l)=interm(i,l)+vecb(k,i)*eps(t(k,l))
204                 continue
                    do 205 j = i, 3
                        epi(t(i,j))=epi(t(i,j))+interm(i,l)*vecb(l,j)
205                 continue
203             continue
202         continue
            tot1=.false.
            tot2=.false.
            tot3=.false.
            call r8inir(6, 0.d0, bmr, 1)
            if (valb(1)-tolb .le. 0.d0) then
                bmr(1)=valb(2)
                bmr(2)=valb(3)
                do 801 i = 1, 6
                    epsr(i)=epi(r1(i))
801             continue
                tot1=.true.
            else if (valb(2)-tolb.le.0.d0) then
                bmr(1)=valb(3)
                bmr(2)=valb(1)
                do 802 i = 1, 6
                    epsr(i)=epi(r2(i))
802             continue
                tot2=.true.
            else if (valb(3)-tolb.le.0.d0) then
                bmr(1)=valb(1)
                bmr(2)=valb(2)
                do 803 i = 1, 6
                    epsr(i)=epi(i)
803             continue
                tot3=.true.
            endif
!
            call lceob2(intmax, toler, epsr, bmr, dm,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, bdim, br, d, mult,&
                        elas, dbloq, iret)
!--VERIF SUR ENDO FINAL POUR VOIR SI ENDO DEPASSE 1 OU PAS
!-- SI ENDO DEPASSE 1 PLUS QUE TOLERANCE ON PASSE DANS LCEOBB
!-- QUI DECOUPE L INCREMENT DE CHARGE POUR ALLER DOUCEMENT A ENDO=1
!-- ENSUITE ON REVIENT AU 3D DANS REPERE INITIAL
!
            call diago3(br, vecbr, valbr)
            do 102 i = 1, 2
                if (valbr(i) .lt. 0) then
                    reinit=.true.
                endif
                if (valbr(i)-tolb .le. 0.d0) then
                    valbr(i)=tolb-r8prem()
                    total=.true.
                endif
102         continue
            if (d .gt. 1.d0) then
                reinit=.true.
            endif
            if (un-d-tolb .le. 0.d0) then
                d=un-tolb+r8prem()
                dbloq=.true.
            endif
            if (.not.reinit) then
                if (total) then
                    call r8inir(6, 0.d0, br, 1)
                    do 222 i = 1, 3
                        do 223 j = i, 3
                            do 224 k = 1, 3
                                br(t(i,j))=br(t(i,j))+vecbr(i,k)*&
                                valbr(k)*vecbr(j,k)
224                         continue
223                     continue
222                 continue
                endif
                call r8inir(6, 0.d0, binter, 1)
                if (tot1) then
                    binter(1)=tolb-r8prem()
                    binter(2)=br(1)
                    binter(3)=br(2)
                    binter(6)=br(4)
                else if (tot2) then
                    binter(1)=br(2)
                    binter(2)=tolb-r8prem()
                    binter(3)=br(1)
                    binter(5)=br(4)
                else if (tot3) then
                    binter(1)=br(1)
                    binter(2)=br(2)
                    binter(3)=tolb-r8prem()
                    binter(4)=br(4)
                endif
                call r8inir(9, 0.d0, interm, 1)
                call r8inir(6, 0.d0, b, 1)
                do 232 i = 1, 3
                    do 233 l = 1, 3
                        do 234 k = 1, 3
                            interm(i,l)=interm(i,l)+vecb(i,k)*binter(&
                            t(k,l))
234                     continue
                        do 235 j = i, 3
                            b(t(i,j))=b(t(i,j))+interm(i,l)*vecb(j,l)
235                     continue
233                 continue
232             continue
            else
                call lceobb(intmax, toler, epsm, deps, bm,&
                            dm, lambda, mu, alpha, ecrob,&
                            ecrod, rk, rk1, rk2, b,&
                            d, mult, elas, dbloq, iret)
            endif
!
!----------------------------------------------------------------
!----CAS OU 2 VALEURS PROPRES SONT NULLES------------------------
!----------------------------------------------------------------
!
        else if (bdim.eq.1) then
!
!-- ON RESTREINT L ESPACE CAR L ENDO N EVOLUE PLUS DANS UNE DIRECTION
!
            call r8inir(9, 0.d0, interm, 1)
            call r8inir(6, 0.d0, epi, 1)
            do 242 i = 1, 3
                do 243 l = 1, 3
                    do 244 k = 1, 3
                        interm(i,l)=interm(i,l)+vecb(k,i)*eps(t(k,l))
244                 continue
                    do 245 j = i, 3
                        epi(t(i,j))=epi(t(i,j))+interm(i,l)*vecb(l,j)
245                 continue
243             continue
242         continue
            tot1=.false.
            tot2=.false.
            tot3=.false.
            call r8inir(6, 0.d0, bmr, 1)
            if (valb(1)-tolb .gt. 0.d0) then
                bmr(1)=valb(1)
                do 804 i = 1, 6
                    epsr(i)=epi(i)
804             continue
                tot1=.true.
            else if (valb(2)-tolb.gt.0.d0) then
                bmr(1)=valb(2)
                do 805 i = 1, 6
                    epsr(i)=epi(r1(i))
805             continue
                tot2=.true.
            else if (valb(3)-tolb.gt.0.d0) then
                bmr(1)=valb(3)
                do 806 i = 1, 6
                    epsr(i)=epi(r2(i))
806             continue
                tot3=.true.
            endif
!
            call lceob1(intmax, toler, epsr, bmr, dm,&
                        lambda, mu, alpha, ecrob, ecrod,&
                        seuil, bdim, br, d, mult,&
                        elas, dbloq, iret)
!--VERIF SUR ENDO FINAL POUR VOIR SI ENDO DEPASSE 1 OU PAS
!-- SI ENDO DEPASSE 1 PLUS QUE TOLERANCE ON PASSE DANS LCEOBB
!-- QUI DECOUPE L INCREMENT DE CHARGE POUR ALLER DOUCEMENT A ENDO=1
!-- ENSUITE ON REVIENT AU 3D DANS REPERE INITIAL
!
            if (br(1) .lt. 0) then
                reinit=.true.
            endif
            if (br(1)-tolb .le. 0.d0) then
                br(1)=tolb-r8prem()
            endif
            if (d .gt. 1.d0) then
                reinit=.true.
            endif
            if (un-d-tolb .le. 0.d0) then
                d=un-tolb+r8prem()
                dbloq=.true.
            endif
            if (.not.reinit) then
                valb(1)=tolb-r8prem()
                valb(2)=tolb-r8prem()
                valb(3)=tolb-r8prem()
                if (tot1) valb(1)=br(1)
                if (tot2) valb(2)=br(1)
                if (tot3) valb(3)=br(1)
                call r8inir(6, 0.d0, b, 1)
                do 252 i = 1, 3
                    do 253 j = i, 3
                        do 254 k = 1, 3
                            b(t(i,j))=b(t(i,j))+vecb(i,k)*valb(k)*&
                            vecb(j,k)
254                     continue
253                 continue
252             continue
            else
                call lceobb(intmax, toler, epsm, deps, bm,&
                            dm, lambda, mu, alpha, ecrob,&
                            ecrod, rk, rk1, rk2, b,&
                            d, mult, elas, dbloq, iret)
            endif
!
        endif
!
!-----------------------------------------------------
!--ON RECUPERE L ENDOMMAGEMENT ET LA CONTRAINTE
!-----------------------------------------------------
        do 262 i = 1, 3
            vip(i)=un-b(i)
262     continue
        do 263 i = 4, 6
            vip(i)=-b(i)
263     continue
        vip(7)=d
        call sigeob(eps, b, d, 3, lambda,&
                    mu, sigm)
    endif
!
! ================================================================
!                            MATRICE TANGENTE
! ================================================================
!
!-on verifie l etat de B et D finaux pour calcul matrice tangente
!
    if (rigi) then
!
        call r8inir(36, 0.d0, dsidep, 1)
        if ((b(1).eq.1.d0) .and. (b(2).eq.1.d0) .and. (b(3).eq.1.d0) .and. (d.eq.0.d0)) then
            do 910 i = 1, 6
                dsidep(i,i)=dsidep(i,i)+deux*mu
910         continue
            do 911 i = 1, 3
                do 912 j = 1, 3
                    dsidep(i,j)=dsidep(i,j)+lambda
912             continue
911         continue
!
        else
!
            ad=(un-d)**deux
!
!
! -- DSIGMA/DEPS A ENDOMMAGEMENT CONSTANT = MATRICE SECANTE----
!
            call r8inir(6, 0.d0, cc, 1)
            do 98 i = 1, 3
                do 99 j = i, 3
                    do 100 k = 1, 3
                        cc(t(i,j))=cc(t(i,j))+b(t(i,k))*eps(t(k,j))+&
                        b(t(j,k))*eps(t(k,i))
100                 continue
 99             continue
 98         continue
            treps=0.d0
            treb=0.d0
            do 181 i = 1, 3
                treb=treb+cc(i)/deux
                treps=treps+eps(i)
181         continue
            if (treb .ge. 0.d0) then
                do 182 i = 1, 6
                    if (i .gt. 3) then
                        rtemp2=rac2
                    else
                        rtemp2=1.d0
                    endif
                    do 103 j = 1, 6
                        if (j .gt. 3) then
                            rtemp3=rac2
                        else
                            rtemp3=1.d0
                        endif
                        dsidep(i,j)=dsidep(i,j)+lambda*b(i)*b(j)*&
                        rtemp2*rtemp3
103                 continue
182             continue
            endif
            if (treps .lt. 0.d0) then
                do 104 i = 1, 6
                    if (i .gt. 3) then
                        rtemp2=rac2
                    else
                        rtemp2=1.d0
                    endif
                    do 105 j = 1, 6
                        if (j .gt. 3) then
                            rtemp3=rac2
                        else
                            rtemp3=1.d0
                        endif
                        dsidep(i,j)=dsidep(i,j)+lambda*ad*kron(i)*&
                        kron(j) *rtemp2*rtemp3
105                 continue
104             continue
            endif
            call dfmdf(6, eps, zozo)
            do 106 i = 1, 6
                do 107 j = 1, 6
                    dsidep(i,j)=dsidep(i,j)+deux*ad*mu*zozo(i,j)
107             continue
106         continue
            call dfpdf(6, cc, zaza)
            call r8inir(36, 0.d0, bobo, 1)
            do 108 i = 1, 3
                do 109 j = i, 3
                    if (i .eq. j) then
                        rtemp2=1.d0
                    else
                        rtemp2=rac2
                    endif
                    do 110 p = 1, 3
                        do 111 q = 1, 3
                            if (p .eq. q) then
                                rtemp3=1.d0
                            else
                                rtemp3=un/rac2
                            endif
                            do 112 k = 1, 3
                                if (k .eq. i) then
                                    rtemp4=1.d0
                                else
                                    rtemp4=un/rac2
                                endif
                                if (k .eq. j) then
                                    rtemp5=1.d0
                                else
                                    rtemp5=un/rac2
                                endif
                                do 113 m = 1, 3
                                    do 114 n = 1, 3
                                        if (m .eq. n) then
                                            rtemp6=1.d0
                                        else
                                            rtemp6=1/rac2
                                        endif
                                        bobo(t(i,j),t(p,q))=bobo(t(i,&
                                        j),t(p,q))+(zaza(t(i,k),t(m,n)&
                                        )* (b(t(m,p))*kron(t(q,n))+&
                                        kron(t(m,p))* b(t(q,n)))*&
                                        rtemp4*b(t(k,j)) +(zaza(t(k,j)&
                                        ,t(m,n))*rtemp5* (b(t(m,p))*&
                                        kron(t(q,n))+kron(t(m,p))*&
                                        b(t(q,n)))*b(t(i,k))))*rtemp2*&
                                        rtemp3*rtemp6
114                                 continue
113                             continue
112                         continue
111                     continue
110                 continue
109             continue
108         continue
            do 115 i = 1, 6
                do 116 j = 1, 6
                    dsidep(i,j)=dsidep(i,j)+mu/deux*bobo(i,j)
116             continue
115         continue
            call r8inir(36, 0.d0, dsisup, 1)
            if (option(10:14) .ne. '_ELAS') then
                if (.not.elas) then
!
                    if (bdim .eq. 3) then
!
                        call r8inir(6, 0.d0, deltab, 1)
                        do 250 i = 1, 6
                            deltab(i)=b(i)-bm(i)
250                     continue
                        deltad=d-dm
!
                        call meobl3(eps, b, d, deltab, deltad,&
                                    mult, lambda, mu, ecrob, ecrod,&
                                    alpha, rk1, rk2, bdim, dsisup)
!
                    else if (bdim.eq.2) then
!
                        call diago3(b, vecb, valb)
!
                        call r8inir(9, 0.d0, interm, 1)
                        call r8inir(9, 0.d0, interb, 1)
                        call r8inir(6, 0.d0, epi, 1)
                        call r8inir(6, 0.d0, epib, 1)
                        do 302 i = 1, 3
                            do 303 l = 1, 3
                                do 304 k = 1, 3
                                    interm(i,l)=interm(i,l)+vecb(k,i)*&
                                    eps(t(k,l))
                                    interb(i,l)=interb(i,l)+vecb(k,i)*&
                                    bm(t(k,l))
304                             continue
                                do 305 j = i, 3
                                    epi(t(i,j))=epi(t(i,j))+interm(i,&
                                    l)*vecb(l,j)
                                    epib(t(i,j))=epib(t(i,j))+interb(&
                                    i,l)*vecb(l,j)
305                             continue
303                         continue
302                     continue
                        tot1=.false.
                        tot2=.false.
                        tot3=.false.
                        call r8inir(6, 0.d0, br, 1)
                        call r8inir(6, 0.d0, deltab, 1)
                        if (abs(valb(1))-tolb .le. 0.d0) then
!
                            br(1)=valb(2)
                            br(2)=valb(3)
                            do 807 i = 1, 6
                                epsr(i)=epi(r1(i))
807                         continue
                            deltab(1)=valb(2)-epib(2)
                            deltab(2)=valb(3)-epib(3)
                            deltab(4)=-epib(6)
                            deltad=d-dm
                            tot1=.true.
                        else if (abs(valb(2))-tolb.le.0.d0) then
!
                            br(1)=valb(3)
                            br(2)=valb(1)
                            do 808 i = 1, 6
                                epsr(i)=epi(r2(i))
808                         continue
                            deltab(1)=valb(3)-epib(3)
                            deltab(2)=valb(1)-epib(1)
                            deltab(4)=-epib(5)
                            deltad=d-dm
                            tot2=.true.
!
                        else if (abs(valb(3))-tolb.le.0.d0) then
!
                            br(1)=valb(1)
                            br(2)=valb(2)
                            do 809 i = 1, 6
                                epsr(i)=epi(i)
809                         continue
                            deltab(1)=valb(1)-epib(1)
                            deltab(2)=valb(2)-epib(2)
                            deltab(4)=-epib(4)
                            deltad=d-dm
                            tot3=.true.
!
                        endif
!
                        call meobl2(epsr, br, d, deltab, deltad,&
                                    mult, lambda, mu, ecrob, ecrod,&
                                    alpha, rk1, rk2, bdim, dsiint)
!
                        if (tot1) then
                            do 811 i = 1, 6
                                do 812 j = 1, 6
                                    if (i .ge. 4) then
                                        rtemp1=un/rac2
                                    else
                                        rtemp1=1.d0
                                    endif
                                    if (j .ge. 4) then
                                        rtemp2=un/rac2
                                    else
                                        rtemp2=1.d0
                                    endif
                                    dsimed(r1(i),r1(j))=dsiint(i,j)*&
                                    rtemp1*rtemp2
812                             continue
811                         continue
                        else if (tot2) then
                            do 813 i = 1, 6
                                do 814 j = 1, 6
                                    if (i .ge. 4) then
                                        rtemp1=un/rac2
                                    else
                                        rtemp1=1.d0
                                    endif
                                    if (j .ge. 4) then
                                        rtemp2=un/rac2
                                    else
                                        rtemp2=1.d0
                                    endif
                                    dsimed(r2(i),r2(j))=dsiint(i,j)*&
                                    rtemp1*rtemp2
814                             continue
813                         continue
                        else if (tot3) then
                            do 815 i = 1, 6
                                do 816 j = 1, 6
                                    if (i .ge. 4) then
                                        rtemp1=un/rac2
                                    else
                                        rtemp1=1.d0
                                    endif
                                    if (j .ge. 4) then
                                        rtemp2=un/rac2
                                    else
                                        rtemp2=1.d0
                                    endif
                                    dsimed(i,j)=dsiint(i,j)*rtemp1*&
                                    rtemp2
816                             continue
815                         continue
                        endif
!
                        call r8inir(36, 0.d0, dsisup, 1)
!
                        do 850 i = 1, 3
                            do 851 j = i, 3
                                if (i .eq. j) then
                                    rtemp1=1.d0
                                else
                                    rtemp1=rac2
                                endif
                                do 852 p = 1, 3
                                    do 853 q = p, 3
                                        if (p .eq. q) then
                                            rtemp2=1.d0
                                        else
                                            rtemp2=rac2
                                        endif
                                        do 860 k = 1, 3
                                            do 861 l = 1, 3
                                                do 862 m = 1, 3
                                                    do 863 n = 1, 3
!
                                                        dsisup(t(i,j),t(p,q))=dsisup(&
                                        t(i,j),t(p,q)) +vecb(i,k)*&
                                        vecb(j,l)*vecb(p,m)*vecb(q,n)&
                                        *dsimed(t(k,l),t(m,n))*rtemp1*&
                                        rtemp2
863                                                 continue
862                                             continue
861                                         continue
860                                     continue
853                                 continue
852                             continue
851                         continue
850                     continue
!
!--------------------------------------------------------
!--------------------------------------------------------
!
                    else if (bdim.eq.1) then
!
                        call diago3(b, vecb, valb)
!
                        call r8inir(9, 0.d0, interm, 1)
                        call r8inir(9, 0.d0, interb, 1)
                        call r8inir(6, 0.d0, epi, 1)
                        call r8inir(6, 0.d0, epib, 1)
                        do 602 i = 1, 3
                            do 603 l = 1, 3
                                do 604 k = 1, 3
                                    interm(i,l)=interm(i,l)+vecb(k,i)*&
                                    eps(t(k,l))
                                    interb(i,l)=interb(i,l)+vecb(k,i)*&
                                    bm(t(k,l))
604                             continue
                                do 605 j = i, 3
                                    epi(t(i,j))=epi(t(i,j))+interm(i,&
                                    l)*vecb(l,j)
                                    epib(t(i,j))=epib(t(i,j))+interb(&
                                    i,l)*vecb(l,j)
605                             continue
603                         continue
602                     continue
!
                        tot1=.false.
                        tot2=.false.
                        tot3=.false.
                        call r8inir(6, 0.d0, br, 1)
                        call r8inir(6, 0.d0, deltab, 1)
!
                        if (abs(valb(1))-tolb .gt. 0.d0) then
!
                            br(1)=valb(1)
                            do 607 i = 1, 6
                                epsr(i)=epi(i)
607                         continue
                            deltab(1)=valb(1)-epib(1)
                            deltad=d-dm
                            tot1=.true.
!
                        else if (abs(valb(2))-tolb.gt.0.d0) then
!
                            br(1)=valb(2)
!
                            do 608 i = 1, 6
                                epsr(i)=epi(r1(i))
608                         continue
                            deltab(1)=valb(2)-epib(2)
                            deltad=d-dm
                            tot2=.true.
!
                        else if (abs(valb(3))-tolb.gt.0.d0) then
!
                            br(1)=valb(3)
!
                            do 609 i = 1, 6
                                epsr(i)=epi(r2(i))
609                         continue
                            deltab(1)=valb(3)-epib(3)
                            deltad=d-dm
                            tot3=.true.
!
                        endif
!
                        call meobl1(epsr, b, d, deltab, deltad,&
                                    mult, lambda, mu, ecrob, ecrod,&
                                    alpha, rk1, rk2, bdim, dsiint)
!
                        if (tot1) then
                            do 611 i = 1, 6
                                do 612 j = 1, 6
                                    if (i .ge. 4) then
                                        rtemp1=un/rac2
                                    else
                                        rtemp1=1.d0
                                    endif
                                    if (j .ge. 4) then
                                        rtemp2=un/rac2
                                    else
                                        rtemp2=1.d0
                                    endif
                                    dsimed(i,j)=dsiint(i,j)*rtemp1*&
                                    rtemp2
612                             continue
611                         continue
                        else if (tot2) then
                            do 613 i = 1, 6
                                do 614 j = 1, 6
                                    if (i .ge. 4) then
                                        rtemp1=un/rac2
                                    else
                                        rtemp1=1.d0
                                    endif
                                    if (j .ge. 4) then
                                        rtemp2=un/rac2
                                    else
                                        rtemp2=1.d0
                                    endif
                                    dsimed(r1(i),r1(j))=dsiint(i,j)*&
                                    rtemp1*rtemp2
614                             continue
613                         continue
                        else if (tot3) then
                            do 615 i = 1, 6
                                do 616 j = 1, 6
                                    if (i .ge. 4) then
                                        rtemp1=un/rac2
                                    else
                                        rtemp1=1.d0
                                    endif
                                    if (j .ge. 4) then
                                        rtemp2=un/rac2
                                    else
                                        rtemp2=1.d0
                                    endif
                                    dsimed(r2(i),r2(j))=dsiint(i,j)*&
                                    rtemp1*rtemp2
616                             continue
615                         continue
                        endif
!
                        call r8inir(36, 0.d0, dsisup, 1)
!
                        do 650 i = 1, 3
                            do 651 j = i, 3
                                if (i .eq. j) then
                                    rtemp1=1.d0
                                else
                                    rtemp1=rac2
                                endif
                                do 652 p = 1, 3
                                    do 653 q = p, 3
                                        if (p .eq. q) then
                                            rtemp2=1.d0
                                        else
                                            rtemp2=rac2
                                        endif
                                        do 660 k = 1, 3
                                            do 661 l = 1, 3
                                                do 662 m = 1, 3
                                                    do 663 n = 1, 3
!
                                                        dsisup(t(i,j),t(p,q))=dsisup(&
                                        t(i,j),t(p,q)) +vecb(i,k)*&
                                        vecb(j,l)*vecb(p,m)*vecb(q,n)&
                                        *dsimed(t(k,l),t(m,n))*rtemp1*&
                                        rtemp2
663                                                 continue
662                                             continue
661                                         continue
660                                     continue
653                                 continue
652                             continue
651                         continue
650                     continue
!
                    endif
!
                endif
            endif
!
            do 120 i = 1, 6
                do 121 j = 1, 6
                    dsidep(i,j)=dsidep(i,j)+dsisup(i,j)
121             continue
120         continue
!
        endif
    endif
end subroutine
