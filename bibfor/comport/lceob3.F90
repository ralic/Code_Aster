subroutine lceob3(intmax, tole, eps, bm, dm,&
                  lambda, mu, alpha, ecrob, ecrod,&
                  seuil, bdim, b, d, mult,&
                  elas, dbloq, iret)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
#include "asterfort/ceobfb.h"
#include "asterfort/ceobfd.h"
#include "asterfort/dfbdb.h"
#include "asterfort/dfmdf.h"
#include "asterfort/mgauss.h"
#include "asterfort/r8inir.h"
    real(kind=8) :: eps(6)
    real(kind=8) :: bm(6), dm, b(6), d, mult
    real(kind=8) :: lambda, mu, alpha, seuil, ecrob, ecrod
    real(kind=8) :: tole
!
    integer :: intmax, iret, bdim
!
    aster_logical :: elas, dbloq
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
!     ROUTINE DE RESOLUTION DU SYSTEME NON LINEAIRE
!     ALGORITHME DE NEWTON
!
!
!
!  IN INTMAX  : NBRE D'ITERATION MAX POUR LE NEWTON LOCAL
!  IN TOLE    : RESIDU TOLERE POUR LE NEWTON LOCAL
!  IN  BDIM   : DIMENSION DE L'ESPACE
!  IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
!  IN  EPSM   : DEFORMATION EN T- REPERE GLOBAL
!  IN  DEPS   : INCREMENT DE DEFORMATION
!  IN  BM DM  : VARIABLES INTERNES EN T-
!  IN LAMBDA  : /
!  IN MU      : / COEFFICIENTS DE LAME
!  IN  ALPHA  : /
!  IN  ECROB  : /
!  IN  ECROD  : / PARAMETRES DU MODELE
!  IN  SEUIL  : SEUIL DU CRITERE D'ENDOMMAGEMENT
!  IN  BDIM   : DIMENSION DE L ESPACE
!
! OUT  B D    : VARIABLES INTERNES EN T+
! OUT MULT    : MULTIPLICATEUR PLASTIQUE DU PRINCIPE DE NORMALITE
! OUT ELAS    : ELASTIQUE OU DISSIPATION?
! OUT DBLOQ   : BLOQUAGE DE L'ENDOMMAGEMENT DE COMPRESSION
! OUT IRET    : CODE RETOUR
! ----------------------------------------------------------------------
!
    integer :: i, j, k, compte, iret1
!
    real(kind=8) :: fb(6), db(6), fd, dd, fbm(6), resb(6)
    real(kind=8) :: rac2, un, deux
    real(kind=8) :: rtemp2, rtemp3, delta1(6), delta2
    real(kind=8) :: ddg, tolc, det, tata, normrb, rtemp, crit
    real(kind=8) :: mte1(6, 6), mte2(6, 6)
    real(kind=8) :: ksi(6, 6), iksi(6, 6), toti(6, 6), ide(6, 6)
    real(kind=8) :: teme(6, 6), coupl
    real(kind=8) :: resd, dfddd, psi
    real(kind=8) :: inter1, inter2, inter3, inter4
!
    deux=2.d0
    rac2=sqrt(deux)
    tolc=seuil*tole
    un=1.d0
    compte=0
    mult=0.d0
!
    do 100 i = 1, 6
        b(i)=bm(i)
100 end do
!
    d=dm
!
!-------------------------------------------------------
!-------------------------------------------------------
!----CALCUL DE FB: FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION
!
    call ceobfb(b, eps, lambda, mu, ecrob,&
                bdim, fb, rtemp, fbm)
!
!----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION
!
    if (dbloq) then
        fd=0.d0
    else
        call ceobfd(d, eps, lambda, mu, ecrod,&
                    fd)
    endif
!
!----CALCUL DU CRITERE-------------------------------------
!
    coupl=sqrt(alpha*rtemp+(1-alpha)*fd**2)
    crit=coupl-seuil
!
    elas=.false.
!
    if (crit .le. tolc) then
        elas=.true.
        goto 999
!
    else
!
        do 32 i = 1, 6
            resb(i)=-b(i)+bm(i)+alpha*mult*fbm(i)
 32     continue
        resd=-d+dm+(1-alpha)*mult*fd
        do 33 i = 4, 6
            resb(i)=rac2*resb(i)
 33     continue
!
        tata=0.d0
        do 37 i = 1, 6
            tata=tata+resb(i)*resb(i)
 37     continue
!
        normrb=sqrt(tata)
!
        ddg=0.d0
!
!--------------------------------------------------------
!--BOUCLE DU NEWTON SUR LES VARIABLES INTERNES-----------
!--------------------------------------------------------
!
 38     continue
        if (((crit.gt.tolc).or.(normrb.gt.tole).or.(abs(resd).gt.tole))) then
            if ((compte.lt.intmax) .and. (coupl.ne.0.d0)) then
! Rajout du test sur COUPL (fiche 15020) : lorsque c'est le cas,
! la derivee du residu est une matrice singuliere et le systeme ne
! peut etre resolu. On sort pour enclencher la decoupe du pas de temps
!
                call dfmdf(6, fb, mte1)
                call dfbdb(3, b, eps, deux*mu, lambda,&
                           ecrob, mte2)
!
                dfddd=0.d0
!
                if ((.not.dbloq) .and. (fd.ne.0.d0)) then
                    dfddd=-(fd+deux*ecrod)/(un-d)
                endif
!
                call r8inir(36, 0.d0, ksi, 1)
                do 40 i = 1, 6
                    do 41 j = 1, 6
                        do 42 k = 1, 6
                            ksi(i,j)=ksi(i,j)-mult*alpha*mte1(i,k)*&
                            mte2(k,j)
 42                     continue
 41                 continue
 40             continue
!
                do 43 i = 1, 6
                    ksi(i,i)=ksi(i,i)+1
 43             continue
!
                do 44 i = 1, 6
                    do 45 j = 1, 6
                        toti(i,j)=ksi(i,j)
 45                 continue
 44             continue
                do 46 i = 1, 6
                    do 47 j = 1, 6
                        if (i .eq. j) then
                            ide(i,j)=1.d0
                        else
                            ide(i,j)=0.d0
                        endif
 47                 continue
 46             continue
                call r8inir(36, 0.d0, teme, 1)
                do 48 i = 1, 6
                    do 49 j = 1, 6
                        teme(i,j)=ide(i,j)
 49                 continue
 48             continue
                call mgauss('NFVP', toti, teme, 6, 6,&
                            6, det, iret1)
                call r8inir(36, 0.d0, iksi, 1)
                do 51 i = 1, 6
                    do 52 j = 1, 6
                        iksi(i,j)=teme(i,j)
 52                 continue
 51             continue
!
                psi=1-mult*(1-alpha)*dfddd
!
                call r8inir(6, 0.d0, delta1, 1)
                do 53 i = 1, 6
                    do 54 j = 1, 6
                        if (j .ge. 4) then
                            rtemp2=rac2
                        else
                            rtemp2=1.d0
                        endif
                        delta1(i)=delta1(i)+alpha/coupl*rtemp2*fbm(j)*&
                        mte2(j,i)
 54                 continue
 53             continue
!
                delta2=(1-alpha)/coupl*fd*dfddd
!
                inter1=0.d0
                inter3=0.d0
                do 55 i = 1, 6
                    do 56 j = 1, 6
                        if (j .ge. 4) then
                            rtemp2=rac2
                        else
                            rtemp2=1.d0
                        endif
                        inter1=inter1+delta1(i)*iksi(i,j)*resb(j)
                        inter3=inter3+alpha*rtemp2*delta1(i) *iksi(i,&
                        j)*fbm(j)
 56                 continue
 55             continue
!
                inter2=delta2/psi*resd
                inter4=delta2/psi*(1-alpha)*fd
!
                ddg=-(crit+inter1+inter2)/(inter3+inter4)
!
                call r8inir(6, 0.d0, db, 1)
!
                dd=resd/psi+ddg*(1-alpha)*fd/psi
                do 57 i = 1, 6
                    do 58 j = 1, 6
                        if (i .ge. 4) then
                            rtemp2=1/rac2
                        else
                            rtemp2=1.d0
                        endif
                        if (j .ge. 4) then
                            rtemp3=rac2
                        else
                            rtemp3=1.d0
                        endif
                        db(i)=db(i)+rtemp2*iksi(i,j)* (resb(j)+ddg*&
                        alpha*fbm(j)*rtemp3)
 58                 continue
 57             continue
!
                do 59 i = 1, 6
                    b(i)=b(i)+db(i)
 59             continue
                d=d+dd
!
                compte=compte+1
                mult=mult+ddg
!
!----CALCUL DE FB DANS NEWTON---------------------------
!
                call ceobfb(b, eps, lambda, mu, ecrob,&
                            bdim, fb, rtemp, fbm)
!
!----CALCUL DE FD DANS NEWTON----------------------------
                if (dbloq) then
                    fd=0.d0
                else
                    call ceobfd(d, eps, lambda, mu, ecrod,&
                                fd)
                endif
!
!----CALCUL DU CRITERE-------------------------------------
                coupl=sqrt(alpha*rtemp+(1-alpha)*fd**2)
                crit=coupl-seuil
!
                do 132 i = 1, 6
                    resb(i)=-b(i)+bm(i)+alpha*mult*fbm(i)
132             continue
                resd=-d+dm+(1-alpha)*mult*fd
                do 133 i = 4, 6
                    resb(i)=rac2*resb(i)
133             continue
!
                tata=0.d0
                do 137 i = 1, 6
                    tata=tata+resb(i)*resb(i)
137             continue
!
                normrb=sqrt(tata)
!
                goto 38
            else
                iret = 1
                goto 999
            endif
        endif
!
    endif
999 continue
!
end subroutine
