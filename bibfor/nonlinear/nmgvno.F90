subroutine nmgvno(fami, ndim, nno1, nno2, npg,&
                  iw, vff1, vff2, idfde1, idfde2,&
                  geom, typmod, option, mat, compor,&
                  lgpg, crit, instam, instap, ddlm,&
                  ddld, angmas, sigm, vim, sigp,&
                  vip, matr, vect, codret)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/codere.h"
#include "asterfort/coefdg.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmepsi.h"
#include "asterfort/nmgvdn.h"
#include "asterfort/nmmabu.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=16) :: option, compor(*)
    integer :: ndim, nno1, nno2, npg, idfde1, idfde2, iw, mat, lgpg, codret
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg)
    real(kind=8) :: geom(ndim, nno1)
    real(kind=8) :: crit(*), instam, instap
    real(kind=8) :: ddlm(*), ddld(*), sigm(2*ndim+1, npg), sigp(2*ndim+1, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg), matr(*), vect(*)
    real(kind=8) :: angmas(3)
!
! ---------------------------------------------------------------------
!
!     RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR GRAD_VARI (2D ET 3D)
!
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  IDFDE1  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
! IN  IDFDE2  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE E)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS DE REFERENCE (INDICE)
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  MAT     : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  TEMPM   : TEMPERATURE AUX NOEUDS A L'INSTANT PRECEDENT
! IN  TEMPP   : TEMPERATURE AUX NOEUDS A L'INSTANT DE CALCUL
! IN  TREF    : TEMPERATURE DE REFERENCE
! IN  DDLM    : DDL A L'INSTANT PRECEDENT
! IN  DDLD    : INCREMENT DES DDL
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
! OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! OUT CODRET  : CODE RETOUR
! MEM DFDI2   :
! ---------------------------------------------------------------------
!
    integer :: k2(1)
    character(len=8) :: nom(1), famil, poum
!
    logical :: resi, rigi, grand, axi, elas, full
    integer :: nddl, ndimsi, g, cod(27), n, i, m, j, kl, pq, os, osa, kk
    integer :: iu(3*27), ia(8), kpg, spt
    real(kind=8) :: rac2, c, val(1)
    real(kind=8) :: deplm(3*27), depld(3*27), dfdi1(27, 3)
    real(kind=8) :: avm, avd, avp, agm(3), agd(3), agp(3), bp
    real(kind=8) :: r, wg, epsm(6), epsd(6), f(3, 3), b(6, 3, 27)
    real(kind=8) :: nonloc(2), sigmam(6), sigma(6), dsidep(6, 6, 4), t1, t2
    real(kind=8) :: di, char, r8bid
    real(kind=8) :: dfdi2(8*3)
    real(kind=8) :: critd(20)
!
    data  nom /'C_GRAD_V'/
!
! ---------------------------------------------------------------------
!
! - INITIALISATION
!
    resi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA'
    rigi = option.eq.'RIGI_MECA_TANG'
    full = option(1:9).eq.'FULL_MECA'
    elas = option.eq.'FULL_MECA_ELAS' .or. option.eq.'RIGI_MECA_ELAS'
!
    if (elas) then
        full = .false.
    endif
!
    rac2 = sqrt(2.d0)
    grand = .false.
    axi = typmod(1) .eq. 'AXIS'
    nddl = nno1*ndim + nno2
    ndimsi = 2*ndim
!
    call r8inir(2, 0.d0, nonloc, 1)
!
!      NOM(1) = 'C_GRAD_VARI'
!
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(famil, kpg, spt, poum, mat,&
                ' ', 'NON_LOCAL', 0, ' ', [0.d0],&
                1, nom, val, k2, 2)
    call coefdg(compor(1), mat, di)
!
    c = val(1)
!
    do 5 g = 1, npg
        cod(g)=0
 5  end do
!
    if (rigi) call r8inir((nddl*(nddl+1))/2, 0.d0, matr, 1)
    if (full) call r8inir((nddl*(nddl+1))/2, 0.d0, matr, 1)
    if (elas) call r8inir((nddl*(nddl+1))/2, 0.d0, matr, 1)
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
!
    call nmgvdn(ndim, nno1, nno2, iu, ia)
!
!    EXTRACTION DES DEPLACEMENTS
!
    do 10 n = 1, nno1
        do 20 i = 1, ndim
            deplm(i+(n-1)*ndim) = ddlm(iu(nno1*(i-1)+n))
            if (rigi) then
                depld(i+(n-1)*ndim) = 0.d0
            else
                depld(i+(n-1)*ndim) = ddld(iu(nno1*(i-1)+n))
            endif
20      continue
10  end do
!
! - CREATION D'UN VECTEUR VALANT 0 POUR ABSENCE DE DEPLACEMENT
!
    do 30 n = 1, nno2
        critd(n) = 0.d0
        do 40 i = 1, ndim
            critd(n) = critd(n) + abs(ddld(iu(nno1*(i-1)+n)))
40      continue
30  end do
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 1000 g = 1, npg
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR A
!
        call dfdmip(ndim, nno2, axi, geom, g,&
                    iw, vff2(1, g), idfde2, r, wg,&
                    dfdi2)
        avm = 0
        avd = 0
        do 50 n = 1, nno2
            avm = avm + vff2(n,g)*ddlm(ia(n))
            avd = avd + vff2(n,g)*ddld(ia(n))
            if (rigi) then
                avd = 0.d0
            endif
50      continue
        avp = avm + avd
!
        if (avp .gt. 1.d0) then
            avp = 1.d0
        endif
!
        do 60 i = 1, ndim
            agm(i) = 0
            agd(i) = 0
            do 70 n = 1, nno2
                agm(i) = agm(i) + dfdi2(nno2*(i-1)+n)*ddlm(ia(n))
                agd(i) = agd(i) + dfdi2(nno2*(i-1)+n)*ddld(ia(n))
                if (rigi) then
                    agd(i) = 0.d0
                endif
70          continue
            agp(i) = agm(i) + agd(i)
60      continue
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U
!
        call dfdmip(ndim, nno1, axi, geom, g,&
                    iw, vff1(1, g), idfde1, r, wg,&
                    dfdi1)
        call r8inir(6, 0.d0, epsm, 1)
        call r8inir(6, 0.d0, epsd, 1)
!
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dfdi1, deplm, f, epsm)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dfdi1, depld, f, epsd)
        call nmmabu(ndim, nno1, .false., grand, dfdi1,&
                    b)
        if (axi) then
            do 80 n = 1, nno1
                b(3,1,n) = vff1(n,g)/r
80          continue
        endif
!
        do 90 kl = 1, 3
            sigmam(kl) = sigm(kl,g)
90      continue
        do 100 kl = 4, ndimsi
            sigmam(kl) = sigm(kl,g)*rac2
100      continue
!
        nonloc(1)= avp
        nonloc(2)= c
!
        call nmcomp(fami, g, 1, ndim, typmod,&
                    mat, compor, crit, instam, instap,&
                    6, epsm, epsd, 6, sigmam,&
                    vim(1, g), option, angmas, 2, nonloc,&
                    sigma, vip(1, g), 6*6*4, dsidep, 1,&
                    r8bid, cod(g))
!
        if (cod(g) .eq. 1) goto 9000
!
!      FORCE INTERIEURE ET CONTRAINTES DE CAUCHY
!
        if (resi) then
!
!        CONTRAINTES
!
            do 110 kl = 1, 3
                sigp(kl,g) = sigma(kl)
110          continue
            do 120 kl = 4, ndimsi
                sigp(kl,g) = sigma(kl)/rac2
120          continue
!
            sigp(ndimsi+1,g) = dsidep(1,1,4)
            bp = sigp(ndimsi+1,g)
!
!        VECTEUR FINT:U
!
            do 130 n = 1, nno1
                do 140 i = 1, ndim
                    kk = iu(nno1*(i-1)+n)
                    t1 = 0
                    do 150 kl = 1, ndimsi
                        t1 = t1 + sigma(kl)*b(kl,i,n)
150                  continue
                    vect(kk) = vect(kk) + wg*t1
140              continue
130          continue
!
!        VECTEUR FINT:A
!
            do 160 n = 1, nno2
                t1 = vff2(n,g)*bp
                t2 = 0
                do 170 i = 1, ndim
                    t2 = t2 + c*dfdi2(nno2*(i-1)+n)*agp(i)
170              continue
                kk = ia(n)
                vect(kk) = vect(kk) + wg*(t2+t1)
160          continue
!
        endif
!
!   CALCUL DE LA MATRICE DE RIGIDITE
!   STOCKAGE TRIANGLE INFERIEUR LIGNE DE DFI/DUJ
!
        if (elas .or. rigi .or. full) then
!
!        MATRICE K:U(I,N),U(J,M)
!
            do 180 n = 1, nno1
                do 190 i = 1, ndim
                    os = ((iu(nno1*(i-1)+n)-1)*iu(nno1*(i-1)+n))/2
                    do 200 m = 1, nno1
                        do 210 j = 1, ndim
                            if (iu(nno1*(j-1)+m) .gt. iu(nno1*(i-1)+n)) goto 821
                            kk = os+iu(nno1*(j-1)+m)
                            t1 = 0
                            do 220 kl = 1, ndimsi
                                do 230 pq = 1, ndimsi
                                    t1 = t1+dsidep(kl,pq,1)*b(pq,j,m)* b(kl,i,n)
230                              continue
220                          continue
                            matr(kk) = matr(kk) + wg*t1
210                      continue
200                  continue
821                  continue
190              continue
180          continue
!
!        MATRICES K:A(N),A(M) SI ENDO NON-NUL
!
            do 240 n = 1, nno2
                osa = ((ia(n)-1)*ia(n))/2
                do 250 m = 1, nno2
                    t1 = vff2(n,g)*vff2(m,g)*dsidep(1,1,3)
                    t2 = 0
                    do 260 i = 1, ndim
                        t2 = t2 + dfdi2(nno2*(i-1)+n)*dfdi2(nno2*(i-1) +m)
260                  continue
                    t2 = c*t2
                    if (ia(m) .le. ia(n)) then
                        kk = osa+ia(m)
                        matr(kk) = matr(kk) + wg*(t2+t1)
                    endif
250              continue
240          continue
!
        endif
!
        if (rigi .or. full) then
!
!        MATRICES K:A(N),U(J,M)
!
            do 270 n = 1, nno2
                do 280 m = 1, nno1
                    do 290 j = 1, ndim
                        t1 = 0
                        do 300 kl = 1, ndimsi
                            t1 = t1 + dsidep(kl,1,2)*b(kl,j,m)
300                      continue
                        t1 = vff2(n,g)*t1
                        if (ia(n) .ge. iu(nno1*(j-1)+m)) then
                            kk = ((ia(n)-1)*ia(n))/2 + iu(nno1*(j-1)+ m)
                        else
                            kk = ( (iu(nno1*(j-1)+m)-1)*iu(nno1*(j-1)+ m) )/2 +ia(n )
                        endif
                        matr(kk) = matr(kk) + wg*t1
290                  continue
280              continue
270          continue
!
        endif
!
        if (elas .or. rigi .or. full) then
!
            do 310 n = 1, nno2
                osa = ((ia(n)-1)*ia(n))/2
                do 320 m = 1, nno2
                    if (ia(m) .le. ia(n)) then
                        kk=osa+ia(m)
!
                        char = ddld(ia(m))
!
                        if (char .eq. 0.d0 .and. critd(m) .ne. 0.d0) then
                            if (ia(m) .eq. ia(n)) then
                                matr(kk) = di
                            else
                                matr(kk) = 0.d0
                            endif
                        endif
!
                        char = ddld(ia(n))
!
                        if (char .eq. 0.d0 .and. critd(n) .ne. 0.d0) then
                            if (ia(m) .eq. ia(n)) then
                                matr(kk) = di
                            else
                                matr(kk) = 0.d0
                            endif
                        endif
!
                        if (ia(m) .eq. ia(n)) then
                            if (critd(n) .eq. 0.d0 .and. avp .eq. 0.d0) then
                                matr(kk) = di
                            endif
                        endif
                    endif
320              continue
310          continue
!
        endif
!
        if (rigi .or. full) then
!
            do 330 n = 1, nno2
!
                char = ddld(ia(n))
!
                if (char .eq. 0.d0 .and. critd(n) .ne. 0.d0) then
                    do 340 m = 1, nno1
                        do 350 j = 1, ndim
                            if (ia(n) .ge. iu(nno1*(j-1)+m)) then
                                kk=((ia(n)-1)*ia(n))/2 + iu(nno1*(j-1)&
                                +m)
                            else
                                kk=((iu(nno1*(j-1)+m)-1)*iu(nno1*(j-1)&
                                +m))/2+ia(n)
                            endif
                            matr(kk) = 0.d0
350                      continue
340                  continue
                endif
!
330          continue
!
        endif
!
1000  end do
!
9000  continue
!
    call codere(cod, npg, codret)
!
end subroutine
