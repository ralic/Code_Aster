subroutine eifint(ndim, axi, nno1, nno2, npg,&
                  wref, vff1, vff2, dffr2, geom,&
                  ang, typmod, option, mat, compor,&
                  lgpg, crit, instam, instap, ddlm,&
                  ddld, iu, im, vim, sigp,&
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
! person_in_charge: jerome.laverne at edf.fr
!
! aslint: disable=W1504
    implicit none
!
#include "asterfort/codere.h"
#include "asterfort/eicine.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
!
    logical :: axi
    integer :: ndim, nno1, nno2, npg, mat, lgpg, iu(3, 18), im(3, 9)
    integer :: codret
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno2)
    real(kind=8) :: wref(npg)
    real(kind=8) :: crit(*), instam, instap
    real(kind=8) :: ddlm(2*nno1*ndim+nno2*ndim), ddld(2*nno1*ndim+nno2*ndim)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg), vect(2*nno1*ndim+nno2*ndim)
    real(kind=8) :: dffr2(ndim-1, nno2, npg), ang(*), sigp(2*ndim, npg), matr(*)
! ----------------------------------------------------------------------
!
!   RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR L'ELEMENT D'INTERFACE
!
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  AXI     : .TRUE. SI AXISYMETRIE
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  IDFF1   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE X)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE X)
! IN  DFFR2   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE L)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  WREF    : POIDS DES POINTS DE GAUSS DE REFERENCE
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  MAT     : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DDLM    : DDL A L'INSTANT PRECEDENT
! IN  DDLD    : INCREMENT DES DDL
! IN  IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! IN  IM      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
! OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! OUT CODRET  : CODE RETOUR
! ----------------------------------------------------------------------
    logical :: resi, rigi
    integer :: nddl, g, cod(27), n, i, m, j, k, l, os, kk
    real(kind=8) :: rbid(1), r(1), mu(3), su(3), wg, b(3, 3, 18), de(6)
    real(kind=8) :: ddedt(6, 6), t1
! ----------------------------------------------------------------------
!
!
! - INITIALISATION
!
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
    nddl = nno1*2*ndim + nno2*ndim
!
    do 5 g = 1, npg
        cod(g)=0
 5  end do
!
    call r8inir(3, 0.d0, su, 1)
    call r8inir(3, 0.d0, mu, 1)
!
    if (rigi) call r8inir((nddl*(nddl+1))/2, 0.d0, matr, 1)
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
!
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 1000 g = 1, npg
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF
!
        call eicine(ndim, axi, nno1, nno2, vff1(1, g),&
                    vff2(1, g), wref(g), dffr2(1, 1, g), geom, ang,&
                    wg, b)
!
        do 150 i = 1, ndim
            su(i) = 0.d0
            do 160 j = 1, ndim
                do 161 n = 1, 2*nno1
                    su(i) = su(i) + b(i,j,n)*(ddlm(iu(j,n))+ddld(iu(j, n)))
161              continue
160          continue
150      continue
!
        do 170 i = 1, ndim
            mu(i) = 0.d0
            do 180 n = 1, nno2
                mu(i) = mu(i) + vff2(n,g)*(ddlm(im(i,n))+ddld(im(i,n)) )
180          continue
170      continue
!
!
!      LOI DE COMPORTEMENT
!
!      CONVENTIONS :
!       1. MU EST RANGE DANS EPSM(1:3)
!       2. SU EST RANGE DANS EPSD(1:3)
!       3. DELTA EST RENVOYE DANS SIGP(1:3)             : DE
!       4. D(DELTA)/DT EST RENVOYE DANS DSIDEP(1:3,1:3) : DDEDT
!       5. R (PENALISATION) EST RENVOYE DANS TAMPON(1)  : R
!
        call nmcomp('RIGI', g, 1, ndim, typmod,&
                    mat, compor, crit, instam, instap,&
                    3, mu, su, 1, rbid,&
                    vim(1, g), option, rbid, 1, r,&
                    de, vip(1, g), 36, ddedt, 1,&
                    rbid, cod(g))
        if (cod(g) .eq. 1) goto 9000
!
!
!      FORCE INTERIEURE ET CONTRAINTES DE CAUCHY
!
        if (resi) then
!
!        STOCKAGE DES CONTRAINTES
            do 200 i = 1, ndim
                sigp( i,g) = mu(i) + r(1)*(su(i)-de(i))
                sigp(ndim+i,g) = su(i) - de(i)
200          continue
!
!        VECTEUR FINT:U
            do 300 n = 1, 2*nno1
                do 301 i = 1, ndim
                    kk = iu(i,n)
                    t1 = 0
                    do 320 k = 1, ndim
                        t1 = t1 + b(k,i,n)*sigp(k,g)
320                  continue
                    vect(kk) = vect(kk) + wg*t1
301              continue
300          continue
!
!        VECTEUR FINT:M
            do 350 n = 1, nno2
                do 351 i = 1, ndim
                    kk = im(i,n)
                    t1 = vff2(n,g)*sigp(ndim+i,g)
                    vect(kk) = vect(kk) + wg*t1
351              continue
350          continue
!
        endif
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE
!   STOCKAGE TRIANGLE INFERIEUR LIGNE DE DFI/DUJ
!
        if (rigi) then
!
!        MATRICE K:U(I,N),U(J,M)
            do 500 n = 1, 2*nno1
                do 501 i = 1, ndim
                    os = ((iu(i,n)-1)*iu(i,n))/2
                    do 520 m = 1, 2*nno1
                        do 521 j = 1, ndim
                            if (iu(j,m) .gt. iu(i,n)) goto 522
                            kk = os+iu(j,m)
                            t1 = 0
                            do 540 k = 1, ndim
                                t1 = t1 + b(k,i,n)*b(k,j,m)
                                do 550 l = 1, ndim
                                    t1 = t1 - b(k,i,n)*r(1)*ddedt(k,l) *b(l,j,m)
550                              continue
540                          continue
                            t1 = t1 * r(1)
                            matr(kk) = matr(kk) + wg*t1
522                          continue
521                      continue
520                  continue
501              continue
500          continue
!
!        MATRICES K:MU(I,N),U(J,M)
            do 600 n = 1, nno2
                do 601 i = 1, ndim
                    do 620 m = 1, 2*nno1
                        do 621 j = 1, ndim
                            if (im(i,n) .ge. iu(j,m)) then
                                kk = ((im(i,n)-1)*im(i,n))/2 + iu(j,m)
                            else
                                kk = ((iu(j,m)-1)*iu(j,m))/2 + im(i,n)
                            endif
                            t1 = vff2(n,g)*b(i,j,m)
                            do 650 l = 1, ndim
                                t1 = t1 - vff2(n,g)*r(1)*ddedt(i,l)*b( l,j,m)
650                          continue
                            matr(kk) = matr(kk) + wg*t1
621                      continue
620                  continue
601              continue
600          continue
!
!        MATRICES K:MU(I,N),MU(J,M)
            do 700 n = 1, nno2
                do 701 i = 1, ndim
                    os = ((im(i,n)-1)*im(i,n))/2
                    do 710 m = 1, nno2
                        do 711 j = 1, ndim
                            if (im(j,m) .gt. im(i,n)) goto 712
                            kk = os + im(j,m)
                            t1 = - vff2(n,g)*ddedt(i,j)*vff2(m,g)
                            matr(kk) = matr(kk) + wg*t1
712                          continue
711                      continue
710                  continue
701              continue
700          continue
!
        endif
!
1000  end do
!
!
! - SYNTHESE DES CODES RETOUR
!
9000  continue
    call codere(cod, npg, codret)
!
end subroutine
