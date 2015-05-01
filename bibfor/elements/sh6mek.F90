subroutine sh6mek(xetemp, sigma, re)
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
!               ELEMENT SHB6
!
    implicit none
#include "jeveux.h"
#include "asterfort/chrp3d.h"
#include "asterfort/r8inir.h"
#include "asterfort/rlosh6.h"
#include "asterfort/s6calb.h"
#include "asterfort/sh6ksi.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: iprope
    real(kind=8) :: xe(18), re(18, 18)
    real(kind=8) :: xxg5(5), pxg5(5), xcoq(3, 3), bksip(3, 6, 5)
    real(kind=8) :: xcent(3), ppp(3, 3), tmpke2(18, 18), sigmag(6)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3), sigma(*), tmpke(18, 18)
    real(kind=8) :: sigloc(6), sitmp1(6, 6), sitmp2(6, 6), xetemp(*)
    real(kind=8) :: b(3, 6)
!
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE COMME CA:
! (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2 ZNOEUD2,...)
! DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      IF (NOMSHB.EQ.'SHB8') THEN
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
!-----------------------------------------------------------------------
    integer :: i, ip, j, kk
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    xxg5(1) = -0.906179845938664d0
    xxg5(2) = -0.538469310105683d0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683d0
    xxg5(5) = 0.906179845938664d0
!
    pxg5(1) = 0.236926885056189d0
    pxg5(2) = 0.478628670499366d0
    pxg5(3) = 0.568888888888889d0
    pxg5(4) = 0.478628670499366d0
    pxg5(5) = 0.236926885056189d0
!
! -----------------------------------------------------
! ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
! SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
! -----------------------------------------------------
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 18
        xe(i) = xetemp(i)
10  end do
!
! CALCUL DE B (1 2 3) AUX 5 POINTS DE GAUSS
!
    call sh6ksi(5, xxg5, bksip)
    do 30 j = 1, 6
        do 20 i = 1, 6
            sitmp2(i,j) = 0.d0
20      continue
30  end do
!
! DEBUT DE LA BOUCLE SUR LES 5 PTS GAUSS
!
    do 240 ip = 1, 5
!
! CALCUL DE MATRICE DE PASSAGE POUR POUVOIR CALCULER LES
! CONTRAINTES DANS LE REPERE GLOBAL
!
        do 40 i = 1, 6
! C'EST LES CONTRAINTES LOCALES POUR POUVOIR TRAITER LA PLASTICITE AVANT
            sigloc(i) = sigma((ip-1)*6+i)
40      continue
!
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 60 i = 1, 3
            do 50 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+3)*3+j)
50          continue
60      continue
!
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
        call chrp3d(ppp, sigloc, sigmag, 1)
!
! CALCUL DE B
        call s6calb(bksip(1, 1, ip), xe, b, ajac)
!
! PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!
        do 100 j = 1, 6
            do 90 i = 1, 6
                sitmp1(i,j) = 0.d0
90          continue
100      continue
!
        do 120 j = 1, 6
            do 110 i = 1, 6
                sitmp1(i,j) = sigmag(1)*b(1,i)*b(1,j) + sigmag(2)*b(2, i)*b(2,j) + sigmag(3)*b(3,&
                              &i)*b(3,j) + sigmag(4)*(b(1, i)*b(2,j)+b(2,i)*b(1,j)) + 0.2025d0*si&
                              &gmag(6)*(b(1,i)* b(3,j)+b(3,i)*b(1,j))+ 0.2025d0*sigmag(5)*(b(3,i)&
                              &*b(2, j)+b(2,i)*b(3,j))
110          continue
120      continue
!
        do 140 j = 1, 6
            do 130 i = 1, 6
                sitmp2(i,j) = sitmp2(i,j) + 0.5d0*ajac*pxg5(ip)* sitmp1(i,j)
130          continue
140      continue
240  end do
!
    call r8inir(324, 0.d0, tmpke, 1)
    do 170 kk = 1, 3
        do 160 i = 1, 6
            do 150 j = 1, 6
                tmpke(i+(kk-1)*6,j+(kk-1)*6) = sitmp2(i,j)
150          continue
160      continue
170  end do
!
! ON MET DE L'ORDRE:
!
    call r8inir(324, 0.d0, tmpke2, 1)
    do 190 j = 1, 6
        do 180 i = 1, 18
            tmpke2(i,(j-1)*3+1) = tmpke(i,j)
            tmpke2(i,(j-1)*3+2) = tmpke(i,j+6)
            tmpke2(i,(j-1)*3+3) = tmpke(i,j+12)
180      continue
190  end do
!
    call r8inir(324, 0.d0, tmpke, 1)
    do 210 i = 1, 6
        do 200 j = 1, 18
            tmpke((i-1)*3+1,j) = tmpke2(i,j)
            tmpke((i-1)*3+2,j) = tmpke2(i+6,j)
            tmpke((i-1)*3+3,j) = tmpke2(i+12,j)
200      continue
210  end do
!
!         IPROPE = PROPEL(1)
    iprope = 1
    if (iprope .eq. 0) then
        call dcopy(324, tmpke, 1, re, 1)
    endif
!
    if (iprope .eq. 1) then
        call daxpy(324, 1.d0, tmpke, 1, re,&
                   1)
    endif
end subroutine
