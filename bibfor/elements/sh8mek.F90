subroutine sh8mek(xetemp, sigma, re)
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
! ELEMENT SHB8-PS
!               ELEMENT SHB8
!
    implicit none
#include "jeveux.h"
#include "asterfort/chrp3d.h"
#include "asterfort/r8inir.h"
#include "asterfort/rloshb.h"
#include "asterfort/shbksi.h"
#include "asterfort/shcalb.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: iprope
    real(kind=8) :: xe(24), re(24, 24)
    real(kind=8) :: xxg5(5), pxg5(5), xcoq(3, 4), bksip(3, 8, 5), b(3, 8)
    real(kind=8) :: xcent(3), ppp(3, 3), tmpke2(24, 24), sigmag(6)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3), sigma(*), tmpke(24, 24)
    real(kind=8) :: sigloc(6), sitmp1(8, 8), sitmp2(8, 8), xetemp(*)
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
! -----------------------------------------------------
! ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
! SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
! -----------------------------------------------------
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 24
        xe(i) = xetemp(i)
10  end do
!
! CALCUL DE B (1 2 3) AUX 5 POINTS DE GAUSS
!
    call shbksi(5, xxg5, bksip)
    do 930 j = 1, 8
        do 920 i = 1, 8
            sitmp2(i,j) = 0.d0
920      continue
930  end do
!
! DEBUT DE LA BOUCLE SUR LES 5 PTS GAUSS
!
    do 990 ip = 1, 5
!
! CALCUL DE B
!
        call shcalb(bksip(1, 1, ip), xe, b, ajac)
!
! CALCUL DE MATRICE DE PASSAGE POUR POUVOIR CALCULER LES
! CONTRAINTES DANS LE REPERE GLOBAL
!
        do 940 i = 1, 6
! C'EST LES CONTRAINTES LOCALES POUR POUVOIR TRAITER LA PLASTICITE AVANT
            sigloc(i) = sigma((ip-1)*6+i)
940      continue
        zeta = xxg5(ip)
        zlamb = 0.5d0* (1.d0-zeta)
        do 960 i = 1, 4
            do 950 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+4)*3+j)
950          continue
960      continue
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!
        call chrp3d(ppp, sigloc, sigmag, 1)
        do 962 j = 1, 8
            do 961 i = 1, 8
                sitmp1(i,j) = 0.d0
961          continue
962      continue
!
        do 964 j = 1, 8
            do 963 i = 1, 8
                sitmp1(i,j) = sigmag(1)*b(1,i)*b(1,j) + sigmag(2)*b(2, i)*b(2,j) + sigmag(3)*b(3,&
                              &i)*b(3,j) + sigmag(4)*(b(1, i)*b(2,j)+b(2,i)*b(1,j)) + sigmag(6)*(&
                              &b(1,i)*b(3,j)+b( 3,i)*b(1,j)) + sigmag(5)*(b(3,i)*b(2,j)+b(2,i)*b(&
                              &3,j))
963          continue
964      continue
        do 980 j = 1, 8
            do 970 i = 1, 8
                sitmp2(i,j) = sitmp2(i,j) + 4.d0*ajac*pxg5(ip)*sitmp1( i,j)
970          continue
980      continue
990  end do
    call r8inir(576, 0.d0, tmpke, 1)
    do 1020 kk = 1, 3
        do 1010 i = 1, 8
            do 1000 j = 1, 8
                tmpke(i+(kk-1)*8,j+(kk-1)*8) = sitmp2(i,j)
1000          continue
1010      continue
1020  end do
!
! ON MET DE L'ORDRE:
!
    call r8inir(576, 0.d0, tmpke2, 1)
    do 1040 j = 1, 8
        do 1030 i = 1, 24
            tmpke2(i, (j-1)*3+1) = tmpke(i,j)
            tmpke2(i, (j-1)*3+2) = tmpke(i,j+8)
            tmpke2(i, (j-1)*3+3) = tmpke(i,j+16)
1030      continue
1040  end do
!
    call r8inir(576, 0.d0, tmpke, 1)
    do 1060 i = 1, 8
        do 1050 j = 1, 24
            tmpke((i-1)*3+1,j) = tmpke2(i,j)
            tmpke((i-1)*3+2,j) = tmpke2(i+8,j)
            tmpke((i-1)*3+3,j) = tmpke2(i+16,j)
1050      continue
1060  end do
!
    iprope = 1
    if (iprope .eq. 0) then
        call dcopy(576, tmpke, 1, re, 1)
    endif
!
    if (iprope .eq. 1) then
        call daxpy(576, 1.d0, tmpke, 1, re,&
                   1)
    endif
!      K = 0
!      DO 750 J = 1,24
!          DO 740 I = 1,24
!            K = K + 1
!            REV(K) = RE(I,J)
!  740     CONTINUE
!  750 CONTINUE
!*      WRITE(6,*) 'RE = ',RE
end subroutine
