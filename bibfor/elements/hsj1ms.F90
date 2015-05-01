subroutine hsj1ms(epais, vectg, vectt, hsfm, hss,&
                  hsj1m, hsj1s)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/jacbm1.h"
    real(kind=8) :: epais
    real(kind=8) :: vectg(2, 3), vectt(3, 3), matj(3, 3), jm1(3, 3), detj
    real(kind=8) :: hsfm(3, 9), hss(2, 9), hsj1m(3, 9), hsj1s(2, 9)
!
!     CONSTRUCTION DE J-1 AUX PTS D'INTEGRATION REDUITS.   J = JACOBIEN
!
!-----------------------------------------------------------------------
    integer :: i, j, j1, jb, k, k1
!-----------------------------------------------------------------------
    call jacbm1(epais, vectg, vectt, matj, jm1,&
                detj)
!
!     CONSTRUCTION DE HFM * S * JTILD-1  ET  HS * S * JTILD-1
!                                            AUX PTS DE GAUSS REDUITS
!
!     PARTITION EN BLOCS DE HSFM ET HSS
!
!     HFM * S = HSFM = ( HSFM11 , HSFM12 , HSFM13 )  OU   HSFM1J (3,3)
!
!     HS  * S = HSS =  ( HSS11 , HSS12 , HSS13 )     OU   HSS1J (2,3)
!
!               J-1 0  0
!     JTILD-1 = 0  J-1 0                        OU   J-1   (3,3)
!               0   0  J-1
!
!           IB=1,1
    do 10 jb = 1, 3
        do 20 i = 1, 3
            do 30 j = 1, 3
                j1=j+3*(jb-1)
                hsj1m(i,j1)=0.d0
                if (i .le. 2) hsj1s(i,j1)=0.d0
                do 40 k = 1, 3
                    k1=k+3*(jb-1)
                    if (i .le. 2) hsj1s(i,j1)=hsj1s(i,j1)+hss(i,k1)*jm1( k,j)
                    hsj1m(i,j1)=hsj1m(i,j1)+hsfm(i,k1)*jm1(k,j)
40              end do
30          end do
20      end do
10  end do
!
end subroutine
