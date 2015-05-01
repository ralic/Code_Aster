subroutine dsxhlt(df, jacob, hlt2)
    implicit  none
    real(kind=8) :: df(3, 3), jacob(*), hlt2(4, 6)
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
!     -----------------------------------------------------------------
!     MATRICE PRODUIT HL.T2(4,6)
!     -----------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: vj11, vj12, vj21, vj22, hl(4, 6), t2(3, 3)
!     ---------------------------------------------------------------
    vj11 = jacob(1)
    vj12 = jacob(2)
    vj21 = jacob(3)
    vj22 = jacob(4)
!
    hl(1,1) = df(1,1)
    hl(1,2) = - df(3,3)
    hl(1,3) = 0.d0
    hl(1,4) = df(1,3)
    hl(1,5) = - df(2,3)
    hl(1,6) = df(1,2) - df(3,3)
    hl(2,1) = df(1,3)
    hl(2,2) = - df(2,3)
    hl(2,3) = df(3,3) - df(1,2)
    hl(2,4) = df(3,3)
    hl(2,5) = - df(2,2)
    hl(2,6) = 0.d0
    hl(3,1) = df(1,2)
    hl(3,2) = 0.d0
    hl(3,3) = df(2,3)
    hl(3,4) = df(2,3)
    hl(3,5) = 0.d0
    hl(3,6) = df(2,2)
    hl(4,1) = 0.d0
    hl(4,2) = df(1,3)
    hl(4,3) = df(1,1)
    hl(4,4) = 0.d0
    hl(4,5) = df(1,2)
    hl(4,6) = df(1,3)
!
    t2(1,1) = vj11 * vj11
    t2(1,2) = vj12 * vj12
    t2(1,3) = 2.d0 * vj11 * vj12
    t2(2,1) = vj21 * vj21
    t2(2,2) = vj22 * vj22
    t2(2,3) = 2.d0 * vj21 * vj22
    t2(3,1) = vj11 * vj21
    t2(3,2) = vj12 * vj22
    t2(3,3) = vj11 * vj22 + vj12 * vj21
!
    do 100 k = 1, 4
        do 101 j = 1, 6
            hlt2(k,j) = 0.d0
101      continue
100  end do
    do 110 i = 1, 4
        do 110 j = 1, 3
            do 110 k = 1, 3
                hlt2(i,j) = hlt2(i,j) + hl(i,k) * t2(k,j)
                hlt2(i,j+3) = hlt2(i,j+3) + hl(i,k+3) * t2(k,j)
110          continue
end subroutine
