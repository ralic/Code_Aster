subroutine dsxhft(df, jacob, hft2)
    implicit  none
    real(kind=8) :: df(3, 3), jacob(*), hft2(2, 6)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     MATRICE PRODUIT HF.T2(2,6)
!     -----------------------------------------------------------------
    integer :: j, k
    real(kind=8) :: vj11, vj12, vj21, vj22, hf(2, 6), t2(3, 3)
!     ---------------------------------------------------------------
    vj11 = jacob(1)
    vj12 = jacob(2)
    vj21 = jacob(3)
    vj22 = jacob(4)
!
    hf(1,1) = df(1,1)
    hf(1,2) = df(3,3)
    hf(1,3) = 2.d0 * df(1,3)
    hf(1,4) = df(1,3)
    hf(1,5) = df(2,3)
    hf(1,6) = df(1,2) + df(3,3)
    hf(2,1) = df(1,3)
    hf(2,2) = df(2,3)
    hf(2,3) = df(1,2) + df(3,3)
    hf(2,4) = df(3,3)
    hf(2,5) = df(2,2)
    hf(2,6) = 2.d0 * df(2,3)
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
    do 100 j = 1, 2
        do 100 k = 1, 6
            hft2(j,k) = 0.d0
100      continue
    do 110 j = 1, 3
        do 110 k = 1, 3
            hft2(1,j) = hft2(1,j) + hf(1,k) * t2(k,j)
            hft2(1,j+3) = hft2(1,j+3) + hf(1,k+3) * t2(k,j)
            hft2(2,j) = hft2(2,j) + hf(2,k) * t2(k,j)
            hft2(2,j+3) = hft2(2,j+3) + hf(2,k+3) * t2(k,j)
110      continue
end subroutine
