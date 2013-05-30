subroutine dstbfb(jacob, bfb)
    implicit  none
    real(kind=8) :: jacob(*), bfb(3, 9)
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
!     ------------------------------------------------------------------
!     MATRICE BFB(3,9) POUR L'ELEMENT DST
!     ------------------------------------------------------------------
    integer :: k, j
    real(kind=8) :: vj11, vj12, vj21, vj22
!     ------------------------------------------------------------------
    vj11 = jacob(1)
    vj12 = jacob(2)
    vj21 = jacob(3)
    vj22 = jacob(4)
!
    do 100 k = 1, 3
        do 101 j = 1, 9
            bfb(k,j) = 0.d0
101      continue
100  end do
    bfb(1,2) = - vj11 - vj12
    bfb(1,5) = vj11
    bfb(1,8) = vj12
    bfb(2,3) = - vj21 - vj22
    bfb(2,6) = vj21
    bfb(2,9) = vj22
    bfb(3,2) = - vj21 - vj22
    bfb(3,3) = - vj11 - vj12
    bfb(3,5) = vj21
    bfb(3,6) = vj11
    bfb(3,8) = vj22
    bfb(3,9) = vj12
!
end subroutine
