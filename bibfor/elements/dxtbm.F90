subroutine dxtbm(jacob, bm)
    implicit none
    real(kind=8) :: jacob(*), bm(3, 6)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     MATRICE BM(3,6) EN MEMBRANE POUR LES ELEMENT DKT ET DST
!     ------------------------------------------------------------------
    real(kind=8) :: vj11, vj12, vj21, vj22
!     ------------------------------------------------------------------
    vj11 = jacob(1)
    vj12 = jacob(2)
    vj21 = jacob(3)
    vj22 = jacob(4)
!
    bm(1,1) = - vj11 - vj12
    bm(1,2) = 0.d0
    bm(1,3) = vj11
    bm(1,4) = 0.d0
    bm(1,5) = vj12
    bm(1,6) = 0.d0
    bm(2,1) = 0.d0
    bm(2,2) = - vj21 - vj22
    bm(2,3) = 0.d0
    bm(2,4) = vj21
    bm(2,5) = 0.d0
    bm(2,6) = vj22
    bm(3,1) = bm(2,2)
    bm(3,2) = bm(1,1)
    bm(3,3) = bm(2,4)
    bm(3,4) = bm(1,3)
    bm(3,5) = bm(2,6)
    bm(3,6) = bm(1,5)
!
end subroutine
