subroutine dktbnl(jacob, bnl)
    implicit none
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
    real(kind=8) :: jacob(*), bnl(2, 9)
!     ------------------------------------------------------------------
!     MATRICE BNL(3,9) EN MEMBRANE NON-LINEAIRE
!                      POUR L'ELEMENT DKT
!     ------------------------------------------------------------------
    real(kind=8) :: vj11, vj12, vj21, vj22
!     ------------------------------------------------------------------
!
    vj11 = jacob(1)
    vj12 = jacob(2)
    vj21 = jacob(3)
    vj22 = jacob(4)
!
    bnl(1,1) = - vj11 - vj12
    bnl(1,2) = 0.d0
    bnl(1,3) = 0.d0
    bnl(1,4) = vj11
    bnl(1,5) = 0.d0
    bnl(1,6) = 0.d0
    bnl(1,7) = vj12
    bnl(1,8) = 0.d0
    bnl(1,9) = 0.d0
!
    bnl(2,1) = - vj22 - vj21
    bnl(2,2) = 0.d0
    bnl(2,3) = 0.d0
    bnl(2,4) = vj21
    bnl(2,5) = 0.d0
    bnl(2,6) = 0.d0
    bnl(2,7) = vj22
    bnl(2,8) = 0.d0
    bnl(2,9) = 0.d0
!
end subroutine
