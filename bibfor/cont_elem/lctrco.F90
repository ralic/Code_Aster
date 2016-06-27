subroutine lctrco(i_tria, tria_node, poin_inte, tria_coor)
!
implicit none
!
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: i_tria
    integer, intent(in) :: tria_node(6, 3)
    real(kind=8), intent(in) :: poin_inte(2, 16)
    real(kind=8), intent(out) :: tria_coor(2, 3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Coordinates of current triangle
!
! --------------------------------------------------------------------------------------------------
!
! In  i_tria           : index of current triangle
! In  tria_node        : list of triangles (defined by index of intersection points)
! In  poin_inte        : list (sorted) of intersection points
! Out tria_coor        : coordinates of current triangle
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_node
!
! --------------------------------------------------------------------------------------------------
!
    do i_node=1, 3
        tria_coor(1,i_node) = poin_inte(1,tria_node(i_tria,i_node))
        tria_coor(2,i_node) = poin_inte(2,tria_node(i_tria,i_node))
    end do
!
end subroutine
