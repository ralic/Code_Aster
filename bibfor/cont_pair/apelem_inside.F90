subroutine apelem_inside(pair_tole   , elem_dime, elem_code,&
                         nb_poin_coor, poin_coor,&
                         nb_poin_inte, poin_inte)
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
    real(kind=8), intent(in) :: pair_tole
    integer, intent(in) :: elem_dime
    character(len=8), intent(in) :: elem_code
    integer, intent(in) :: nb_poin_coor
    real(kind=8), intent(in) :: poin_coor(elem_dime-1,4)
    integer, intent(inout) :: nb_poin_inte
    real(kind=8), intent(inout) :: poin_inte(elem_dime-1,16)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Add points in list of intersection point if inside element
!
! --------------------------------------------------------------------------------------------------
!
! In  pair_tole        : tolerance for pairing
! In  elem_dime        : dimension of elements
! In  elem_code        : code of element
! In  nb_poin_coor     : number of points 
! In  poin_coor        : parametric coordinates of points 
! IO  nb_poin_inte     : number of intersection points
! IO  poin_inte        : list of intersection points
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_node
    real(kind=8) :: xpt, ypt
!
! --------------------------------------------------------------------------------------------------
!
    if (elem_code .eq. 'SE2') then
        do i_node = 1, nb_poin_coor
            xpt = poin_coor(1, i_node)
            if (xpt .ge. (-1.d0-pair_tole) .and.&
                xpt .le. ( 1.d0+pair_tole)) then       
                nb_poin_inte              = nb_poin_inte+1
                ASSERT(nb_poin_inte.le.16)
                poin_inte(1,nb_poin_inte) = xpt
            endif
        end do
    elseif (elem_code .eq. 'TR3') then
        do i_node = 1, nb_poin_coor
            xpt=poin_coor(1,i_node)
            ypt=poin_coor(2,i_node)
            if (xpt .ge. -pair_tole .and.&
                ypt .ge. -pair_tole .and.&
               (ypt+xpt).le.(1.d0+pair_tole)) then       
                nb_poin_inte              = nb_poin_inte+1
                ASSERT(nb_poin_inte.le.16)
                poin_inte(1,nb_poin_inte) = xpt
                poin_inte(2,nb_poin_inte) = ypt
            endif
        end do
    elseif (elem_code .eq. 'QU4') then
        do i_node = 1, nb_poin_coor
            xpt=poin_coor(1,i_node)
            ypt=poin_coor(2,i_node)
            if (xpt .ge. (-1.d0-pair_tole) .and.&
                ypt .ge. (-1.d0-pair_tole) .and.&
                ypt .le. ( 1.d0+pair_tole) .and.&
                xpt .le. ( 1.d0+pair_tole)) then       
                nb_poin_inte              = nb_poin_inte+1
                ASSERT(nb_poin_inte.le.16)
                poin_inte(1,nb_poin_inte) = xpt
                poin_inte(2,nb_poin_inte) = ypt
            endif
        end do
    else
        ASSERT(.false.)
    end if
!
!
end subroutine
