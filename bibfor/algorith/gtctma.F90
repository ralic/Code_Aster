subroutine gtctma(elem_coor, elem_nbnode,elem_code, elem_dime,&
                  ctcoor)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/reerel.h"
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

    real(kind=8), intent(in) :: elem_coor(3,9)
    integer, intent(in) :: elem_nbnode
    character(len=8), intent(in) :: elem_code
    integer, intent(in) :: elem_dime
    real(kind=8), intent(out) :: ctcoor(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Get center of a given contact element 
!
! --------------------------------------------------------------------------------------------------
!
! In elem_coor        : coordinates of nodes for current element
! In elem_nbnode      : number of node for current element
! In elem_code        : code of current element
! In elem_dime        : dimension of current element
! Out ctcoor           : coordonate of the center
!
! --------------------------------------------------------------------------------------------------
!
  real(kind=8) :: rfcoor(3), elem_cort(27)
  integer      :: i_dime, i_node
  
!
! --------------------------------------------------------------------------------------------------
!

!
! - Initialisation
!   
    ctcoor(1:3) = 0.d0
    select case (elem_code)
        case('SE2')
            rfcoor(1) = 0.d0
            rfcoor(2) = 0.d0
            rfcoor(3) = 0.d0
        case('SE3')
            rfcoor(1) = 0.d0
            rfcoor(2) = 0.d0
            rfcoor(3) = 0.d0
        case('TR3')
            rfcoor(1) = 1.d0/3.d0
            rfcoor(2) = 1.d0/3.d0
            rfcoor(3) = 0.d0
        case('TR6')
            rfcoor(1) = 1.d0/3.d0
            rfcoor(2) = 1.d0/3.d0
            rfcoor(3) = 0.d0
        case('QU4')
            rfcoor(1) = 0.d0
            rfcoor(2) = 0.d0
            rfcoor(3) = 0.d0
        case('QU8')
            rfcoor(1) = 0.d0 
            rfcoor(2) = 0.d0
            rfcoor(3) = 0.d0
        case('QU9')
            rfcoor(1) = 0.d0
            rfcoor(2) = 0.d0
            rfcoor(3) = 0.d0
        case default
            ASSERT(.false.)
    end select
!
! - Transform the format of slave element coordinates
!
    do i_node = 1,elem_nbnode
        do i_dime = 1, elem_dime
            elem_cort(elem_dime*(i_node-1)+i_dime) = elem_coor(i_dime, i_node)
        end do
    end do
!
!
! - Compute center 
!
   call reerel(elem_code, elem_nbnode, elem_dime, elem_cort, rfcoor,&
               ctcoor)
!
! - Print check
!
    !write(*,*)ctcoor(:)
!
end subroutine    
