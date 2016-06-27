subroutine apdcma(elem_code, elin_sub, elin_nbnode, elin_nbsub)
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
    character(len=8), intent(in) :: elem_code
    integer, intent(out) :: elin_sub(8,4)
    integer, intent(out) :: elin_nbnode(8)
    integer, intent(out) :: elin_nbsub
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Cut element in linearized sub-elements (SEG2 or TRIA3)
!
! --------------------------------------------------------------------------------------------------
!
! SEG2  => 1xSEG2
! SEG3  => 2xSEG2
! TRIA3 => 1xTRIA3
! TRIA6 => 4xTRIA3
! QUAD4 => 2xTRIA3
! QUAD8 => 6xTRIA3
! QUAD9 => 2xTRIA3
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_code        : code of current element
! Out elin_nbsub       : number of linearized sub-elements
! Out elin_nbnode      : number of nodes for each linearized sub-element
! Out elin_sub         : list of nodes for each linearized sub-element
!
! --------------------------------------------------------------------------------------------------
!
    elin_sub(:,:)  = 0
    elin_nbnode(:) = 0
    elin_nbsub     = 0
    if (elem_code .eq. 'SE2') then
        elin_nbsub     = 1
        elin_nbnode(1) = 2
        elin_sub(1,1)  = 1
        elin_sub(1,2)  = 2 
    elseif (elem_code .eq. 'SE3') then
        elin_nbsub     = 2
        elin_nbnode(1) = 2
        elin_sub(1,1)  = 1
        elin_sub(1,2)  = 3 
        elin_nbnode(2) = 2
        elin_sub(2,1)  = 3
        elin_sub(2,2)  = 2 
    elseif (elem_code .eq. 'TR3') then
        elin_nbsub     = 1
        elin_nbnode(1) = 3
        elin_sub(1,1)  = 1
        elin_sub(1,2)  = 2           
        elin_sub(1,3)  = 3       
    else if (elem_code .eq. 'TR6') then
        elin_nbsub     = 4
        elin_nbnode(1) = 3
        elin_sub(1,1)  = 1
        elin_sub(1,2)  = 4           
        elin_sub(1,3)  = 6
        elin_nbnode(2) = 3
        elin_sub(2,1)  = 4
        elin_sub(2,2)  = 2           
        elin_sub(2,3)  = 5
        elin_nbnode(3) = 3
        elin_sub(3,1)  = 4
        elin_sub(3,2)  = 5          
        elin_sub(3,3)  = 6
        elin_nbnode(4) = 3
        elin_sub(4,1)  = 5
        elin_sub(4,2)  = 3          
        elin_sub(4,3)  = 6 
    else if (elem_code .eq. 'QU4') then
        elin_nbsub     = 2
        elin_nbnode(1) = 3
        elin_sub(1,1)  = 1
        elin_sub(1,2)  = 2           
        elin_sub(1,3)  = 3
        elin_nbnode(2) = 3
        elin_sub(2,1)  = 3
        elin_sub(2,2)  = 4           
        elin_sub(2,3)  = 1        
    else if (elem_code .eq. 'QU8') then
        elin_nbsub     = 6
        elin_nbnode(1) = 3
        elin_sub(1,1)  = 1
        elin_sub(1,2)  = 5          
        elin_sub(1,3)  = 8
        elin_nbnode(2) = 3
        elin_sub(2,1)  = 5
        elin_sub(2,2)  = 2           
        elin_sub(2,3)  = 6
        elin_nbnode(3) = 3
        elin_sub(3,1)  = 6
        elin_sub(3,2)  = 3           
        elin_sub(3,3)  = 7
        elin_nbnode(4) = 3
        elin_sub(4,1)  = 7
        elin_sub(4,2)  = 4           
        elin_sub(4,3)  = 8
        elin_nbnode(5) = 3
        elin_sub(5,1)  = 5
        elin_sub(5,2)  = 6           
        elin_sub(5,3)  = 7
        elin_nbnode(6) = 3
        elin_sub(6,1)  = 7
        elin_sub(6,2)  = 8           
        elin_sub(6,3)  = 5
    else if (elem_code .eq. 'QU9') then
        elin_nbsub     = 8
        elin_nbnode(1) = 3
        elin_sub(1,1)  = 1
        elin_sub(1,2)  = 5           
        elin_sub(1,3)  = 9
        elin_nbnode(2) = 3
        elin_sub(2,1)  = 5
        elin_sub(2,2)  = 2           
        elin_sub(2,3)  = 9
        elin_nbnode(3) = 3
        elin_sub(3,1)  = 2
        elin_sub(3,2)  = 6         
        elin_sub(3,3)  = 9
        elin_nbnode(4) = 3
        elin_sub(4,1)  = 6
        elin_sub(4,2)  = 3          
        elin_sub(4,3)  = 9
        elin_nbnode(5) = 3
        elin_sub(5,1)  = 3
        elin_sub(5,2)  = 7           
        elin_sub(5,3)  = 9
        elin_nbnode(6) = 3
        elin_sub(6,1)  = 7
        elin_sub(6,2)  = 4           
        elin_sub(6,3)  = 9
        elin_nbnode(7) = 3
        elin_sub(7,1)  = 4
        elin_sub(7,2)  = 8         
        elin_sub(7,3)  = 9
        elin_nbnode(8) = 3
        elin_sub(8,1)  = 8
        elin_sub(8,2)  = 1          
        elin_sub(8,3)  = 9
    else
        ASSERT(.false.)
    end if
!
end subroutine
