subroutine mmbouc(ds_contact, loop_type, operation, loop_value_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=4), intent(in) :: loop_type
    character(len=4), intent(in) :: operation
    integer, intent(out), optional :: loop_value_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! All methods - Loops management
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  loop_type        : name of loop
!                        'Cont' - Contact status loop
!                        'Fric' - Friction loop
!                        'Goem' - Geometric loop
! In  operation        : type of operation on loop
!                        'READ' - Read value of loop iteration
!                        'INCR' - Add iteration to loop
!                        'INIT' - Initialization of loop
! Out loop_value       : value of loop (iteration)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_loop, nb_loop, loop_indx, loop_value
!
! --------------------------------------------------------------------------------------------------
!
    loop_indx = 0
    nb_loop   = ds_contact%nb_loop
    do i_loop = 1, nb_loop
        if (ds_contact%loop(i_loop)%type .eq. loop_type) then
            loop_indx = i_loop
        endif
    end do
    ASSERT(loop_indx .ne. 0)
!
    if (operation .eq. 'INIT') then
        ds_contact%loop(loop_indx)%counter = 0
    else if (operation.eq.'INCR') then
        ds_contact%loop(loop_indx)%counter = ds_contact%loop(loop_indx)%counter +1
    else if (operation.eq.'READ') then
        loop_value = ds_contact%loop(loop_indx)%counter
    else
        ASSERT(.false.)
    endif
!
    if (present(loop_value_)) then
        loop_value_ = loop_value
    endif
!
end subroutine
