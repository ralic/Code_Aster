subroutine mmbouc(ds_contact   , loop_type  , operation_ ,&
                  loop_counter_, loop_state_, loop_locus_, loop_vale_)
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
    character(len=*), intent(in) :: operation_
    integer, intent(out), optional :: loop_counter_
    aster_logical, intent(out), optional :: loop_state_
    character(len=16), intent(inout), optional :: loop_locus_
    real(kind=8), intent(inout), optional :: loop_vale_
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
!                        'Geom' - Geometric loop
! In  operation        : type of operation on loop
!                        'Read_Counter'    - Read value of loop counter
!                        'Incr_Counter'    - Add iteration to loop counter
!                        'Init_counter'    - Initialization of loop counter
!                        'Set_Convergence' - Set convergence of loop
!                        'Set_Error'       - Set error in loop
!                        'Set_NoError'     - Set no error in loop
!                        'Set_Divergence'  - Set divergence of loop
!                        'Is_Error'        - Return if loop error
!                        'Is_NoError'      - Return if no loop error
!                        'Is_Convergence'  - Return if loop converged
!                        'Is_Divergence'   - Return if loop not converged
!                        'Set_Locus'       - Set locus
!                        'Get_Locus'       - Get locus
!                        'Set_Vale'        - Set value of residual
!                        'Get_Vale'        - Get value of residual
! Out loop_counter     : value of loop (counter)
! Out loop_state       : value of flag (convergence/error)
! IO  loop_locus       : value of locus 
! IO  loop_vale        : value of residual 
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_loop, nb_loop, loop_indx, loop_counter
    character(len=16) :: operation
    aster_logical :: loop_state
!
! --------------------------------------------------------------------------------------------------
!
    operation    = operation_
    loop_state   = .false.
    loop_counter = 0
    loop_indx    = 0
!
! - Find loop
!
    nb_loop = ds_contact%nb_loop
    do i_loop = 1, nb_loop
        if (ds_contact%loop(i_loop)%type .eq. loop_type) then
            loop_indx = i_loop
        endif
    end do
    ASSERT(loop_indx .ne. 0)
!
! - Operation
!
    if (operation .eq. 'Init_Counter') then
        ds_contact%loop(loop_indx)%counter = 0
    else if (operation.eq.'Incr_Counter') then
        ds_contact%loop(loop_indx)%counter = ds_contact%loop(loop_indx)%counter +1
    else if (operation.eq.'Read_Counter') then
        loop_counter = ds_contact%loop(loop_indx)%counter
    else if (operation.eq.'Set_Convergence') then
        ds_contact%loop(loop_indx)%conv = .true.
    else if (operation.eq.'Set_Divergence') then
        ds_contact%loop(loop_indx)%conv = .false.
    else if (operation.eq.'Set_Error') then
        ds_contact%loop(loop_indx)%error = .true.
    else if (operation.eq.'Set_NoError') then
        ds_contact%loop(loop_indx)%error = .false.
    else if (operation.eq.'Is_Convergence') then
        if (ds_contact%loop(loop_indx)%conv) then
            loop_state = .true.
        else
            loop_state = .false.
        endif
    else if (operation.eq.'Is_Divergence') then
        if (ds_contact%loop(loop_indx)%conv) then
            loop_state = .false.
        else
            loop_state = .true.
        endif
    else if (operation.eq.'Is_Error') then
        if (ds_contact%loop(loop_indx)%error) then
            loop_state = .true.
        else
            loop_state = .false.
        endif
    else if (operation.eq.'Is_NoError') then
        if (ds_contact%loop(loop_indx)%error) then
            loop_state = .false.
        else
            loop_state = .true.
        endif
    else if (operation.eq.'Set_Locus') then
        ds_contact%loop(loop_indx)%locus_calc = loop_locus_
    else if (operation.eq.'Get_Locus') then
        loop_locus_ = ds_contact%loop(loop_indx)%locus_calc
    else if (operation.eq.'Set_Vale') then
        ds_contact%loop(loop_indx)%vale_calc = loop_vale_
    else if (operation.eq.'Get_Vale') then
        loop_vale_ = ds_contact%loop(loop_indx)%vale_calc        
    else
        ASSERT(.false.)
    endif
!
    if (present(loop_counter_)) then
        loop_counter_ = loop_counter
    endif
!
    if (present(loop_state_)) then
        loop_state_ = loop_state
    endif
!
end subroutine
