subroutine nmrvai(ds_measure, device_type_, phasis, input_count, output_count)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/GetDevice.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=*), intent(in) :: device_type_
    character(len=1), optional, intent(in) :: phasis
    integer, optional, intent(in) :: input_count
    integer, optional, intent(out) :: output_count
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistic management
!
! Counters management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
! In  device_type      : type of current device
! In  phasis           : phasis (time step, Newton iteration, all computation)
! In  input_count      : value to increase counter
! Out output_count     : value of counter
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: operation
    integer :: device_indx
    type(NL_DS_Device) :: device
    aster_logical :: l_count_add
!
! --------------------------------------------------------------------------------------------------
!
    operation = ' '
!
! - Get current device
!
    call GetDevice(ds_measure, device_type_, device, device_indx)
!
! - Which type of operation ?
!
    if (present(input_count)) then
        ASSERT(.not.present(output_count))
        operation = 'Write'
    elseif (present(output_count)) then
        ASSERT(.not.present(input_count))
        operation = 'Read'
    else
        ASSERT(.false.)
    endif 
!
! - Write
!
    if (operation .eq. 'Write') then
        l_count_add = device%l_count_add
        if (present(phasis)) then
            if (phasis.eq.'N') then
                if (l_count_add) then
                    device%count_iter = device%count_iter + input_count
                else
                    device%count_iter = input_count
                endif
            elseif (phasis.eq.'P') then
                if (l_count_add) then
                    device%count_step = device%count_step + input_count
                else
                    device%count_step = input_count
                endif
            elseif (phasis.eq.'T') then
                if (l_count_add) then
                    device%count_comp = device%count_comp + input_count
                else
                    device%count_comp = input_count
                endif
            else
                ASSERT(.false.)
            endif
        else
            if (l_count_add) then
                device%count_iter = device%count_iter + input_count
            else
                device%count_iter = input_count
            endif
            if (l_count_add) then
                device%count_step = device%count_step + input_count
            else
                device%count_step = input_count
            endif
            if (l_count_add) then
                device%count_comp = device%count_comp + input_count
            else
                device%count_comp = input_count
            endif
        endif
        ds_measure%device(device_indx) = device
    elseif (operation .eq. 'Read') then
        if (phasis.eq.'N') then
            output_count = device%count_iter
        elseif (phasis.eq.'P') then
            output_count = device%count_step
        elseif (phasis.eq.'T') then
            output_count = device%count_comp
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
