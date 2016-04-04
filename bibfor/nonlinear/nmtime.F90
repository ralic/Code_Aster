subroutine nmtime(ds_measure, operation_, device_type_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/nmrtim.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
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
    character(len=*), intent(in) :: operation_
    character(len=*), intent(in) :: device_type_
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistic management
!
! Timer management for device
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
! In  operation        : operation to do Init, Launch or stop timer for current device
! In  device_type      : type of current device
!
! --------------------------------------------------------------------------------------------------
!
    type(NL_DS_Device) :: device
    character(len=9) :: timer_type
    character(len=10) :: device_type
    integer :: i_timer, timer_indx, nb_timer
    type(NL_DS_Timer) :: timer
    character(len=24) :: operation, cpu_name
    real(kind=8) :: time, list_time(7)
!
! --------------------------------------------------------------------------------------------------
!
    operation      = operation_
    list_time(1:7) = 0.d0
!
! - Get current device
!
    call GetDevice(ds_measure, device_type_, device)
    timer_type  = device%timer_name
    device_type = device%type
!
! - Find timer
!
    nb_timer   = ds_measure%nb_timer
    timer_indx = 0
    do i_timer = 1, nb_timer
        if (ds_measure%timer(i_timer)%type .eq. timer_type) then
            ASSERT(timer_indx.eq.0)
            timer_indx = i_timer
        endif
    end do
!
! - Get current timer
!
    ASSERT(timer_indx.ne.0)
    timer     = ds_measure%timer(timer_indx)
    cpu_name  = timer%cpu_name
!
! - Operations
!
    if (operation .eq. 'Init') then
        call uttcpu(cpu_name, 'INIT', ' ')
        timer%time_init = 0.d0
    elseif (operation .eq. 'Launch') then
        call uttcpu(cpu_name, 'DEBUT', ' ')
    elseif (operation .eq. 'Stop') then
        call uttcpu(cpu_name, 'FIN', ' ')
        call uttcpr(cpu_name, 7, list_time)
        time = list_time(7) - timer%time_init
        timer%time_init = list_time(7)
        if (timer_type.eq.'Store') then
            ds_measure%store_mean_time = list_time(4)
        endif
        if (timer_type.eq.'Time_Step') then
            ds_measure%step_mean_time   = list_time(4)
            ds_measure%step_remain_time = list_time(1)
        endif
        if (timer_type.eq.'Newt_Iter') then
            ds_measure%iter_mean_time   = list_time(4)
            ds_measure%iter_remain_time = list_time(1)
        endif
        call nmrtim(ds_measure, device_type, time)
    else
        ASSERT(.false.)
    endif
!
! - Save timer
!
    ds_measure%timer(timer_indx)   = timer
!
end subroutine
