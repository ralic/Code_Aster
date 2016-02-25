subroutine CreateMeasureDS(ds_measure)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
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
    type(NL_DS_Measure), intent(out) :: ds_measure
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistic management
!
! Create measure and statistic management datastructure
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_measure       : datastructure for measure and statistics management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer, parameter :: nb_device_defi = 23
    integer, parameter :: nb_timer_defi = 7
    integer :: i_device, i_timer
!
! - Name of timer
!
    character(len=9), parameter :: timer_type(nb_timer_defi) = (/&
                    'Time_Step','Newt_Iter','Store    ',&
                    'Post     ','Total    ','CPU_1    ',&
                    'CPU_2    '/)
!
! - Internal name of timer
!
    character(len=24), parameter :: cpu_name(nb_timer_defi) = (/&
                    'CPU.NMTIME.PAS','CPU.NMTIME.ITE','CPU.NMTIME.ARC',&
                    'CPU.NMTIME.PST','CPU.NMTIME.TOT','CPU.NMTIME.TM1',&
                    'CPU.NMTIME.TM2'/)
!
! - Type of device
!
    character(len=16), parameter :: device_type(nb_device_defi) = (/&
                    'Computation     ','Lost_Time       ','Time_Step       ',&
                    'Newt_Iter       ','Store           ','Post            ',&
                    'Integration     ','Factor          ','Second_Member   ',&
                    'Solve           ','Contact_Geometry','Contact_Algo    ',&
                    'Contact_Prepare ','Contact_Elem    ','Matrix_Assembly ',&
                    'Contact_NumbCont','Contact_NumbFric','Line_Search     ',&
                    'Contact_Cycl_1  ','Contact_Cycl_2  ','Contact_Cycl_3  ',&
                    'Contact_Cycl_4  ','Other           '/)
!
! - Timer linked to device
!
    character(len=9), parameter :: timer_name(nb_device_defi) = (/&
                    'Total    ','NoTimer  ','Time_Step',&
                    'Newt_Iter','Store    ','Post     ',&
                    'CPU_1    ','CPU_1    ','CPU_1    ',&
                    'CPU_1    ','CPU_1    ','CPU_1    ',&
                    'CPU_1    ','CPU_1    ','CPU_2    ',&
                    'NoTimer  ','NoTimer  ','NoTimer  ',&
                    'NoTimer  ','NoTimer  ','NoTimer  ',&
                    'NoTimer  ','NoTimer  '/)
!
! - Flag for counter add or not
!
    aster_logical, parameter :: l_count_add(nb_device_defi) = (/&
                    .true. , .true., .true.,&
                    .true. , .true., .true.,&
                    .true. , .true., .true.,&
                    .true. , .true., .true.,&
                    .true. , .true., .true.,&
                    .false.,.false., .true.,&
                    .true. , .true., .true.,&
                    .true. , .true./)
!
! - Flag for time measure: 2 by device - First = Step / Second = Total computation
!
    integer, parameter :: time_indi(2*nb_device_defi) = (/&
                     0,  2,  0,  3,  1,  0,&
                     0,  0,  0, 16,  0, 15,&
                     7,  7,  6,  6, 11, 11,&
                     8,  8, 10, 10,  9,  9,&
                    13, 13, 14, 14, 12, 12,&
                     0,  0,  0,  0,  0,  0,&
                     0,  0,  0,  0,  0,  0,&
                     0,  0, 17,  0/)
!
! - Flag for count measure: 2 by device - First = Step / Second = Total computation
!
    integer, parameter :: count_indi(2*nb_device_defi) = (/&
                     0,  0,  0,  0,  0, 25,&
                    26, 26,  0,  0,  0,  0,&
                     7,  7,  6,  6,  0,  0,&
                     8,  8, 10, 10,  9,  9,&
                    13, 13,  0,  0,  0,  0,&
                    18, 18, 19, 19, 24, 24,&
                    20, 20, 21, 21, 22, 22,&
                    23, 23,  0,  0/)
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Create measure and statistics management datastructure'
    endif
!
! - Main parameters
!
    ds_measure%nb_timer         = nb_timer_defi
    ds_measure%nb_device        = nb_device_defi
    ds_measure%store_mean_time  = 0.d0
    ds_measure%iter_mean_time   = 0.d0
    ds_measure%step_mean_time   = 0.d0
    ds_measure%iter_remain_time = 0.d0
    ds_measure%step_remain_time = 0.d0
    ds_measure%l_device_acti(1:ds_measure%nb_device_maxi) = .false._1
!
! - For timers
!
    do i_timer = 1, nb_timer_defi
        ds_measure%timer(i_timer)%type      = timer_type(i_timer)
        ds_measure%timer(i_timer)%cpu_name  = cpu_name(i_timer)
        ds_measure%timer(i_timer)%time_init = 0.d0
    end do
!
! - For devices
!
    do i_device = 1, nb_device_defi
        ds_measure%device(i_device)%type            = device_type(i_device)
        ds_measure%device(i_device)%timer_name      = timer_name(i_device)
        ds_measure%device(i_device)%time_indi_step  = time_indi(2*(i_device-1)+1)
        ds_measure%device(i_device)%time_indi_comp  = time_indi(2*(i_device-1)+2)
        ds_measure%device(i_device)%time_iter       = 0.d0
        ds_measure%device(i_device)%time_step       = 0.d0
        ds_measure%device(i_device)%time_comp       = 0.d0
        ds_measure%device(i_device)%l_count_add     = l_count_add(i_device)
        ds_measure%device(i_device)%count_indi_step = count_indi(2*(i_device-1)+1)
        ds_measure%device(i_device)%count_indi_comp = count_indi(2*(i_device-1)+2)
        ds_measure%device(i_device)%count_iter      = 0
        ds_measure%device(i_device)%count_step      = 0
        ds_measure%device(i_device)%count_comp      = 0
    end do
!
! - Checks
!
    ASSERT(ds_measure%nb_timer.le.ds_measure%nb_timer_maxi)
    ASSERT(ds_measure%nb_device.le.ds_measure%nb_device_maxi)
!
end subroutine
