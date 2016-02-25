subroutine nmstat(phasis, ds_measure, ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/impfot.h"
#include "asterfort/impmem.h"
#include "asterfort/GetDevice.h"
#include "asterfort/nmrini.h"
#include "asterfort/nmrvai.h"
#include "asterfort/nmrtim.h"
#include "asterfort/nmtimr.h"
#include "asterfort/utmess.h"
#include "asterfort/nmstat_mess.h"
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
    character(len=1), intent(in) :: phasis
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(NL_DS_Print), intent(in) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print statistics
!
! --------------------------------------------------------------------------------------------------
!
! In  phasis           : phasis
!                          'P' current step time
!                          'T' on all transient
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_print = .false._1
    real(kind=8) :: time_other = 0.d0
    real(kind=8) :: time_time_step = 0.d0
    real(kind=8) :: time_sub = 0.d0, time = 0.d0
    integer :: i_device, nb_device
    type(NL_DS_Device) :: device
    character(len=16) :: device_type
!
! --------------------------------------------------------------------------------------------------
!
!
! - Print for this step time ?
!
    l_print = ds_print%l_print
!
! - Time for other operations (not measured)
!
    nb_device = ds_measure%nb_device
    if (phasis.eq.'P') then
        call nmtimr(ds_measure, 'Time_Step'  , phasis, time_time_step)
        time_sub  = 0.d0
        do i_device = 1, nb_device
            device      = ds_measure%device(i_device)
            device_type = device%type
            call GetDevice(ds_measure, device_type, device)
            if (device%time_indi_step .ne. 0 .and. device_type.ne.'Time_Step') then
                call nmtimr(ds_measure, device_type, phasis, time)
                time_sub = time_sub + time
            endif
        end do
        time_other = time_time_step - time_sub
        if (time_other .le. 0.d0) then
            time_other = 0.d0
        endif
        call nmrtim(ds_measure, 'Other', time_other)
    endif
!
! - Print at end of current step time
!
    if ((phasis.eq.'P') .and. l_print) then
        call nmstat_mess(ds_measure, phasis)
        call impmem()
    endif
!
! - Print at end of computation
!
    if (phasis .eq. 'T') then
        call nmstat_mess(ds_measure, phasis)
    endif
!
! - Reset times and counters
!
    call nmrini(ds_measure, phasis)
!
end subroutine
