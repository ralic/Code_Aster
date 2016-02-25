subroutine nmstat_mess(ds_measure, phasis)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/impfot.h"
#include "asterfort/utmess.h"
#include "asterfort/nmtimr.h"
#include "asterfort/nmrvai.h"
#include "asterfort/nmimpr_mess.h"
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
    character(len=1), intent(in) :: phasis
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print statistics in message unit
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
! In  phasis           : phasis
!                          'P' current step time
!                          'T' on all transient
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_device, i_device
    type(NL_DS_Device) :: device
    aster_logical :: l_time, l_count, l_acti
    integer :: time_mesg, count_mesg, count
    character(len=16) :: device_type
    real(kind=8) :: time
!
! --------------------------------------------------------------------------------------------------
!
    nb_device = ds_measure%nb_device
    do i_device = 1, nb_device
        device = ds_measure%device(i_device)
        l_acti = ds_measure%l_device_acti(i_device)
        if (l_acti) then
            if (phasis.eq.'P') then
                time_mesg   = device%time_indi_step
                count_mesg  = device%count_indi_step
            elseif (phasis.eq.'T') then
                time_mesg   = device%time_indi_comp
                count_mesg  = device%count_indi_comp
            else
                ASSERT(.false.)
            endif
            l_time      = time_mesg .ne. 0
            l_count     = count_mesg .ne. 0
            device_type = device%type
            if (l_time) then
                call nmtimr(ds_measure, device_type, phasis, time)
            endif
            if (l_count) then
                call nmrvai(ds_measure, device_type, phasis, output_count = count)
            endif
            if (l_time.and.l_count) then
                ASSERT(time_mesg .eq. count_mesg)
                call nmimpr_mess(time_mesg, vali_ = count, valr_ = time)
            endif
            if (l_time.and..not.l_count) then
                call nmimpr_mess(time_mesg, valr_ = time)
            endif
            if (l_count.and..not.l_time) then
                call nmimpr_mess(count_mesg, vali_ = count)
            endif
        endif
    end do
!
end subroutine
