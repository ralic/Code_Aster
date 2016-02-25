subroutine nmtimr(ds_measure, device_type_, phasis, time)
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
    type(NL_DS_Measure), intent(in) :: ds_measure
    character(len=*), intent(in) :: device_type_
    character(len=1), intent(in) :: phasis
    real(kind=8), intent(out) :: time
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistic management
!
! Get time
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_measure       : datastructure for measure and statistics management
! In  device_type      : type of current device
! In  phasis           : phasis (time step, Newton iteration, all computation)
! Out time             : time
!
! --------------------------------------------------------------------------------------------------
!
    type(NL_DS_Device) :: device
    real(kind=8) :: time_iter, time_step, time_comp
!
! --------------------------------------------------------------------------------------------------
!
!
! - Get current device
!
    call GetDevice(ds_measure, device_type_, device)
!
! - Get times
!
    time_iter = device%time_iter
    time_step = device%time_step
    time_comp = device%time_comp
    if (phasis .eq. 'T') then
        time = time_comp
    elseif (phasis .eq. 'P') then
        time = time_step
    elseif (phasis .eq. 'N') then
        time = time_iter
    else
        ASSERT(.false.)
    endif
!
end subroutine
