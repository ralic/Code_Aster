subroutine nmrini(ds_measure, phasis)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=1), intent(in) :: phasis
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistic management
!
! Reset times and counters
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
! In  phasis           : phasis (time step, Newton iteration, all computation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_device, nb_device 
!
! --------------------------------------------------------------------------------------------------
!
    nb_device  = ds_measure%nb_device
!
! - Reset all timers
!
    do i_device = 1, nb_device  
        if (phasis .eq. 'T') then
            ds_measure%device(i_device)%time_comp = 0.d0
            ds_measure%device(i_device)%time_step = 0.d0
            ds_measure%device(i_device)%time_iter = 0.d0
        elseif (phasis .eq. 'P') then
            ds_measure%device(i_device)%time_step = 0.d0
            ds_measure%device(i_device)%time_iter = 0.d0
        elseif (phasis .eq. 'N') then
            ds_measure%device(i_device)%time_iter = 0.d0
        else
            ASSERT(.false.)
        endif
    end do
!
! - Reset all counters
!
    do i_device = 1, nb_device  
        if (phasis .eq. 'T') then
            ds_measure%device(i_device)%count_comp = 0
            ds_measure%device(i_device)%count_step = 0
            ds_measure%device(i_device)%count_iter = 0
        elseif (phasis .eq. 'P') then
            ds_measure%device(i_device)%count_step = 0
            ds_measure%device(i_device)%count_iter = 0
        elseif (phasis .eq. 'N') then
            ds_measure%device(i_device)%count_iter = 0
        else
            ASSERT(.false.)
        endif
    end do
!
end subroutine
