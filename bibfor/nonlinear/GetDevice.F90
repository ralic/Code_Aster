subroutine GetDevice(ds_measure, device_type_, device, device_indx_)
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
    type(NL_DS_Measure), intent(in) :: ds_measure
    character(len=*), intent(in) :: device_type_
    type(NL_DS_Device), intent(out) :: device
    integer, optional, intent(out) :: device_indx_
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistic management
!
! Get device
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_measure       : datastructure for measure and statistics management
! In  device_type      : type of current device
! Out device           : current device
! Out device_indx      : index of current device
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: device_type
    integer :: i_device, device_indx, nb_device
!
! --------------------------------------------------------------------------------------------------
!
    device_type    = device_type_
!
! - Find device
!
    device_type = device_type_
    nb_device   = ds_measure%nb_device
    device_indx = 0
    do i_device = 1, nb_device
        if (ds_measure%device(i_device)%type .eq. device_type) then
            ASSERT(device_indx.eq.0)
            device_indx = i_device
        endif
    end do
!
! - Get current device
!
    ASSERT(device_indx.ne.0)
    device = ds_measure%device(device_indx)
    if (present(device_indx_)) then
        device_indx_ = device_indx
    endif
!
end subroutine
