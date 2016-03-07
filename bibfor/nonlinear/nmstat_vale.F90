subroutine nmstat_vale(ds_measure, time_curr, sderro)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmtimr.h"
#include "asterfort/nmrvai.h"
#include "asterfort/utgtme.h"
#include "asterfort/SetTableColumn.h"
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
    real(kind=8), intent(in) :: time_curr
    character(len=24), intent(in) :: sderro
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistics management
!
! Update statistics in columns
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
! In  time_curr        : current time
! In  sderro           : datastructure for errors during algorithm
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: vmpeak(1)
    integer :: iret
    integer :: nb_cols, nb_device
    integer :: i_col, i_device
    type(NL_DS_Table) :: table
    type(NL_DS_Column) :: column
    type(NL_DS_Device) :: device
    aster_logical :: l_vale_inte, l_vale_real
    integer :: count, i_event, zeven, icode
    character(len=10) :: device_type
    real(kind=8) :: time
    character(len=16) :: col_name, event_name, state
    character(len=24) :: errinf, erreno, erraac
    integer, pointer :: v_errinf(:) => null()
    integer, pointer :: v_erraac(:) => null()
    character(len=16), pointer :: v_erreno(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    table     = ds_measure%table
    nb_cols   = table%nb_cols
    nb_device = ds_measure%nb_device
!
! - State of step
!
    state  = 'CONV'
    errinf = sderro(1:19)//'.INFO'
    erreno = sderro(1:19)//'.ENOM'
    erraac = sderro(1:19)//'.EACT'
    call jeveuo(errinf, 'L', vi = v_errinf)
    zeven  = v_errinf(1)
    call jeveuo(erreno, 'L', vk16 = v_erreno)
    call jeveuo(erraac, 'L', vi = v_erraac)
    do i_event = 1, zeven
        event_name = v_erreno(i_event)
        icode      = v_erraac(i_event)
        if (icode .eq. 1) then
            state = event_name
            exit
        endif
    end do
!
! - Get memory
!
    call utgtme(1, 'VMPEAK  ', vmpeak, iret)
!
! - Set list of values in columns
!
    do i_col = 1, nb_cols
        column   = table%cols(i_col)
        i_device = table%indx_vale(i_col)
        col_name = column%name
        if (i_device .eq. 0) then
            if (col_name .eq. 'INST') then
                call SetTableColumn(table, 'INST',  flag_affe_ = .true._1, valer_ = time_curr)
            elseif (col_name .eq. 'State') then
                call SetTableColumn(table, 'State',  flag_affe_ = .true._1, valek_ = state)
            elseif (col_name .eq. 'Memory') then
                call SetTableColumn(table, 'Memory',  flag_affe_ = .true._1,&
                                    valei_ = nint(vmpeak(1)))
            endif
        else
            device      = ds_measure%device(i_device)
            device_type = device%type
            l_vale_inte = column%l_vale_inte
            l_vale_real = column%l_vale_real
            if (l_vale_real) then
                call nmtimr(ds_measure, device_type, 'P', time)
                ASSERT(col_name(1:5) .eq. 'Time_')
                call SetTableColumn(table, col_name,  flag_affe_ = .true._1, valer_ = time)
            endif
            if (l_vale_inte) then
                call nmrvai(ds_measure, device_type, 'P', output_count = count)
                ASSERT(col_name(1:6) .eq. 'Count_')
                call SetTableColumn(table, col_name,  flag_affe_ = .true._1, valei_ = count)
            endif
        endif
    end do
!
    ds_measure%table = table
!
end subroutine
