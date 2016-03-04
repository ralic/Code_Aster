subroutine nmstat_table(ds_measure, time_curr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/nmtimr.h"
#include "asterfort/nmrvai.h"
#include "asterfort/tbajli.h"
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
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistics management
!
! Update statistics
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_measure       : datastructure for measure and statistics management
! In  time_curr        : current time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cols, nb_device
    integer :: i_col, i_para_real, i_para_inte, i_device
    integer :: vali(40)
    character(len=8) :: k8bid
    complex(kind=8), parameter :: c16bid =(0.d0,0.d0)
    real(kind=8) :: valr(40)
    type(NL_DS_Table) :: table
    type(NL_DS_Column) :: column
    type(NL_DS_Device) :: device
    aster_logical :: l_acti, l_vale_inte, l_vale_real
    integer :: count
    character(len=10) :: device_type
    real(kind=8) :: time
!
! --------------------------------------------------------------------------------------------------
!
    i_para_real = 0
    i_para_inte = 0
!
! - Get parameters
!
    table     = ds_measure%table
    nb_cols   = table%nb_cols
    nb_device = ds_measure%nb_device
!
! - First value
!
    i_para_real       = i_para_real + 1
    valr(i_para_real) = time_curr
    ASSERT(table%cols(1)%name(1:4) .eq. 'INST')
!
! - Set list of values
!
    do i_col = 1, nb_cols
        column   = table%cols(i_col)
        l_acti   = table%l_cols_acti(i_col)
        if (l_acti) then
            i_device    = table%indx_vale(i_col)
            device      = ds_measure%device(i_device)
            device_type = device%type
            l_vale_inte = column%l_vale_inte
            l_vale_real = column%l_vale_real
            if (l_vale_real) then
                if (column%name .eq. 'INST') then  
                    time = time_curr
                else
                    call nmtimr(ds_measure, device_type, 'P', time)
                    ASSERT(table%cols(i_col)%name(1:5) .eq. 'Time_')
                endif
                i_para_real       = i_para_real + 1
                valr(i_para_real) = time
            endif
            if (l_vale_inte) then
                call nmrvai(ds_measure, device_type, 'P', output_count = count)
                i_para_inte       = i_para_inte + 1
                vali(i_para_inte) = count
                ASSERT(table%cols(i_col)%name(1:6) .eq. 'Count_')
            endif
        endif
    end do
!
! - Add line in table
!
    call tbajli(table%table_name, table%nb_para, table%list_para,&
                vali, valr, [c16bid], k8bid, 0)
!
end subroutine
