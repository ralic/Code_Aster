subroutine nmcrti(list_func_acti, result, ds_contact, ds_measure)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/ulopen.h"
#include "asterfort/uttcpu.h"
#include "asterfort/impfok.h"
#include "asterfort/ActivateDevice.h"
#include "asterfort/CreateTable.h"
#include "asterfort/SetTablePara.h"
#include "asterfort/SetTableColumn.h"
#include "asterfort/ComputeTableHead.h"
#include "asterfort/ComputeTableWidth.h"
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
    integer, intent(in) :: list_func_acti(*)
    character(len=8), intent(in) :: result
    type(NL_DS_Contact), intent(in) :: ds_contact
    type(NL_DS_Measure), intent(inout) :: ds_measure
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistics management
!
! Initializations for measure and statistics management
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  result           : name of results datastructure
! In  ds_contact       : datastructure for contact management
! IO  ds_measure       : datastructure for measure and statistics management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_timer, nb_timer, i_device, i_col, line_width, nb_cols_active
    character(len=24) :: cpu_name
    character(len=512) :: table_head(3)
    aster_logical :: l_line_search
    aster_logical :: l_cont, l_fric, l_cont_disc, l_cont_cont
    aster_logical :: l_loop_cont, l_loop_fric, l_loop_geom, l_newt_geom
    aster_logical :: l_all_verif, l_device_acti
    type(NL_DS_Table) :: table
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... Initializations for measure and statistics management'
    endif
!
! - Get active functionnalities
!
    l_all_verif   = .false.
    l_fric        = .false.
    l_line_search = isfonc(list_func_acti,'RECH_LINE')
    l_cont        = isfonc(list_func_acti, 'CONTACT' )
    l_cont_disc   = isfonc(list_func_acti, 'CONT_DISCRET')
    l_cont_cont   = isfonc(list_func_acti, 'CONT_CONTINU')
    l_loop_cont   = isfonc(list_func_acti, 'BOUCLE_EXT_CONT')
    l_loop_fric   = isfonc(list_func_acti, 'BOUCLE_EXT_FROT')
    l_loop_geom   = isfonc(list_func_acti, 'BOUCLE_EXT_GEOM')
    l_newt_geom   = isfonc(list_func_acti, 'GEOM_NEWTON')
    if (l_cont) then
        l_all_verif = cfdisl(ds_contact%sdcont_defi, 'ALL_VERIF')    
        l_fric      = cfdisl(ds_contact%sdcont_defi, 'FROTTEMENT')
    endif
!
! - Activate devices (standard)
!
    call ActivateDevice(ds_measure, 'Compute')
    call ActivateDevice(ds_measure, 'Time_Step')
    call ActivateDevice(ds_measure, 'Newt_Iter')
    call ActivateDevice(ds_measure, 'Integrate')
    call ActivateDevice(ds_measure, 'Matr_Asse')
    call ActivateDevice(ds_measure, 'Factor')
    call ActivateDevice(ds_measure, '2nd_Member')
    call ActivateDevice(ds_measure, 'Solve')
    call ActivateDevice(ds_measure, 'Store')
    call ActivateDevice(ds_measure, 'Post')
    call ActivateDevice(ds_measure, 'Lost_Time')
    call ActivateDevice(ds_measure, 'Other')
    if (l_line_search) then
        call ActivateDevice(ds_measure, 'LineSearch')
    endif
!
! - Activate devices for contact (10)
!
    if (l_cont .and. (.not.l_all_verif)) then
        call ActivateDevice(ds_measure, 'Cont_NCont')
        if (l_fric) then
            call ActivateDevice(ds_measure, 'Cont_NFric')
        endif
        if (l_loop_geom .or. l_newt_geom) then
            call ActivateDevice(ds_measure, 'Cont_Geom')
        endif
        if (l_cont_disc) then
            call ActivateDevice(ds_measure, 'Cont_Algo')
        endif
        if (l_cont_cont) then
            call ActivateDevice(ds_measure, 'Cont_Prep')
            call ActivateDevice(ds_measure, 'Cont_Elem')
            call ActivateDevice(ds_measure, 'Cont_Cycl1')
            call ActivateDevice(ds_measure, 'Cont_Cycl2')
            call ActivateDevice(ds_measure, 'Cont_Cycl3')
            call ActivateDevice(ds_measure, 'Cont_Cycl4')
        endif
    endif
!
! - Reset all timers
!
    nb_timer  = ds_measure%nb_timer
    do i_timer = 1, nb_timer
        cpu_name  = ds_measure%timer(i_timer)%cpu_name
        call uttcpu(cpu_name, 'INIT', ' ')
        ds_measure%timer(i_timer)%time_init = 0.d0
    end do
!
! - Create table
!
    if (ds_measure%l_table .or. ds_measure%table%l_csv) then
!
! ----- Get table
!
        table = ds_measure%table
!
! ----- First column: time
!
        i_col = 1
        table%l_cols_acti(i_col) = .true._1
        ASSERT(table%cols(i_col)%name(1:4) .eq. 'INST')
!
! ----- Loop on active devices to activate columns
!
        do i_device = 1, ds_measure%nb_device
            l_device_acti = ds_measure%l_device_acti(i_device)
            if (l_device_acti) then
                i_col = ds_measure%indx_cols(2*(i_device-1)+1)
                if (i_col .ne. 0) then
                    table%l_cols_acti(i_col) = .true._1
                    ASSERT(table%cols(i_col)%name(1:5) .eq. 'Time_')
                endif
                i_col = ds_measure%indx_cols(2*(i_device-1)+2)
                if (i_col .ne. 0) then
                    table%l_cols_acti(i_col) = .true._1
                    ASSERT(table%cols(i_col)%name(1:6) .eq. 'Count_')
                endif
            endif   
        end do
!
! ----- Activate state and memory
!
        if (table%l_csv) then
            call SetTableColumn(table, 'State' , flag_acti_ = .true._1)
            call SetTableColumn(table, 'Memory' , flag_acti_ = .true._1)
        endif
!
! ----- Set table
!
        ds_measure%table  = table
!
! ----- Create list of parameters
!
        call SetTablePara(table)
!
! ----- Create table in results datastructure
!
        call CreateTable(result, table)
!
! ----- Prepare table in output CSV file
!
        call ComputeTableWidth(table, line_width, nb_cols_active)
        table%width        = line_width
!
! ----- Print table head in output CSV file
!
        if (table%l_csv) then
            call ulopen(table%unit_csv, ' ', ' ', 'NEW', 'O')
            call ComputeTableHead(table, ',', table_head)
            call impfok(table_head(1), table%width, table%unit_csv)
            call impfok(table_head(2), table%width, table%unit_csv)
        endif
!
! ----- Set table
!
        ds_measure%table  = table
    endif
!
end subroutine
