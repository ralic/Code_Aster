subroutine InitTableCvg(list_func_acti, sdsuiv, ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/impfoi.h"
#include "asterfort/SetTableColumn.h"
#include "asterfort/utmess.h"
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
    character(len=24), intent(in) :: sdsuiv
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Initializations for convergence table
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  sdsuiv           : datastructure for DOF monitoring
! IO  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_col, i_dof_monitor, nb_dof_monitor, line_width, i, nb_cols_active
    type(NL_DS_Table) :: table_cvg
    aster_logical :: l_line_search, l_pilo, l_cont_disc, l_cont_cont
    aster_logical :: l_deborst, l_refe_rela, l_comp_rela
    aster_logical :: l_loop_cont, l_loop_frot, l_loop_geom, l_cont_all_verif
    aster_logical :: l_newt_frot, l_newt_cont, l_newt_geom
    aster_logical :: l_info_resi, l_info_time, l_csv
    character(len=1) :: indsui
    character(len=24) :: col_name
    character(len=512) :: sep_line
    character(len=24) :: sdsuiv_info
    integer, pointer :: v_sdsuiv_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Initializations of convergence table'
    endif
    sep_line = ' '
!
! - Get convergence table
!
    table_cvg   = ds_print%table_cvg
!
! - Get parameters
!
    l_info_resi = ds_print%l_info_resi
    l_info_time = ds_print%l_info_time
    l_csv       = ds_print%l_tcvg_csv
!
! - Active functionnalities
!
    l_line_search    = isfonc(list_func_acti,'RECH_LINE')
    l_pilo           = isfonc(list_func_acti,'PILOTAGE')
    l_cont_disc      = isfonc(list_func_acti,'CONT_DISCRET')
    l_cont_cont      = isfonc(list_func_acti,'CONT_CONTINU')
    l_refe_rela      = isfonc(list_func_acti,'RESI_REFE')
    l_deborst        = isfonc(list_func_acti,'DEBORST')
    l_loop_frot      = isfonc(list_func_acti,'BOUCLE_EXT_FROT')
    l_loop_geom      = isfonc(list_func_acti,'BOUCLE_EXT_GEOM')
    l_loop_cont      = isfonc(list_func_acti,'BOUCLE_EXT_CONT')
    l_newt_frot      = isfonc(list_func_acti,'FROT_NEWTON')
    l_newt_cont      = isfonc(list_func_acti,'CONT_NEWTON')
    l_newt_geom      = isfonc(list_func_acti,'GEOM_NEWTON')
    l_comp_rela      = isfonc(list_func_acti,'RESI_COMP')
    l_cont_all_verif = isfonc(list_func_acti,'CONT_ALL_VERIF')
!
! - No cols activated
!
    call SetTableColumn(table_cvg, flag_acti_ = .false._1)
!
! - Time step
!
    if (l_csv) then
        call SetTableColumn(table_cvg, name_ = 'INCR_INST', flag_acti_ = .true._1)
    endif
!
! - Newton iterations
!
    call SetTableColumn(table_cvg, name_ = 'ITER_NUME', flag_acti_ = .true._1)
    if (l_info_time) then
        call SetTableColumn(table_cvg, name_ = 'ITER_TIME', flag_acti_ = .true._1)
    endif
!
! - RESI_GLOB_RELA
!
    call SetTableColumn(table_cvg, name_ = 'RESI_RELA', flag_acti_ = .true._1)
    if (l_info_resi) then
        call SetTableColumn(table_cvg, name_ = 'RELA_NOEU', flag_acti_ = .true._1)
    endif
!
! - RESI_GLOB_MAXI
!
    call SetTableColumn(table_cvg, name_ = 'RESI_MAXI', flag_acti_ = .true._1)
    if (l_info_resi) then
        call SetTableColumn(table_cvg, name_ = 'MAXI_NOEU', flag_acti_ = .true._1)
    endif
!
! - RESI_REFE_RELA
!
    if (l_refe_rela) then
        call SetTableColumn(table_cvg, name_ = 'RESI_REFE', flag_acti_ = .true._1)
        if (l_info_resi) then
            call SetTableColumn(table_cvg, name_ = 'REFE_NOEU', flag_acti_ = .true._1)
        endif
    endif
!
! - RESI_COMP_RELA
!
    if (l_comp_rela) then
        call SetTableColumn(table_cvg, name_ = 'RESI_COMP', flag_acti_ = .true._1)
        if (l_info_resi) then
            call SetTableColumn(table_cvg, name_ = 'COMP_NOEU', flag_acti_ = .true._1)
        endif
    endif
!
! - Contact
!
    if (l_loop_geom) then
        call SetTableColumn(table_cvg, name_ = 'BOUC_GEOM', flag_acti_ = .true._1)
    endif
    if (l_loop_frot) then
        call SetTableColumn(table_cvg, name_ = 'BOUC_FROT', flag_acti_ = .true._1)
    endif
    if (l_loop_cont) then
        call SetTableColumn(table_cvg, name_ = 'BOUC_CONT', flag_acti_ = .true._1)
    endif
    if (l_cont_disc .and. (.not.l_cont_all_verif)) then
        call SetTableColumn(table_cvg, name_ = 'CTCD_NBIT', flag_acti_ = .true._1)
    endif
    if (l_cont_cont .and. (.not.l_cont_all_verif)) then
        call SetTableColumn(table_cvg, name_ = 'CTCC_CYCL', flag_acti_ = .true._1)
    endif
!
! - Contact (generalized Newton)
!
    if (l_newt_geom) then
        call SetTableColumn(table_cvg, name_ = 'GEOM_NEWT', flag_acti_ = .true._1)
        if (l_info_resi) then
            call SetTableColumn(table_cvg, name_ = 'GEOM_NOEU', flag_acti_ = .true._1)
        endif
    endif
    if (l_newt_frot) then
        call SetTableColumn(table_cvg, name_ = 'FROT_NEWT', flag_acti_ = .true._1)
        if (l_info_resi) then
            call SetTableColumn(table_cvg, name_ = 'FROT_NOEU', flag_acti_ = .true._1)
        endif
    endif
    if (l_newt_cont) then
        call SetTableColumn(table_cvg, name_ = 'CONT_NEWT', flag_acti_ = .true._1)
    endif
!
! - Contact (fixed points)
!
    if (l_loop_geom .or. l_loop_frot .or. l_loop_cont) then
        call SetTableColumn(table_cvg, name_ = 'BOUC_VALE', flag_acti_ = .true._1)
        if (l_info_resi) then
            call SetTableColumn(table_cvg, name_ = 'BOUC_NOEU', flag_acti_ = .true._1)
        endif
    endif
!
! - DE BORST method (stress planes)
!
    if (l_deborst) then
        call SetTableColumn(table_cvg, name_ = 'DEBORST  ', flag_acti_ = .true._1)
    endif
!
! - Line search
!
    if (l_line_search) then
        call SetTableColumn(table_cvg, name_ = 'RELI_NBIT', flag_acti_ = .true._1)
        call SetTableColumn(table_cvg, name_ = 'RELI_COEF', flag_acti_ = .true._1)
    endif
!
! - Pilotage (continuation methods)
!
    if (l_pilo) then
        call SetTableColumn(table_cvg, name_ = 'PILO_COEF', flag_acti_ = .true._1)
    endif
!
! - Matrix option
!
    call SetTableColumn(table_cvg, name_ = 'MATR_ASSE', flag_acti_ = .true._1)
!
! - For DOF monitoring
!
    sdsuiv_info = sdsuiv(1:14)//'     .INFO'
    call jeveuo(sdsuiv_info, 'L', vi = v_sdsuiv_info)
    nb_dof_monitor = v_sdsuiv_info(2)
    do i_dof_monitor = 1, nb_dof_monitor
        call impfoi(0, 1, i_dof_monitor, indsui)
        col_name        = 'SUIVDDL'//indsui
        call SetTableColumn(table_cvg, name_ = col_name, flag_acti_ = .true._1)
    end do
!
! - Compute width of table
!
    call ComputeTableWidth(table_cvg, line_width, nb_cols_active)
    if (line_width .gt. 255) then
        call utmess('F', 'IMPRESSION_2', si = line_width)
    endif
    if (nb_cols_active .ge. 15) then
        call utmess('F', 'IMPRESSION_1', si = nb_cols_active)
    endif
!
! - Compute separator line
!
    do i = 1, line_width
        sep_line(i:i) = '-'
    end do
    table_cvg%width    = line_width
    table_cvg%sep_line = sep_line
    ds_print%sep_line  = sep_line
!
! - No value affected in column
!
    do i_col = 1, table_cvg%nb_cols
        table_cvg%cols(i_col)%l_vale_affe = .false._1
    end do
!
! - Set convergence table
!
    ds_print%table_cvg = table_cvg
!
end subroutine
