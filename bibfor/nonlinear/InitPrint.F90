subroutine InitPrint(sdsuiv, ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/impfoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sdsuiv
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Initializations for printing
!
! --------------------------------------------------------------------------------------------------
!
! In  sdsuiv           : datastructure for DOF monitoring
! IO  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_col, i_dof_monitor, nb_dof_monitor, nb_cols, i_col_name
    type(NL_DS_Table) :: table_cvg
    type(NL_DS_col) :: col
    character(len=9) :: col_name
    character(len=1) :: indsui
    character(len=24) :: sdsuiv_info
    integer, pointer :: v_sdsuiv_info(:) => null()
    character(len=24) :: sdsuiv_titr
    character(len=16), pointer :: v_sdsuiv_titr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... Initializations for printing'
    endif
!
! - Get convergence table
!
    table_cvg = ds_print%table_cvg
!
! - Set convergence table
!
    table_cvg%title_height = 3
    table_cvg%l_csv        = ds_print%l_tcvg_csv
    table_cvg%unit_csv     = ds_print%tcvg_unit
    nb_cols                = table_cvg%nb_cols
!
! - Get number of columns for DOF monitoring
!
    sdsuiv_info = sdsuiv(1:14)//'     .INFO'
    call jeveuo(sdsuiv_info, 'L', vi = v_sdsuiv_info)
    nb_dof_monitor = v_sdsuiv_info(2)
    if (nb_dof_monitor .gt. 9) then
        call utmess('F', 'IMPRESSION_3', si=nb_dof_monitor)
    endif
!
! - Title of columns for DOF monitoring
!
    if (nb_dof_monitor .ne. 0) then
        sdsuiv_titr = sdsuiv(1:14)//'     .TITR'
        call jeveuo(sdsuiv_titr, 'L', vk16 = v_sdsuiv_titr)
    endif
!
! - Set list of columns for DOF monitor in convergence table
!
    do i_dof_monitor = 1, nb_dof_monitor    
!
! ----- Name of the column
!
        call impfoi(0, 1, i_dof_monitor, indsui)
        col_name        = 'SUIVDDL'//indsui
!
! ----- Look for column index
!
        i_col_name = 0
        do i_col = 1, nb_cols
            if (table_cvg%cols(i_col)%name .eq. col_name) then
                ASSERT(i_col_name.eq.0)
                i_col_name = i_col
            endif
        end do
        ASSERT(i_col_name.ne.0)
!
! ----- Set column
!
        col = table_cvg%cols(i_col_name)
        col%l_vale_real = .true._1
        col%title(1)    = v_sdsuiv_titr(3*(i_dof_monitor-1)+1)
        col%title(2)    = v_sdsuiv_titr(3*(i_dof_monitor-1)+2)
        col%title(3)    = v_sdsuiv_titr(3*(i_dof_monitor-1)+3)
        table_cvg%cols(i_col_name)  = col
    end do
!
! - Prepare file output
!
    if (ds_print%l_tcvg_csv) then
        call ulopen(ds_print%tcvg_unit, ' ', ' ', 'NEW', 'O')
    endif
!
! - Print every step time (default)
!
    ds_print%l_print = .true._1
!
! - Set convergence table
!
    ds_print%table_cvg = table_cvg
!
end subroutine
