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
    integer :: i_row, i_dof_monitor, nb_dof_monitor, nb_rows, i_row_name
    type(NL_DS_Table) :: table_cvg
    type(NL_DS_Row) :: row
    character(len=9) :: row_name
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
    nb_rows                = table_cvg%nb_rows
!
! - Get number of rows for DOF monitoring
!
    sdsuiv_info = sdsuiv(1:14)//'     .INFO'
    call jeveuo(sdsuiv_info, 'L', vi = v_sdsuiv_info)
    nb_dof_monitor = v_sdsuiv_info(2)
    if (nb_dof_monitor .gt. 9) then
        call utmess('F', 'IMPRESSION_3', si=nb_dof_monitor)
    endif
!
! - Title of rows for DOF monitoring
!
    if (nb_dof_monitor .ne. 0) then
        sdsuiv_titr = sdsuiv(1:14)//'     .TITR'
        call jeveuo(sdsuiv_titr, 'L', vk16 = v_sdsuiv_titr)
    endif
!
! - Set list of rows for DOF monitor in convergence table
!
    do i_dof_monitor = 1, nb_dof_monitor    
!
! ----- Name of the row
!
        call impfoi(0, 1, i_dof_monitor, indsui)
        row_name        = 'SUIVDDL'//indsui
!
! ----- Look for row index
!
        i_row_name = 0
        do i_row = 1, nb_rows
            if (table_cvg%rows(i_row)%name .eq. row_name) then
                ASSERT(i_row_name.eq.0)
                i_row_name = i_row
            endif
        end do
        ASSERT(i_row_name.ne.0)
!
! ----- Set row
!
        row = table_cvg%rows(i_row_name)
        row%l_vale_real = .true._1
        row%title(1)    = v_sdsuiv_titr(3*(i_dof_monitor-1)+1)
        row%title(2)    = v_sdsuiv_titr(3*(i_dof_monitor-1)+2)
        row%title(3)    = v_sdsuiv_titr(3*(i_dof_monitor-1)+3)
        table_cvg%rows(i_row_name)  = row
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
