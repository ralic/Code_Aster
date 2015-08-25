subroutine nmimr0(ds_print, loop_name)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/SetRow.h"
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
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=4), intent(in) :: loop_name
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Set values are not affected on rows for a loop level
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_print         : datastructure for printing parameters
! In  loop_name        : name of loop
!                         'RESI' - Loop on residuals
!                         'NEWT' - Newton loop
!                         'FIXE' - Fixed points loop
!                         'INST' - Step time loop
!                         'CALC' - Computation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_row, nb_rows
    character(len=9) :: row_name
    type(NL_DS_Table) :: table_cvg
!
! --------------------------------------------------------------------------------------------------
!
    table_cvg = ds_print%table_cvg
    nb_rows   = table_cvg%nb_rows
!
! - No value affected in row for loop level
!
    do i_row = 1, nb_rows
        if (table_cvg%l_rows_acti(i_row)) then
            row_name = table_cvg%rows(i_row)%name
            if (loop_name .eq. row_name(1:4)) then
                call SetRow(table_cvg, name_ = row_name, flag_affe_ = .false._1)
            endif
        endif
    end do
!
! - Set convergence table
!
    ds_print%table_cvg = table_cvg
!
end subroutine
