subroutine nmimen(ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/impfok.h"
#include "asterfort/iunifi.h"
#include "asterfort/ComputeTableHead.h"
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
    type(NL_DS_Print), intent(in) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print head of convergence table
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    character(len=255) :: sep_line, table_head(3)
    type(NL_DS_Table) :: table_cvg
    aster_logical :: l_tcvg_csv
    integer :: line_width, mesg_unit, tcvg_unit
!
! --------------------------------------------------------------------------------------------------
!
    mesg_unit       = iunifi('MESSAGE')
!
! - Get convergence table
!
    table_cvg       = ds_print%table_cvg
!
! - Get parameters
!
    sep_line        = ds_print%table_cvg%sep_line
    tcvg_unit       = ds_print%tcvg_unit
    line_width      = ds_print%table_cvg%width
    l_tcvg_csv      = ds_print%l_tcvg_csv
!
! - Compute table head
!
    call ComputeTableHead(table_cvg, '|', table_head)
!
! - Print in message unit
!
    call impfok(sep_line, line_width, mesg_unit)
    call impfok(table_head(1), line_width, mesg_unit)
    call impfok(table_head(2), line_width, mesg_unit)
    call impfok(table_head(3), line_width, mesg_unit)
    call impfok(sep_line, line_width, mesg_unit)
!
! - Print in file
!
    if (l_tcvg_csv) then
        call ComputeTableHead(table_cvg, ',', table_head)
        call impfok(table_head(1), line_width, tcvg_unit)
        call impfok(table_head(2), line_width, tcvg_unit)
        call impfok(table_head(3), line_width, tcvg_unit)
    endif
!
end subroutine
