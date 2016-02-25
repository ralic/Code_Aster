subroutine nmimpr(ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/PrintTableLine.h"
#include "asterfort/iunifi.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Print), intent(in) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print line in convergence table
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_csv
    integer :: unit_mess, unit_csv
    character(len=1) :: row_sep
    type(NL_DS_Table) :: table_cvg
!
! --------------------------------------------------------------------------------------------------
!
    unit_mess = iunifi('MESSAGE')
!
! - Get convergence table
!
    table_cvg = ds_print%table_cvg
!
! - Get parameters
!
    l_csv     = ds_print%l_tcvg_csv
    unit_csv  = ds_print%tcvg_unit
!
! - Print in message unit
!
    row_sep = '|'
    if (ds_print%l_print) then
        call PrintTableLine(table_cvg, row_sep, unit_mess)
    endif
!
! - Print in file
!
    if (l_csv) then
        row_sep = ','
        call PrintTableLine(table_cvg, row_sep, unit_csv)
    endif
!
end subroutine
