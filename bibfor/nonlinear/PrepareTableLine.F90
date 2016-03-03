subroutine PrepareTableLine(table, col_sep, table_line)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/impfok.h"
#include "asterfort/utmess.h"
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
    type(NL_DS_Table), intent(in) :: table
    character(len=1), intent(in) :: col_sep
    character(len=512), intent(out) :: table_line
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Table management
!
! Prepare line of table with empty cols
!
! --------------------------------------------------------------------------------------------------
!
! In  table            : datastructure for table
! In  col_sep          : separator between columns
! Out table_line       : line of the table
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_col, nb_cols, width, total_width, line_width
!
! --------------------------------------------------------------------------------------------------
!
    table_line = ' '
!
! - Get parameters
!
    nb_cols         = table%nb_cols
    line_width      = table%width
    ASSERT(line_width .le. 512)
    ASSERT(nb_cols .le. table%nb_cols_maxi)
!
! - Prepare line
!
    table_line(1:1) = col_sep
    total_width     = 1
    do i_col = 1, nb_cols
        if (table%l_cols_acti(i_col)) then
            width       = 16
            total_width = total_width + width + 1
            ASSERT(total_width + width + 1 .le. 512)
            table_line(total_width:total_width) = col_sep
        endif
    end do
!
    ASSERT(total_width.eq.line_width)
!
end subroutine
