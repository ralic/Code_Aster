subroutine ComputeTableHead(table, col_sep, table_head)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/PrepareTableLine.h"
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
    type(NL_DS_Table), intent(in) :: table
    character(len=1), intent(in) :: col_sep
    character(len=255), intent(out) :: table_head(3)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Compute head of table
!
! --------------------------------------------------------------------------------------------------
!
! In  table            : datastructure for table
! In  col_sep          : separator between colums
! Out table_head       : head of table
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_col, nb_cols, pos
    integer :: col_width, title_height, table_width
    character(len=16) :: col_title(3)
!
! --------------------------------------------------------------------------------------------------
!
    table_head(1:3) = ' '
!
! - Get parameters
!
    nb_cols      = table%nb_cols
    title_height = table%title_height
    table_width  = table%width
    ASSERT((title_height.gt.0).and.(title_height.le.3))
    ASSERT(table_width.le.255)
    ASSERT(nb_cols.le.table%nb_cols_maxi)
!
! - Prepare heads of table with empty cols
!
    call PrepareTableLine(table, col_sep, table_head(1))
    if (title_height .ge. 2) then
        call PrepareTableLine(table, col_sep, table_head(2))
    endif
    if (title_height .eq. 3) then
        call PrepareTableLine(table, col_sep, table_head(3))
    endif
!
! - Set title of columns in heads of table
!
    pos = 2
    do i_col = 1, nb_cols
        if (table%l_cols_acti(i_col)) then
            col_width    = table%cols(i_col)%width
            col_title(1) = table%cols(i_col)%title(1)
            if (title_height .ge. 2) then
                col_title(2) = table%cols(i_col)%title(2)
            endif
            if (title_height .eq. 3) then
                col_title(3) = table%cols(i_col)%title(3)
            endif
            table_head(1)(pos:pos+col_width-1) = col_title(1)
            if (title_height .ge. 2) then
                table_head(2)(pos:pos+col_width-1) = col_title(2)
            endif
            if (title_height .eq. 3) then
                table_head(3)(pos:pos+col_width-1) = col_title(3)
            endif
            pos = pos+col_width+1
        endif
    end do
!
    ASSERT(pos.eq.table_width+1)
!
end subroutine
!
