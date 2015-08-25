subroutine PrintTableLine(table, row_sep, unit_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/impfoi.h"
#include "asterfort/impfor.h"
#include "asterfort/impfok.h"
#include "asterfort/utmess.h"
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
    character(len=1), intent(in) :: row_sep
    integer, intent(in) :: unit_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print line of table
!
! --------------------------------------------------------------------------------------------------
!
! In  table            : datastructure for table
! In  row_sep          : separator between rows
! In  unit_print       : logical unit to print
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_row, nb_rows
    type(NL_DS_Row) :: row
    integer :: vali
    integer :: pos, posfin, posmar, unibid
    character(len=16) :: chvide, name
    character(len=24) :: valk 
    real(kind=8) :: valr
    character(len=255) :: table_line
    integer :: longr, precr, longi
    aster_logical :: l_vale_affe, l_vale_real, l_vale_inte, l_vale_strg
    integer :: row_width, line_width
    character(len=1) :: mark
!
! --------------------------------------------------------------------------------------------------
!
    unibid = 0
    chvide = ' '
    pos    = 2
    longr  = 12
    precr  = 5
    longi  = 6
!
! - Get parameters
!
    nb_rows         = table%nb_rows
    line_width      = table%width
    ASSERT(line_width.le.255)
!
! - Prepare line of table - Void rows
!
    call PrepareTableLine(table, row_sep, table_line)
!
! - Set line with values and marks
!
    do i_row = 1, nb_rows
        if (table%l_rows_acti(i_row)) then
            row         = table%rows(i_row)
            row_width   = row%width
            mark        = row%mark
            name        = row%name
            l_vale_affe = row%l_vale_affe
            l_vale_real = row%l_vale_real
            l_vale_inte = row%l_vale_inte
            l_vale_strg = row%l_vale_strg
            posfin      = row_width+pos-1
!
! --------- Set values
!
            if (.not.l_vale_affe) then
                table_line(pos:posfin) = chvide(1:row_width)
            else
                if (l_vale_inte) then
                    vali = row%vale_inte
                    call impfoi(unibid, longi, vali, table_line(pos:posfin))
                else if (l_vale_real) then
                    valr = row%vale_real
                    call impfor(unibid, longr, precr, valr, table_line(pos: posfin))
                else if (l_vale_strg) then
                    valk = row%vale_strg
                    table_line(pos:posfin) = valk(1:row_width)
                else
                    ASSERT(.false.)
                endif
            endif
!
! --------- Set mark
!
            if (mark(1:1) .ne. ' ') then
                posmar = pos + row_width - 2
                table_line(posmar:posmar) = mark(1:1)
            endif
            pos = pos + row_width + 1
        endif
    end do
!
! - Print
!
    call impfok(table_line, line_width, unit_print)
!
end subroutine
