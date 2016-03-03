subroutine ComputeTableWidth(table, line_width, nb_cols_active)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
    integer, intent(out) :: line_width
    integer, intent(out) :: nb_cols_active
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Table management
!
! Compute line of table
!
! --------------------------------------------------------------------------------------------------
!
! In  table            : datastructure for table
! Out line_width       : width of line
! Out nb_cols_active   : number of active columns
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_col, nb_cols
!
! --------------------------------------------------------------------------------------------------
!
    nb_cols        = table%nb_cols
    line_width     = 0
    nb_cols_active = 0
!
! - Number of active columns
!
    do i_col = 1, nb_cols
        if (table%l_cols_acti(i_col)) then
            nb_cols_active = nb_cols_active + 1
        endif
    end do
!
! - Compute width
!
    line_width = 1
    do i_col = 1, nb_cols
        if (table%l_cols_acti(i_col)) then
            line_width = line_width + (16+1)
        endif
    end do
    ASSERT(line_width .le. 512)
!
end subroutine
