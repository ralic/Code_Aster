subroutine ComputeTableWidth(table, width)
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
    integer, intent(out) :: width
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Compute width of table
!
! --------------------------------------------------------------------------------------------------
!
! In  table            : datastructure for table
! Out width            : width of table
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_col, nb_cols, nb_cols_active
!
! --------------------------------------------------------------------------------------------------
!
    nb_cols    = table%nb_cols
    width      = 0
!
! - Number of active columns
!
    nb_cols_active = 0
    do i_col = 1, nb_cols
        if (table%l_cols_acti(i_col)) then
            nb_cols_active = nb_cols_active + 1
        endif
    end do
    if (nb_cols_active .ge. 15) then
        call utmess('F', 'IMPRESSION_1', si=nb_cols_active)
    endif
!
! - Compute width
!
    width = 1
    do i_col = 1, nb_cols
        if (table%l_cols_acti(i_col)) then
            width = width + (16+1)
        endif
    end do
!
! - Save value
!
    if (width .gt. 255) then
        call utmess('F', 'IMPRESSION_2', si=width)
    endif
!
end subroutine
