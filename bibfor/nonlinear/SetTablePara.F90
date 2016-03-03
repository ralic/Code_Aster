subroutine SetTablePara(table)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
    type(NL_DS_Table), intent(inout) :: table
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Table management
!
! Prepare list of parameters
!
! --------------------------------------------------------------------------------------------------
!
! IO  table            : datastructure for table
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_all_col, i_col
    integer :: nb_cols, nb_para_real, nb_para_inte, nb_para
    aster_logical :: l_acti
    character(len=24) :: col_name
!
! --------------------------------------------------------------------------------------------------
!
    i_col        = 0
    nb_para      = 0
    nb_para_inte = 0
    nb_para_real = 0
    nb_cols      = table%nb_cols
!
! - On all cols
!
    do i_all_col = 1, nb_cols
        l_acti   = table%l_cols_acti(i_all_col)
        col_name = table%cols(i_all_col)%name
        if (l_acti) then
            i_col = i_col + 1
            table%list_para(i_col)  = col_name
            if (table%cols(i_all_col)%l_vale_inte) then
                table%type_para(i_col) = 'I'
                nb_para          = nb_para + 1
                nb_para_inte     = nb_para_inte + 1
            elseif (table%cols(i_all_col)%l_vale_real) then
                table%type_para(i_col) = 'R'
                nb_para          = nb_para + 1
                nb_para_real     = nb_para_real + 1
            else
                ASSERT(.false.)
            endif
        endif
    end do
!
! - Set total number of parameters
!
    ASSERT(nb_para .eq. (nb_para_real + nb_para_inte))
    ASSERT(nb_para .le. nb_cols)
    table%nb_para      = nb_para
    table%nb_para_real = nb_para_real
    table%nb_para_inte = nb_para_inte
!
end subroutine
