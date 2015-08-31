subroutine SetCol(table     , name_ , flag_acti_,&
                  flag_affe_, valer_, valei_    , valek_, mark_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
    type(NL_DS_Table), intent(inout) :: table
    character(len=*), optional, intent(in) :: name_
    aster_logical, optional, intent(in) :: flag_acti_
    aster_logical, optional, intent(in) :: flag_affe_
    real(kind=8), optional, intent(in) :: valer_
    integer, optional, intent(in) :: valei_
    character(len=*), optional, intent(in) :: valek_
    character(len=1), optional, intent(in) :: mark_
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Set column in table
!
! --------------------------------------------------------------------------------------------------
!
! In  table            : datastructure for table
! In  name             : name of column 
!                        If .not. present => all columns
! In  flag_acti        : flag for activation of column
! In  valer            : value (real) for column
! In  valei            : value (integer) for column
! In  valek            : value (string) for column
! In  flag_affe        : flag for value set in column
! In  mark             : mark for column
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_col, nb_cols, i_col_name
!
! --------------------------------------------------------------------------------------------------
!
    i_col_name    = 0
    nb_cols       = table%nb_cols
!
! - On all cols
!
    if (.not.present(name_)) then
        do i_col = 1, nb_cols
            if (present(flag_acti_)) then
                table%l_cols_acti(i_col) = flag_acti_
            endif
            if (present(mark_)) then
                table%cols(i_col)%mark = mark_
            endif
            ASSERT(.not.present(valer_))
            ASSERT(.not.present(valei_))
            ASSERT(.not.present(valek_))
            ASSERT(.not.present(flag_affe_))
        end do
    endif
!
! - On one column
!
    if (present(name_)) then
        do i_col = 1, nb_cols
            if (table%cols(i_col)%name .eq. name_) then
                ASSERT(i_col_name.eq.0)
                i_col_name = i_col   
            endif
        end do
        ASSERT(i_col_name.ne.0)
        if (present(mark_)) then
            table%cols(i_col_name)%mark = mark_
        endif
        if (present(flag_acti_)) then
            table%l_cols_acti(i_col_name) = flag_acti_
        endif
        if (present(flag_affe_)) then
            table%cols(i_col_name)%l_vale_affe = flag_affe_
        endif
        if (present(valer_)) then
            table%cols(i_col_name)%vale_real = valer_
            ASSERT(table%cols(i_col_name)%l_vale_real)
        endif
        if (present(valei_)) then
            table%cols(i_col_name)%vale_inte = valei_
            ASSERT(table%cols(i_col_name)%l_vale_inte)
        endif
        if (present(valek_)) then
            table%cols(i_col_name)%vale_strg = valek_
            ASSERT(table%cols(i_col_name)%l_vale_strg)
        endif
    endif
!
end subroutine
