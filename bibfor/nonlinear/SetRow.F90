subroutine SetRow(table     , name_ , flag_acti_,&
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
! Set row in table
!
! --------------------------------------------------------------------------------------------------
!
! In  table            : datastructure for table
! In  name             : name of row 
!                        If .not. present => all rows
! In  flag_acti        : flag for activation of row
! In  valer            : value (real) for row
! In  valei            : value (integer) for row
! In  valek            : value (string) for row
! In  flag_affe        : flag for value set in row
! In  mark             : mark for row
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_row, nb_rows, i_row_name
!
! --------------------------------------------------------------------------------------------------
!
    i_row_name    = 0
    nb_rows       = table%nb_rows
!
! - On all rows
!
    if (.not.present(name_)) then
        do i_row = 1, nb_rows
            if (present(flag_acti_)) then
                table%l_rows_acti(i_row) = flag_acti_
            endif
            if (present(mark_)) then
                table%rows(i_row)%mark = mark_
            endif
            ASSERT(.not.present(valer_))
            ASSERT(.not.present(valei_))
            ASSERT(.not.present(valek_))
            ASSERT(.not.present(flag_affe_))
        end do
    endif
!
! - On one row
!
    if (present(name_)) then
        do i_row = 1, nb_rows
            if (table%rows(i_row)%name .eq. name_) then
                ASSERT(i_row_name.eq.0)
                i_row_name = i_row   
            endif
        end do
        ASSERT(i_row_name.ne.0)
        if (present(mark_)) then
            table%rows(i_row_name)%mark = mark_
        endif
        if (present(flag_acti_)) then
            table%l_rows_acti(i_row_name) = flag_acti_
        endif
        if (present(flag_affe_)) then
            table%rows(i_row_name)%l_vale_affe = flag_affe_
        endif
        if (present(valer_)) then
            table%rows(i_row_name)%vale_real = valer_
            ASSERT(table%rows(i_row_name)%l_vale_real)
        endif
        if (present(valei_)) then
            table%rows(i_row_name)%vale_inte = valei_
            ASSERT(table%rows(i_row_name)%l_vale_inte)
        endif
        if (present(valek_)) then
            table%rows(i_row_name)%vale_strg = valek_
            ASSERT(table%rows(i_row_name)%l_vale_strg)
        endif
    endif
!
end subroutine
