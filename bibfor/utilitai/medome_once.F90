subroutine medome_once(result, v_list_store, nb_store, nume_user_,&
                       model_, cara_elem_  , chmate_ , list_load_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/rslesd.h"
#include "asterfort/utmess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: result
    integer, pointer, intent(in) :: v_list_store(:)
    integer, intent(in) :: nb_store
    integer, optional, intent(in) :: nume_user_
    character(len=8), optional, intent(out) :: model_
    character(len=8), optional, intent(out) :: cara_elem_
    character(len=24), optional, intent(out) :: chmate_
    character(len=19), optional, intent(out) :: list_load_
!
! --------------------------------------------------------------------------------------------------
!
! Get datastructures in result and check unicity
!
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_store, nume_store0, nume_store, iexcit
    character(len=8) :: model0, cara_elem0
    character(len=8) :: model, cara_elem
    character(len=24) :: chmate0, chmate
    character(len=19) :: list_load0, list_load
!
! --------------------------------------------------------------------------------------------------
!
    if (present(nume_user_)) then
        nume_store0 = nume_user_
    else
        nume_store0 = v_list_store(1)
    endif
!
! - Get reference datastructures
!
    call rslesd(result    , nume_store0, model0, chmate0, cara_elem0,&
                list_load0, iexcit)
    model     = model0
    chmate    = chmate0
    cara_elem = cara_elem0
    list_load = list_load0
!
! - Check unicity
!             
    do i_store = 2, nb_store
        nume_store = v_list_store(i_store)
        call rslesd(result   , nume_store, model, chmate, cara_elem,&
                    list_load)
        if (present(model_)) then
            if (model .ne. model0) then
                call utmess('F', 'CALCCHAMP_23')
            endif
        endif
        if (present(list_load_)) then
            if ((list_load .ne. list_load0) .and. (iexcit .eq. 0)) then
                call utmess('F', 'CALCCHAMP_24')
            endif
        endif
    end do
!
! - Output
!
    if (present(model_)) then
        model_ = model
    endif
    if (present(chmate_)) then
        chmate_ = chmate
    endif
    if (present(cara_elem_)) then
        cara_elem_ = cara_elem
    endif
    if (present(list_load_)) then
        list_load_ = list_load
    endif
!
end subroutine
