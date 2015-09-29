subroutine GetIOField(ds_inout, field_type,&
                      l_read_ , l_acti_)
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
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=*), intent(in) :: field_type
    aster_logical, optional, intent(out) :: l_read_
    aster_logical, optional, intent(out) :: l_acti_
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output management
!
! Get values for field
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
! In  field_type       : type of field (symbolic name in result datastructure)
! Out l_read           : .true. if this field is read
! Out l_acti           : .true. if this field is activated
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_field_type, i_field, nb_field
!
! --------------------------------------------------------------------------------------------------
!
    i_field_type = 0
    nb_field     = ds_inout%nb_field
!
! - Find field
!
    do i_field = 1, nb_field
        if (ds_inout%field(i_field)%type .eq. field_type) then
            ASSERT(i_field_type.eq.0)
            i_field_type = i_field
        endif
    end do
    ASSERT(i_field_type.ne.0)
!
! - Get parameters
!
    if (present(l_read_)) then
        l_read_ = ds_inout%l_field_read(i_field_type)
    endif
    if (present(l_acti_)) then
        l_acti_ = ds_inout%l_field_acti(i_field_type)
    endif
!
end subroutine
