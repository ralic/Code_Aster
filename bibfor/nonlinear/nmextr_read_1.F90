subroutine nmextr_read_1(sd_inout, keyw_fact    , nb_keyw_fact, list_field, rela_field_keyw,&
                         nb_field, nb_field_comp)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/nmextc.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_inout
    integer, intent(in) :: nb_keyw_fact
    character(len=16), intent(in) :: keyw_fact
    character(len=24), intent(out), pointer :: list_field(:)
    integer, intent(out), pointer :: rela_field_keyw(:)
    integer, intent(out) :: nb_field
    integer, intent(out) :: nb_field_comp
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Read fields to extract
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_inout         : datastructure for input/output parameters
! In  keyw_fact        : factor keyword to read extraction parameters
! In  nb_keyw_fact     : number of factor keyword to read extraction parameters
! Out list_field       : list of fields
! Out rela_field_keyw  : relation between field index and keyword index
! Out nb_field         : total number of fields
! Out nb_field_comp    : number of fields to compute (not a default in nonlinear operator)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_keyw_fact, i_field, i_list_field
    character(len=24) :: field_type, field_old
    aster_logical :: l_extr, l_find
!
! --------------------------------------------------------------------------------------------------
!
    nb_field      = 0
    nb_field_comp = 0
    if (nb_keyw_fact.eq.0) then
        goto 99
    endif
!
! - List of field to extract
!
    AS_ALLOCATE(vk24 = list_field, size = nb_keyw_fact)
!
! - Relation between field index and keyword index
!
    AS_ALLOCATE(vi   = rela_field_keyw, size = nb_keyw_fact)
!
    do i_keyw_fact = 1, nb_keyw_fact
!
! ----- Read field type
!
        call nmextc(sd_inout, keyw_fact, i_keyw_fact, field_type, l_extr)
        if (.not.l_extr) then
            field_type = 'NONE'
        endif
!
! ----- Add field in list to extract
!
        l_find = .false.
        do i_list_field = 1, nb_keyw_fact - 1
            field_old = list_field(i_list_field)
            if (field_old .eq. field_type) then
                i_field = i_list_field
                l_find  = .true.
            endif
        end do
        if (.not.l_find) then
            nb_field = nb_field + 1
            i_field  = nb_field
            list_field(i_field) = field_type
            if (field_type.eq.'EPSI_ELGA') then
                nb_field_comp = nb_field_comp + 1
            endif
        endif
!
! ----- Set relation between field index and keyword index
!
        if (field_type.eq.'EPSI_ELGA') then
            rela_field_keyw(i_keyw_fact) = -i_field
        else
            rela_field_keyw(i_keyw_fact) = i_field
        endif
    end do
!
 99 continue
!
end subroutine
