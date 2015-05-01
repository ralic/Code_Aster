subroutine nmetob(sd_inout, field_name_resu, i_field_obsv)
!
implicit none
!
#include "asterfort/jeveuo.h"
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
    character(len=24), intent(in) :: sd_inout
    character(len=24), intent(in) :: field_name_resu
    integer, intent(out) :: i_field_obsv
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Get index of field used for OBSERVATION
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_inout         : datastructure for input/output parameters
! In  field_name_resu  : name of field (type) in results datastructure
! Out i_field_obsv     : index of field - 0 if not used for OBSERVATION
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, nb_field
    integer :: i_field
    character(len=24) :: keyw_obsv
!
! --------------------------------------------------------------------------------------------------
!
    i_field_obsv = 0
!
! - Access to datastructure
!
    io_lcha = sd_inout(1:19)//'.LCHA'
    io_info = sd_inout(1:19)//'.INFO'
    call jeveuo(io_lcha, 'L', vk24 = v_io_para)
    call jeveuo(io_info, 'L', vi   = v_io_info)
    nb_field = v_io_info(1)
    zioch    = v_io_info(4)
!
! - Find field
!
    do i_field = 1, nb_field
        keyw_obsv = v_io_para(zioch*(i_field-1)+10)
        if (keyw_obsv .eq. field_name_resu) then
            i_field_obsv = i_field
        endif
    end do
!
end subroutine
