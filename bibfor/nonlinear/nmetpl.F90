subroutine nmetpl(sd_inout)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Update name of field in algorithm
!
! This utiliy is required for "hat" variables
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
    character(len=24) :: field_algo_old, field_algo_new
    character(len=6) :: hat_type, hat_vari
!
! --------------------------------------------------------------------------------------------------
!
!
! - Access to datastructure
!
    io_lcha = sd_inout(1:19)//'.LCHA'
    io_info = sd_inout(1:19)//'.INFO'
    call jeveuo(io_lcha, 'E', vk24 = v_io_para)
    call jeveuo(io_info, 'L', vi   = v_io_info)
    nb_field = v_io_info(1)
    zioch    = v_io_info(4)
!
! - Loop on fields
!
    do i_field = 1, nb_field
        field_algo_old = v_io_para(zioch*(i_field-1)+6 )
        if (field_algo_old(1:5) .eq. 'CHAP#') then
            hat_type = field_algo_old(6:11)
            if (hat_type .eq. 'VALINC') then
                hat_vari = field_algo_old(13:18)
                if (hat_vari .eq. 'TEMP') then
                    ASSERT(.false.)
                endif
                field_algo_new = field_algo_old
                field_algo_new(16:18) = 'PLU'
                v_io_para(zioch*(i_field-1)+6 ) = field_algo_new
            endif
        endif
    end do
!
end subroutine
