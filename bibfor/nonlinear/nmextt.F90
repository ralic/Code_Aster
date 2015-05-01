subroutine nmextt(sd_inout, field_type, field_disc)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetob.h"
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
    character(len=24), intent(in) :: field_type
    character(len=4), intent(out) :: field_disc
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Get localization of field (discretization: NOEU or ELGA)
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_inout         : datastructure for input/output parameters
! In  field_type       : type of field (name in results datastructure)
! Out field_disc       : localization of field (discretization: NOEU or ELGA)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, i_field
!
! --------------------------------------------------------------------------------------------------
!
    field_disc = 'XXXX'
!
! - Access to datastructure
!
    io_lcha = sd_inout(1:19)//'.LCHA'
    io_info = sd_inout(1:19)//'.INFO'
    call jeveuo(io_lcha, 'L', vk24 = v_io_para)
    call jeveuo(io_info, 'L', vi   = v_io_info)
    zioch    = v_io_info(4)
!
! - Index of field
!
    call nmetob(sd_inout, field_type, i_field)
!
! - Get localization of field
!
    field_disc = v_io_para(zioch*(i_field-1)+5 )(1:4)
!
! - Check
!
    ASSERT(field_disc.eq.'NOEU'.or.field_disc.eq.'ELGA')

end subroutine
