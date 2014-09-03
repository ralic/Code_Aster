subroutine nmextd(field_name_resu, sd_inout, field)
!
implicit none
!
#include "asterfort/jeveuo.h"
#include "asterfort/nmetnc.h"
#include "asterfort/nmetob.h"
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
    character(len=*), intent(in) :: field_name_resu
    character(len=*), intent(out) :: field 
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Extraction (OBSERVATION/SUIVI_DDL) utilities 
!
! Get field
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_inout        : datastructure for input/output paramnmextnmeters
! In  field_name_resu : name of field in algorithme
! Out field           : name of datastructure for field
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch
    character(len=24) :: field_name_algo
    integer :: i_field_obsv
!
! --------------------------------------------------------------------------------------------------
!
!
! - Access to datastructure
!
    io_lcha = sd_inout(1:19)//'.LCHA'
    io_info = sd_inout(1:19)//'.INFO'
    call jeveuo(io_lcha, 'L', vk24 = v_io_para)
    call jeveuo(io_info, 'L', vi   = v_io_info)
    zioch = v_io_info(4)
!
! - Get index of field used for OBSERVATION
!
    call nmetob(sd_inout, field_name_resu, i_field_obsv)
!
! - Get name of datastructure for field
!
    if (i_field_obsv.ne.0) then
        field_name_algo  = v_io_para(zioch*(i_field_obsv-1)+6 )
        call nmetnc(field_name_algo, field)
    endif
!
end subroutine
