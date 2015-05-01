subroutine ntetl3(result, sd_inout, i_field, tempct)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetnc.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_inout
    character(len=8), intent(in) :: result
    integer, intent(in) :: i_field
    real(kind=8), intent(in) :: tempct
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Input/output datastructure
!
! Read field for ETAT_INIT - Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! In  sd_inout         : datastructure for input/output parameters
! In  i_field          : field index
! In  tempct           : initial temperature if constant
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, iret
    character(len=24) :: flag_etat_init, field_name_resu, field_disc, field_gran, field_state
    character(len=24) :: valk(2)
    character(len=24) :: field_name_algo, field_algo
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
    zioch = v_io_info(4)
!
! - Field to read ?
!
    flag_etat_init = v_io_para(zioch*(i_field-1)+8 )
    if (flag_etat_init .eq. 'OUI') then
!
! ----- Name of field (type) in results datastructure
!
        field_name_resu = v_io_para(zioch*(i_field-1)+1 )
!
! ----- Name of field in algorithm
!
        field_name_algo = v_io_para(zioch*(i_field-1)+6 )
        call nmetnc(field_name_algo, field_algo)
!
! ----- Spatial discretization of field
!
        field_disc = v_io_para(zioch*(i_field-1)+5 )
!
! ----- Type of GRANDEUR of field
!
        field_gran = v_io_para(zioch*(i_field-1)+7 )
!
! ----- Actual state of field
!
        field_state = v_io_para(zioch*(i_field-1)+4 )
!
! ----- Informations about field
!
        if (field_state .eq. ' ') then
            call utmess('F', 'ETATINIT_30', sk=field_name_resu)
        else
            valk(1) = field_name_resu
            valk(2) = result
            if (field_state .eq. 'ZERO') then
                call utmess('I', 'ETATINIT_31', sk=field_name_resu)
            else if (field_state.eq.'SDRESU') then
                call utmess('I', 'ETATINIT_32', nk=2, valk=valk)
            else if (field_state.eq.'CHAMP') then
                call utmess('I', 'ETATINIT_33', sk=field_name_resu)
            else if (field_state.eq.'STATIONNAIRE') then
                call utmess('I', 'ETATINIT_34')
            else if (field_state.eq.'VALE') then
                call utmess('I', 'ETATINIT_35', sr=tempct)
            else
                ASSERT(.false.)
            endif
        endif
!
! ----- Check GRANDEUR and discretization
!
        if (field_gran .ne. ' ') then
            call chpver('F', field_algo, field_disc, field_gran, iret)
        endif
    endif
!
end subroutine
