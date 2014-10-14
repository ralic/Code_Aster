subroutine nmetl2(keyword_fact, sd_inout, i_field)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetcv.h"
#include "asterfort/nmetnc.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
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
    integer, intent(in) :: i_field
    character(len=16), intent(in) :: keyword_fact
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Read field for ETAT_INIT - Field by field
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_inout         : datastructure for input/output parameters
! In  i_field          : field index
! In  keyword_fact     : factor keyword for ETAT_INIT
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, ilecc, iret
    character(len=24) :: valk(3)
    character(len=24) :: field_resu, field_resu_cv, field_algo, keyw_etat_init
    character(len=24) :: flag_etat_init, field_name_resu, field_state
    character(len=24) :: field_name_algo, field_name_init, field_disc_in, field_disc_out
!
! --------------------------------------------------------------------------------------------------
!
    field_resu_cv = '&&NMETL2.CHAMP.CONVER'
    ilecc         = 0
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
! ----- Name of field for initial state (ETAT_INIT)
!
        field_name_init = v_io_para(zioch*(i_field-1)+2 )
!
! ----- Spatial discretization of field
!
        field_disc_out  = v_io_para(zioch*(i_field-1)+5 )
!
! ----- Name of field in algorithm
!
        field_name_algo = v_io_para(zioch*(i_field-1)+6 )
        call nmetnc(field_name_algo, field_algo)
!
! ----- Actual state of field
!
        field_state = v_io_para(zioch*(i_field-1)+4 )
!
! ----- Keyword for initial state
!
        keyw_etat_init = v_io_para(zioch*(i_field-1)+3 )
!
! ----- Read field
!
        if (keyw_etat_init .ne. ' ') then
            call getvid(keyword_fact, keyw_etat_init, iocc=1, scal=field_resu, nbret=ilecc)
        endif
!
! ----- Read initial field
!
        if (ilecc .eq. 0) then
            if ((field_state.ne.'SDRESU') .and. (field_name_init.ne.' ')) then
                call copisd('CHAMP', 'V', field_name_init, field_algo)
                v_io_para(zioch*(i_field-1)+4) = 'ZERO'
            endif
        else
!
! --------- Discretization of input field
!
            call dismoi('TYPE_CHAMP', field_resu, 'CHAMP', repk=field_disc_in, arret='C')
            if (iret .eq. 1) then
                call utmess('F', 'ETATINIT_50', sk=field_resu)
            endif
!
! --------- Try to convert field (discretization) if necessary
!
            call nmetcv(field_name_init, field_resu, field_disc_in, field_resu_cv, field_disc_out)
!
! --------- Copy field
!
            if (field_disc_out .eq. 'NOEU') then
                call vtcopy(field_resu_cv, field_algo, ' ', iret)
                if (iret .ne. 0) then
                    valk(1) = field_resu_cv
                    valk(2) = field_algo
                    call utmess('A', 'MECANONLINE_2', nk=2, valk=valk)
                endif
            else if ((field_disc_out.eq.'ELGA').or.(field_disc_out.eq.'ELEM').or.&
                     (field_disc_out.eq.'ELNO')) then
                call copisd('CHAMP_GD', 'V', field_resu_cv, field_algo)
            else
                write(6,*) 'DISCRETISATION NON TRAITEE: ',field_disc_in
                ASSERT(.false.)
            endif
!
! --------- New state of field
!
            v_io_para(zioch*(i_field-1)+4) = 'CHAMP'
        endif
    endif
!
    call detrsd('CHAMP', field_resu_cv)
!
end subroutine
