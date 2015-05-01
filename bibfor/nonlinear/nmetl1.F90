subroutine nmetl1(result, nume_store_0, sd_inout, i_field)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetnc.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
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
    integer, intent(in) :: nume_store_0
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Read field for ETAT_INIT - From results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result          : name of datastructure for results
! In  sd_inout        : datastructure for input/output parameters
! In  nume_store_0    : initial number of storage in results
! In  i_field         : field index
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, ievol, iret
    character(len=24) :: valk(2)
    character(len=24) :: field_resu, field_algo
    character(len=24) :: flag_etat_init, field_name_resu
    character(len=24) :: field_name_algo, field_name_init, field_disc
!
! --------------------------------------------------------------------------------------------------
!
    field_resu = '&&NMETL1.CHAMP'
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
        field_disc = v_io_para(zioch*(i_field-1)+5 )
!
! ----- Name of field in algorithm
!
        field_name_algo  = v_io_para(zioch*(i_field-1)+6 )
        call nmetnc(field_name_algo, field_algo)
!
! ----- Get field in resultats datastructure
!
        call rsexch(' ', result, field_name_resu, nume_store_0, field_resu,&
                    ievol)
!
! ----- Field conversion if necessary
!
        if (ievol .ne. 0) then
            if (field_name_init .ne. ' ') then
!
! ------------- Not present: take initial state
!
                call copisd('CHAMP', 'V', field_name_init, field_algo)
                v_io_para(zioch*(i_field-1)+4) = 'ZERO'
            endif
        else
            if (field_disc .eq. 'NOEU') then
                call vtcopy(field_resu, field_algo, ' ', iret)
                if (iret .ne. 0) then
                    valk(1) = field_resu
                    valk(2) = field_algo
                    call utmess('A', 'MECANONLINE_2', nk=2, valk=valk)
                endif
            elseif ((field_disc.eq.'ELGA').or.&
                    (field_disc.eq.'ELNO').or.&
                    (field_disc.eq.'ELEM')) then
                call copisd('CHAMP_GD', 'V', field_resu, field_algo)
            else
                write(6,*) 'LOCCHA: ',field_disc
                ASSERT(.false.)
            endif
!
! --------- Present
!
            v_io_para(zioch*(i_field-1)+4) = 'SDRESU'
        endif
    endif
!
end subroutine
