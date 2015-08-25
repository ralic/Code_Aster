subroutine nmeteo(result, sddisc , sd_inout, force, nume_store,&
                  time  , i_field, ds_print_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/diincl.h"
#include "asterfort/exisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmarcc.h"
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
    character(len=19), intent(in) :: sddisc
    character(len=8), intent(in) :: result
    integer, intent(in) :: i_field
    integer, intent(in) :: nume_store
    real(kind=8), intent(in) :: time
    aster_logical, intent(in) :: force
    type(NL_DS_Print), optional, intent(in) :: ds_print_
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Save field in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of datastructure for results
! In  sd_inout         : datastructure for input/output parameters
! In  nume_store       : index to store in results
! In  i_field          : field index
! In  ds_print         : datastructure for printing parameters
! In  sddisc           : datastructure for discretization
! In  time             : current time
! In  force            : .true. to store field whatever storing options
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: io_lcha, io_info
    character(len=24), pointer :: v_io_para(:) => null()
    integer, pointer :: v_io_info(:) => null()
    integer :: zioch, iret
    character(len=24) :: field_name_algo, field_algo, field_name_resu, flag_arch
    aster_logical :: l_print
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
! - Field to store ?
!
    flag_arch = v_io_para(zioch*(i_field-1)+9 )
    if (flag_arch .eq. 'OUI') then
!
! ----- Print for this step ?
!
        l_print = .true.
        if (present(ds_print_)) then
            l_print = ds_print_%l_print
        endif
!
! ----- Name of field (type) in results datastructure
!
        field_name_resu = v_io_para(zioch*(i_field-1)+1 )
!
! ----- Name of field in algorithm
!
        field_name_algo = v_io_para(zioch*(i_field-1)+6 )
        call nmetnc(field_name_algo, field_algo)
        call exisd('CHAMP', field_algo, iret)
!
! ----- Store field
!
        if (diincl(sddisc,field_name_resu,force ) .and. (iret.eq.1)) then
            if (l_print) then
                call utmess('I', 'ARCHIVAGE_6', sk=field_name_resu, si=nume_store, sr=time)
            endif
            call nmarcc(result, nume_store, field_name_resu, field_algo)
        endif
    endif
!
end subroutine
