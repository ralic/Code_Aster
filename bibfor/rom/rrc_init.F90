subroutine rrc_init(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbexve.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rs_get_liststore.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rrc_info.h"
#include "asterfort/dismoi.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_ParaRRC), intent(inout) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! REST_REDUIT_COMPLET - Initializations
!
! Initializations
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iret
    character(len=24) :: typval, field_type
    integer :: nbval, nb_store, nb_mode
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM6_3')
    endif
!
! - Get table for reduced coordinates
!
    call ltnotb(ds_para%result_rom, 'COOR_REDUIT', ds_para%tabl_name, iret_ = iret)
    if (iret .gt. 0) then
        ds_para%tabl_name = ' '
    endif
!
! - Get reduced coordinates
!
    if (iret .eq. 0) then
        call tbexve(ds_para%tabl_name, 'COOR_REDUIT', ds_para%coor_redu, 'V', nbval, typval)
        ASSERT(typval .eq. 'R')
    endif
    nb_mode              = ds_para%ds_empi_prim%nb_mode
!
! - Type of result
!
    field_type = ds_para%ds_empi_prim%field_type
    if (field_type .eq. 'DEPL') then
        ds_para%type_resu = 'EVOL_NOLI'
    elseif (field_type .eq. 'TEMP') then
        ds_para%type_resu = 'EVOL_THER'
    else
        ASSERT(.false.)
    endif
!
! - Create output result datastructure
!
    call rs_get_liststore(ds_para%result_rom, nb_store)
    ds_para%nb_store = nb_store
    call rscrsd('G', ds_para%result_dom, ds_para%type_resu, nb_store)
!
! - Set models
!
    call dismoi('MODELE', ds_para%result_rom, 'RESULTAT', repk=ds_para%model_rom)
!
! - Print parameters
!
    if (niv .ge. 2) then
        call rrc_info(ds_para)
    endif
!
end subroutine
