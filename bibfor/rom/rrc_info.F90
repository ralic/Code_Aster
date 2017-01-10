subroutine rrc_info(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
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
    type(ROM_DS_ParaRRC), intent(in) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! REST_REDUIT_COMPLET - Initializations
!
! Informations
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: valk(2)
    character(len=16) :: type_resu
    character(len=8) :: result_rom, result_dom, model_dom, model_rom
    integer :: nb_store
!
! --------------------------------------------------------------------------------------------------
!
    type_resu    = ds_para%type_resu
    result_rom   = ds_para%result_rom
    model_rom    = ds_para%model_rom
    nb_store     = ds_para%nb_store
    result_dom   = ds_para%result_dom
    model_dom    = ds_para%model_dom
!
! - Print
!
    valk(1) = result_dom
    valk(2) = model_dom
    call utmess('I', 'ROM6_5', nk = 2, valk = valk)
    call utmess('I', 'ROM6_7', sk = type_resu)
    valk(1) = result_rom
    valk(2) = model_rom
    call utmess('I', 'ROM6_10', nk = 2, valk = valk)
    call utmess('I', 'ROM6_11', si = nb_store)
!
end subroutine
