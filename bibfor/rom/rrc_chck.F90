subroutine rrc_chck(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/romBaseChck.h"
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
! Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: mesh_prim, mesh_dual, model_prim, model_dual
    character(len=8) :: model_rom, model_dom
!
! --------------------------------------------------------------------------------------------------
!
    if (ds_para%tabl_name .eq. ' ') then
        call utmess('F', 'ROM6_4')
    endif
!
! - Check mesh
!
    mesh_prim = ds_para%ds_empi_prim%mesh
    mesh_dual = ds_para%ds_empi_dual%mesh
    if (mesh_prim .ne. mesh_dual) then
        call utmess('F','ROM4_9')
    endif
!
! - Check model
!
    model_prim = ds_para%ds_empi_prim%model
    model_dual = ds_para%ds_empi_dual%model
    if (model_prim .eq. '#PLUSIEURS' .or. model_dual .eq. '#PLUSIEURS') then
        call utmess('F','ROM4_11')
    endif
    if (model_prim .ne. model_dual) then
        call utmess('F', 'ROM6_2')
    endif
    model_rom    = ds_para%model_rom
    model_dom    = ds_para%model_dom
    if (model_rom .eq. model_dom) then
        call utmess('A', 'ROM6_8')
    endif
    if (model_prim .ne. model_dom) then
        call utmess('F', 'ROM6_9', sk = ds_para%ds_empi_prim%base)
    endif
    if (model_dual .ne. model_dom) then
        call utmess('F', 'ROM6_9', sk = ds_para%ds_empi_dual%base)
    endif
!
! - Check empiric modes base
!
    call romBaseChck(ds_para%ds_empi_prim)
    call romBaseChck(ds_para%ds_empi_dual)
!
end subroutine
