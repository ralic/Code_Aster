subroutine ddr_chck(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_ParaDDR), intent(in) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_DOMAINE_REDUIT - Initializations
!
! Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iret
    character(len=24) :: grelem_rid  = ' ', grnode_int  = ' '
    type(ROM_DS_Empi) :: empi_prim, empi_dual
    character(len=8) :: mesh_prim, mesh_dual, model_prim, model_dual, mesh
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_10')
    endif
!
! - Get parameters in datastructure
!
    mesh       = ds_para%mesh
    empi_prim  = ds_para%ds_empi_prim
    empi_dual  = ds_para%ds_empi_dual
    grelem_rid = ds_para%grelem_rid
    grnode_int = ds_para%grnode_int
!
! - Check mesh
!
    mesh_prim = empi_prim%mesh
    mesh_dual = empi_dual%mesh
    if (mesh_prim .ne. mesh_dual) then
        call utmess('F','ROM4_9')
    endif
    if (mesh .ne. mesh_prim) then
        call utmess('F','ROM4_10', sk = mesh)
    endif
!
! - Check model
!
    model_prim = empi_prim%model
    model_dual = empi_dual%model
    if (model_prim .eq. '#PLUSIEURS' .or. model_dual .eq. '#PLUSIEURS') then
        call utmess('F','ROM4_11')
    endif
!
! - Check groups
!
    call jeexin(mesh//'.GROUPENO', iret)
    if (iret .ne. 0) then
        call jenonu(jexnom(mesh//'.GROUPENO', grnode_int), iret)
        if (iret .ne. 0) then
            call utmess('F', 'ROM4_12', sk = grnode_int)
        endif
    endif
    call jeexin(mesh//'.GROUPEMA', iret)
    if (iret .ne. 0) then
        call jenonu(jexnom(mesh//'.GROUPEMA', grelem_rid), iret)
        if (iret .ne. 0) then
            call utmess('F', 'ROM4_13', sk = grelem_rid)
        endif
    endif
!
! - Check fields for empiric modes
!
    if (empi_prim%field_type .eq. 'TEMP') then
        if (empi_dual%field_type .ne. 'FLUX_NOEU') then
            call utmess('F', 'ROM4_17', sk = 'FLUX_NOEU')
        endif
    elseif (empi_prim%field_type .eq. 'DEPL') then
        if (empi_dual%field_type .ne. 'SIEF_NOEU') then
            call utmess('F', 'ROM4_17', sk = 'SIEF_NOEU')
        endif
    else
        call utmess('F', 'ROM4_16')
    endif
!
end subroutine

