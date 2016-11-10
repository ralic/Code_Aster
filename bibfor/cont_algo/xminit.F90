subroutine xminit(mesh  , model , ds_contact, nume_inst, ds_measure,&
                  sddyna, hat_valinc, list_func_acti)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/copisd.h"
#include "asterfort/xmiszl.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/xmapin.h"
#include "asterfort/xmelem.h"
#include "asterfort/xoptin.h"
#include "asterfort/mmbouc.h"
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
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    type(NL_DS_Contact), intent(inout) :: ds_contact
    integer, intent(in) :: nume_inst
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: hat_valinc(*)
    character(len=19), intent(in) :: sddyna
    integer, intent(in) :: list_func_acti(*)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! XFEM method - Initializations
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IO  ds_contact       : datastructure for contact management
! In  nume_inst        : index of current step time
! In  hat_valinc       : hat variable for algorithm fields
! IO  ds_measure       : datastructure for measure and statistics management
! In  sddyna           : datastructure for dynamic
! In  list_func_acti   : list of active functionnalities
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_dyna, l_cont_xfem_gg, l_step_first
    character(len=19) :: sdcont_depgeo, sdcont_deplam
    character(len=19) :: disp_prev    
    character(len=19) :: xseuco, xseucp
    character(len=19) :: xindco, xmemco, xindcp, xmemcp, xcohes, xcohep
!
! --------------------------------------------------------------------------------------------------
!
    l_dyna         = ndynlo(sddyna,'DYNAMIQUE')
    l_cont_xfem_gg = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
    ASSERT(.not.l_dyna)
!
! - Using *_INIT options (like SEUIL_INIT)
!
    l_step_first = nume_inst .eq. 1
!
! - Get field names in hat-variables
!
    call nmchex(hat_valinc, 'VALINC', 'DEPMOI', disp_prev)
!
! - Lagrangians initialized (LAMBDA TOTAUX)
!
    if (l_cont_xfem_gg) then
        call xmiszl(disp_prev, ds_contact, mesh)
    endif
!
! - Management of status for time cut
!
    xindco = ds_contact%sdcont_solv(1:14)//'.XFIN'
    xmemco = ds_contact%sdcont_solv(1:14)//'.XMEM'
    xindcp = ds_contact%sdcont_solv(1:14)//'.XFIP'
    xmemcp = ds_contact%sdcont_solv(1:14)//'.XMEP'
    xseuco = ds_contact%sdcont_solv(1:14)//'.XFSE'
    xseucp = ds_contact%sdcont_solv(1:14)//'.XFSP'
    xcohes = ds_contact%sdcont_solv(1:14)//'.XCOH'
    xcohep = ds_contact%sdcont_solv(1:14)//'.XCOP'
    call copisd('CHAMP_GD', 'V', xindcp, xindco)
    call copisd('CHAMP_GD', 'V', xmemcp, xmemco)
    call copisd('CHAMP_GD', 'V', xseucp, xseuco)
    call copisd('CHAMP_GD', 'V', xcohep, xcohes)
!
! - Save displacements for geometric loop
!
    sdcont_depgeo = ds_contact%sdcont_solv(1:14)//'.DEPG'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_depgeo)
!
! - Save displacements for friction loop
!
    sdcont_deplam = ds_contact%sdcont_solv(1:14)//'.DEPF'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_deplam)
!
! - Geometric loop counter initialization
!
    call mmbouc(ds_contact, 'Geom', 'Init_Counter')
!
! - First geometric loop counter
!    
    call mmbouc(ds_contact, 'Geom', 'Incr_Counter')
!
! - Initial pairing
!
    if (l_cont_xfem_gg) then
        call xmapin(mesh, model, ds_contact, ds_measure)
    endif
!
! - Initial options
!
    if (l_cont_xfem_gg.and.l_step_first) then
        call xoptin(mesh, model, ds_contact)
    endif
!
! - Create fields
!
    call xmelem(mesh, model, ds_contact, list_func_acti)
!
end subroutine
