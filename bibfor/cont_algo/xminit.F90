subroutine xminit(mesh  , model , sdcont_defi, sdcont_solv, nume_inst,&
                  sdtime, sdstat, sddyna     , hat_valinc )
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
    integer, intent(in) :: nume_inst
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: hat_valinc(*)
    character(len=19), intent(in) :: sddyna
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
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
! In  nume_inst        : index of current step time
! In  hat_valinc       : hat variable for algorithm fields
! In  sdtime           : datastructure for timers
! In  sdstat           : datastructure for statistics
! In  sddyna           : datastructure for dynamic
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
    l_cont_xfem_gg = cfdisl(sdcont_defi,'CONT_XFEM_GG')
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
        call xmiszl(disp_prev, sdcont_defi, mesh)
    endif
!
! - Management of status for time cut
!
    xindco = sdcont_solv(1:14)//'.XFIN'
    xmemco = sdcont_solv(1:14)//'.XMEM'
    xindcp = sdcont_solv(1:14)//'.XFIP'
    xmemcp = sdcont_solv(1:14)//'.XMEP'
    xseuco = sdcont_solv(1:14)//'.XFSE'
    xseucp = sdcont_solv(1:14)//'.XFSP'
    xcohes = sdcont_solv(1:14)//'.XCOH'
    xcohep = sdcont_solv(1:14)//'.XCOP'
    call copisd('CHAMP_GD', 'V', xindcp, xindco)
    call copisd('CHAMP_GD', 'V', xmemcp, xmemco)
    call copisd('CHAMP_GD', 'V', xseucp, xseuco)
    call copisd('CHAMP_GD', 'V', xcohep, xcohes)
!
! - Save displacements for geometric loop
!
    sdcont_depgeo = sdcont_solv(1:14)//'.DEPG'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_depgeo)
!
! - Save displacements for friction loop
!
    sdcont_deplam = sdcont_solv(1:14)//'.DEPF'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_deplam)
!
! - Geometric loop counter initialization
!
    call mmbouc(sdcont_solv, 'GEOM', 'INIT')
!
! - First geometric loop counter
!
    call mmbouc(sdcont_solv, 'GEOM', 'INCR')
!
! - Initial pairing
!
    if (l_cont_xfem_gg) then
        call xmapin(mesh  , model , sdcont_defi, sdcont_solv, sdtime,&
                    sdstat)
    endif
!
! - Initial options
!
    if (l_cont_xfem_gg.and.l_step_first) then
        call xoptin(mesh, model, sdcont_defi, sdcont_solv)
    endif
!
! - Create fields
!
    call xmelem(mesh, model, sdcont_defi, sdcont_solv)
!
end subroutine
