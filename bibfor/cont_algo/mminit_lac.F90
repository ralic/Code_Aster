subroutine mminit_lac(mesh     , ds_contact, hat_valinc, ds_measure, sdnume,&
                      nume_inst)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmapin.h"
#include "asterfort/misazl.h"
#include "asterfort/copisd.h"
#include "asterfort/nmchex.h"
#include "asterfort/mmopti_lac.h"
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
!
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=19), intent(in) :: hat_valinc(*)
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: sdnume
    integer, intent(in) :: nume_inst
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Initializations
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! IO  ds_contact       : datastructure for contact management
! In  hat_valinc       : hat variable for algorithm fields
! IO  ds_measure       : datastructure for measure and statistics management
! In  sdnume           : name of dof positions datastructure
! In  nume_inst        : index of current step time
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_allv, l_step_first
    character(len=19) :: sdcont_depgeo, disp_prev, sdcont_depini
!
! --------------------------------------------------------------------------------------------------
!    
    l_cont_allv  = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
    ASSERT(.not.l_cont_allv)
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
    sdcont_depini = ds_contact%sdcont_solv(1:14)//'.INIT'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_depini)
    call misazl(sdnume, disp_prev)
!
! - Save displacements for geometric loop
!
    sdcont_depgeo = ds_contact%sdcont_solv(1:14)//'.DEPG'
    call copisd('CHAMP_GD', 'V', disp_prev, sdcont_depgeo)
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
    call mmapin(mesh, ds_contact, ds_measure)
!
! - Initial options
!
    if (.not.l_cont_allv .and. l_step_first) then
       call mmopti_lac(mesh, ds_contact)
    endif
!
end subroutine
