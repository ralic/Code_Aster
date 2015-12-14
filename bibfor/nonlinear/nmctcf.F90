subroutine nmctcf(mesh, model, sderro, hval_incr, ds_print, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmmcri_frot.h"
#include "asterfort/mmreas.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
#include "asterfort/xreacl.h"
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
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: sderro
    character(len=19), intent(in) :: hval_incr(*)
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algo
!
! Friction loop management - Management
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  sderro           : datastructure for errors during algorithm
! In  hval_incr        : hat-variable for incremental values fields
! IO  ds_print         : datastructure for printing parameters
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_cont_cont, l_cont_xfem, l_cont_xfem_gg
    aster_logical :: loop_fric_error
    integer :: iter_fric_maxi
    integer :: loop_fric_count
    character(len=19) :: disp_curr, loop_fric_disp
    character(len=16) :: loop_fric_node
    real(kind=8) :: loop_fric_vale
    aster_logical :: loop_fric_conv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> MISE A JOUR DU SEUIL DE TRESCA'
    endif
!
! - Initializations
!
    loop_fric_conv  = .false.
    loop_fric_node  = ' '
    loop_fric_vale  = r8vide()
    call mmbouc(ds_contact, 'Fric', 'Set_NoError')
!
! - Get fields
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', disp_curr)
!
! - Get contact parameters
!
    l_cont_cont    = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_xfem_gg = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
    l_cont_xfem    = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
!
! - Get friction loop parameters
!
    loop_fric_disp = ds_contact%sdcont_solv(1:14)//'.DEPF'
    iter_fric_maxi = cfdisi(ds_contact%sdcont_defi,'ITER_FROT_MAXI')
!
! - Update triggers
!
    if (l_cont_xfem) then
        if (.not.l_cont_xfem_gg) then
            call xreacl(mesh, model, hval_incr, ds_contact)
        endif
    else if (l_cont_cont) then
        call mmreas(mesh, ds_contact, hval_incr)
    else
        ASSERT(.false.)
    endif
!
! - Compute friction criterion
!
    call mmmcri_frot(mesh, loop_fric_disp, disp_curr, ds_contact)
!
! - Get final loop state
!
    call mmbouc(ds_contact, 'Fric', 'Is_Convergence', loop_state_ = loop_fric_conv)
    call mmbouc(ds_contact, 'Fric', 'Get_Locus'     , loop_locus_ = loop_fric_node)
    call mmbouc(ds_contact, 'Fric', 'Get_Vale'      , loop_vale_  = loop_fric_vale)
    call mmbouc(ds_contact, 'Fric', 'Read_Counter'  , loop_fric_count) 
!
! - Too many iterations ?
!
    if ((.not.loop_fric_conv) .and. (loop_fric_count .eq. iter_fric_maxi)) then
        call mmbouc(ds_contact, 'Fric', 'Set_Error')
    endif
    call mmbouc(ds_contact, 'Fric', 'Is_Error', loop_state_ = loop_fric_error)
!
! - Save events
!
    call nmcrel(sderro, 'ERRE_CTCF', loop_fric_error)
    if (loop_fric_conv) then
        call nmcrel(sderro, 'DIVE_FIXF', .false._1)
    else
        call nmcrel(sderro, 'DIVE_FIXF', .true._1)
    endif
!
! - Set values in convergence table for contact geoemtry informations
!
    call nmimck(ds_print, 'BOUC_NOEU', loop_fric_node, .true._1)
    call nmimcr(ds_print, 'BOUC_VALE', loop_fric_vale, .true._1)
!
! - Update reference displacement for friction loop
!
    if (.not.loop_fric_conv) then
        call copisd('CHAMP_GD', 'V', disp_curr, loop_fric_disp)
    endif
!
end subroutine
