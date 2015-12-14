subroutine mmmcri_frot(mesh, loop_fric_disp, disp_curr, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cnomax.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmconv.h"
#include "asterfort/vtaxpy.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: loop_fric_disp
    character(len=19), intent(in) :: disp_curr
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algo
!
! Friction loop management - Evaluate
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  loop_fric_disp   : dispalcement for current friction loop
! In  disp_curr        : current displacements
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cmp_lagc = 1
    character(len=8), parameter :: list_cmp_lagc(nb_cmp_lagc) = (/'LAGS_C'/)
    real(kind=8) :: frot_diff_maxi, disp_curr_maxi
    real(kind=8) :: loop_fric_vale, alpha,  frot_epsi_maxi
    character(len=24) :: frot_diff
    character(len=16) :: loop_fric_node
    character(len=8) :: node_name
    integer :: frot_diff_node, disp_curr_node
    aster_logical :: loop_fric_conv
!
! --------------------------------------------------------------------------------------------------
!
    frot_diff_maxi = 0.d0
    loop_fric_vale = 0.d0
    loop_fric_node = ' '
    loop_fric_vale = r8vide()
    alpha          = -1.d0
    loop_fric_conv = .false.
!
! - Get parameters
!
    frot_epsi_maxi = cfdisr(ds_contact%sdcont_defi,'RESI_FROT' )
!
! - Compute difference disp_curr - loop_fric_disp
!
    frot_diff = '&&MMMCRI.VTDIFF'
    call copisd('CHAMP_GD', 'V', disp_curr, frot_diff)
    call vtaxpy(alpha, loop_fric_disp, frot_diff)
!
! - Find maximas
!
    call cnomax(frot_diff, nb_cmp_lagc, list_cmp_lagc, frot_diff_maxi, frot_diff_node)
    call cnomax(disp_curr, nb_cmp_lagc, list_cmp_lagc, disp_curr_maxi, disp_curr_node)
!
! - Compute criterion
!
    if (disp_curr_maxi .gt. 0.d0) then
        loop_fric_vale = frot_diff_maxi/disp_curr_maxi
    else
        loop_fric_vale = 0.d0
    endif
!
! - Criterion test
!
    if (loop_fric_vale .lt. abs(frot_epsi_maxi)) then
        loop_fric_conv = .true.
    else
        loop_fric_conv = .false.
    endif  
!
! - Get name of node
!
    if (frot_diff_node .eq. 0) then
        node_name = ' '
    else
        call jenuno(jexnum(mesh//'.NOMNOE', frot_diff_node), node_name)
    endif
    loop_fric_node = node_name
!
! - Save values
!
    call mmbouc(ds_contact, 'Fric', 'Set_Locus', loop_locus_ = loop_fric_node)
    call mmbouc(ds_contact, 'Fric', 'Set_Vale' , loop_vale_  = loop_fric_vale)
    if (loop_fric_conv) then
        call mmbouc(ds_contact, 'Fric', 'Set_Convergence')
    else
        call mmbouc(ds_contact, 'Fric', 'Set_Divergence')
    endif
!
    call detrsd('CHAMP_GD', frot_diff)
!
end subroutine
