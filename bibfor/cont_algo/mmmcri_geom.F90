subroutine mmmcri_geom(mesh      , disp_prev, loop_geom_disp, disp_curr,&
                       ds_contact)
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
    character(len=19), intent(in) :: disp_prev
    character(len=19), intent(in) :: loop_geom_disp
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
! In  disp_prev        : previous displacements
! In  loop_geom_disp   : dispalcement for current geometry loop
! In  disp_curr        : current displacements
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cmp_disp = 3
    character(len=8), parameter :: list_cmp_disp(nb_cmp_disp) = (/'DX','DY','DZ'/)
    real(kind=8) :: geom_dif1_maxi, geom_dif2_maxi, geom_mini
    real(kind=8) :: loop_geom_vale, alpha, geom_maxi, geom_epsi_maxi
    character(len=24) :: geom_diff_1, geom_diff_2
    character(len=16) :: loop_geom_node
    character(len=8) :: node_name
    integer :: geom_dif1_node, geom_dif2_node
    aster_logical :: loop_geom_conv
!
! --------------------------------------------------------------------------------------------------
!
    geom_dif1_maxi = 0.d0
    geom_dif2_maxi = 0.d0
    loop_geom_vale = 0.d0
    loop_geom_node = ' '
    loop_geom_vale = r8vide()
    alpha          = -1.d0
    loop_geom_conv = .false.
!
! - Get parameters
!
    geom_epsi_maxi = cfdisr(ds_contact%sdcont_defi,'RESI_GEOM' )
!
! - Compute difference disp_curr - loop_geom_disp
!
    geom_diff_1 = '&&MMMCRI.VTDIFF'
    call copisd('CHAMP_GD', 'V', disp_curr, geom_diff_1)
    call vtaxpy(alpha, loop_geom_disp, geom_diff_1)
!
! - Compute difference disp_prev - loop_geom_disp
!
    geom_diff_2 = '&&MMMCRI.VTDIF2'
    call copisd('CHAMP_GD', 'V', loop_geom_disp, geom_diff_2)
    call vtaxpy(alpha, disp_prev, geom_diff_2)
!
! - Find maximas
!
    call cnomax(geom_diff_1, nb_cmp_disp, list_cmp_disp, geom_dif1_maxi, geom_dif1_node)
    call cnomax(geom_diff_2, nb_cmp_disp, list_cmp_disp, geom_dif2_maxi, geom_dif2_node)      
!
! - Update maximum
!
    geom_maxi = ds_contact%geom_maxi
    if (geom_maxi .lt. 0.d0) then
        geom_maxi = geom_dif2_maxi
        geom_mini = r8prem()
    else
        geom_maxi = max(geom_maxi,geom_dif2_maxi)
        geom_mini = 1.d-6*geom_maxi
    endif
    ds_contact%geom_maxi = geom_maxi
!
! - Compute criterion
!
    if (geom_dif2_maxi .le. geom_mini) then
        if (geom_dif2_maxi .eq. 0.d0) then
            loop_geom_vale = 10.0d0*geom_epsi_maxi
        else
            loop_geom_vale = 1.d-1*geom_epsi_maxi
        endif
    else
        loop_geom_vale = geom_dif1_maxi/geom_dif2_maxi
    endif
!
! - Criterion test
!
    if (loop_geom_vale .lt. abs(geom_epsi_maxi)) then
        loop_geom_conv = .true.
    else
        loop_geom_conv = .false.
    endif  
!
! - Get name of node
!
    if (geom_dif1_node .eq. 0) then
        loop_geom_node = ' '
    else
        call jenuno(jexnum(mesh//'.NOMNOE', geom_dif1_node), node_name)
    endif
    loop_geom_node = node_name
!
! - Save values
!
    call mmbouc(ds_contact, 'Geom', 'Set_Locus', loop_locus_ = loop_geom_node)
    call mmbouc(ds_contact, 'Geom', 'Set_Vale' , loop_vale_  = loop_geom_vale)
    if (loop_geom_conv) then
        call mmbouc(ds_contact, 'Geom', 'Set_Convergence')
    else
        call mmbouc(ds_contact, 'Geom', 'Set_Divergence')
    endif
!
    call detrsd('CHAMP_GD', geom_diff_1)
    call detrsd('CHAMP_GD', geom_diff_2)
!
end subroutine
