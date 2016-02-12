subroutine nunuco_l(mesh, ds_contact, nume_dof, sdnume)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/get_equa_info.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: sdnume
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Initializations
!
! Get position of contact dof for patch (LAC)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  sdnume           : name of dof positions datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nt_patch, nb_equa, i_equa, i_patch, nume_node
    character(len=8) :: type_equa
    character(len=24) :: sdcont_ddlc
    integer, pointer :: v_sdcont_ddlc(:) => null()
    character(len=24) :: sdnume_nuco
    integer, pointer :: v_sdnume_nuco(:) => null()
    integer, pointer :: v_mesh_conopa(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Get parameters
!
    nt_patch = ds_contact%nt_patch
    call dismoi('NB_EQUA', nume_dof, 'NUME_DDL', repi=nb_equa)
!
! - Access to mesh
!
    call jeveuo(mesh//'.CONOPA','L', vi = v_mesh_conopa)
!
! - Access to unknowns
!
    sdnume_nuco = sdnume(1:19)//'.NUCO'
    call jeveuo(sdnume_nuco, 'L', vi = v_sdnume_nuco)
!
! - Create object
!
    sdcont_ddlc = ds_contact%sdcont_solv(1:14)//'.DDLC'
    call wkvect(sdcont_ddlc, 'V V I', nt_patch, vi = v_sdcont_ddlc)
!
! - Set equation number for each patch
!
    do i_equa = 1,nb_equa
        if (v_sdnume_nuco(i_equa) .eq. 1) then
            call get_equa_info(nume_dof, i_equa, type_equa, nume_nodez = nume_node)
            ASSERT(type_equa .eq. 'A')
            i_patch = v_mesh_conopa(nume_node)
            ASSERT(i_patch.gt.0 .and. i_patch.le.nt_patch)
            v_sdcont_ddlc(i_patch) = i_equa
        endif
    end do 
!
end subroutine
