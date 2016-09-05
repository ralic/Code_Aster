subroutine ddr_main(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/ddr_comp.h"
#include "asterfort/ddr_crid.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utlisi.h"
#include "asterfort/utmess.h"
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
! DEFI_DOMAINE_REDUIT - Main process
!
! Compute EIM (DEIM method) and apply for two bases
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters of EIM computation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_mode_prim, nb_mode_dual, nb_mode_total
    integer :: nb_node, nb_cmp_prim, nb_cmp_dual
    integer :: nb_node_rid, i_node_rid
    character(len=8) :: mesh
    character(len=24):: grelem_rid, grnode_int
    integer, pointer :: v_list_prim(:) => null()
    integer, pointer :: v_list_dual(:) => null()
    integer, pointer :: v_list_unio(:) => null()
    integer, pointer :: v_list_rid(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
! - Get parameters
!
    mesh           = ds_para%mesh
    grelem_rid     = ds_para%grelem_rid
    grnode_int     = ds_para%grnode_int
    nb_mode_prim   = ds_para%ds_empi_prim%nb_mode
    nb_mode_dual   = ds_para%ds_empi_dual%nb_mode
    nb_mode_total  = nb_mode_prim + nb_mode_dual
    nb_node        = ds_para%ds_empi_prim%nb_node
    nb_cmp_prim    = ds_para%ds_empi_prim%nb_cmp
    nb_cmp_dual    = ds_para%ds_empi_dual%nb_cmp
!
! - Prepare working objects
!
    AS_ALLOCATE(vi = v_list_prim, size = nb_mode_prim)
    AS_ALLOCATE(vi = v_list_dual, size = nb_mode_dual)
    AS_ALLOCATE(vi = v_list_unio, size = nb_mode_total)
!
! - Application of DEIM
!    
    call ddr_comp(ds_para%ds_empi_prim, v_list_prim)
    call ddr_comp(ds_para%ds_empi_dual, v_list_dual)
!
! - Finding list of nodes from DEIM
!    
    if (nb_cmp_prim .ne. 1) then
        v_list_prim = v_list_prim/nb_cmp_prim + 1
    endif
    v_list_dual = (v_list_dual-1)/nb_cmp_dual + 1
!
! - Assembling two lists to find a list of interpolated points
!
    call utlisi('UNION', v_list_prim, nb_mode_prim, v_list_dual, nb_mode_dual,&
                v_list_unio, nb_mode_total, nb_node_rid)
    AS_ALLOCATE(vi = v_list_rid , size = nb_node_rid)
    do i_node_rid = 1, nb_node_rid
        v_list_rid(i_node_rid) = v_list_unio(i_node_rid)
    enddo
    if (niv .ge. 2) then
        call utmess('I', 'ROM4_25', si = nb_node_rid)
    endif
!
! - Create RID on the mesh from list
!
    call ddr_crid(mesh, nb_node_rid, v_list_rid, grelem_rid, grnode_int)
!
! - Clean
!
    AS_DEALLOCATE(vi=v_list_prim)
    AS_DEALLOCATE(vi=v_list_dual)
    AS_DEALLOCATE(vi=v_list_unio)
    AS_DEALLOCATE(vi=v_list_rid)
!
end subroutine
