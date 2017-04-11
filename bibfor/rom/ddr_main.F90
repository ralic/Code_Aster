subroutine ddr_main(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/ddr_comp.h"
#include "asterfort/ddr_crid.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/ddr_prep.h"
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
    integer :: nb_node_rid, nb_mode_prim, nb_mode_dual
    integer, pointer :: v_list_prim(:) => null()
    integer, pointer :: v_list_dual(:) => null()
    integer, pointer :: v_list_rid(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
! - Get parameters
!
    nb_mode_prim = ds_para%ds_empi_prim%nb_mode
    nb_mode_dual = ds_para%ds_empi_dual%nb_mode
!
! - Prepare working objects
!
    AS_ALLOCATE(vi = v_list_prim, size = nb_mode_prim)
    AS_ALLOCATE(vi = v_list_dual, size = nb_mode_dual)
!
! - Application of DEIM
!    
    call ddr_comp(ds_para%ds_empi_prim, v_list_prim)
    call ddr_comp(ds_para%ds_empi_dual, v_list_dual)
!
! - Prepare list of nodes in RID
!
    call ddr_prep(ds_para, v_list_prim, v_list_dual, v_list_rid, nb_node_rid)
!
! - Create RID on the mesh from list
!
    call ddr_crid(ds_para, nb_node_rid, v_list_rid)
!
! - Clean
!
    AS_DEALLOCATE(vi=v_list_prim)
    AS_DEALLOCATE(vi=v_list_dual)
    AS_DEALLOCATE(vi=v_list_rid)
!
end subroutine
