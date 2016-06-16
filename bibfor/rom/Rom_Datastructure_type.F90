module Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Define types for datastructures
!
! --------------------------------------------------------------------------------------------------
!
! - Datastructure to select snapshots
!
    type ROM_DS_Snap
! ----- Results datastructure where snapshots are selected
        character(len=8)  :: result
! ----- Number of snapshots
        integer           :: nb_snap
! ----- Name of JEVEUX object for list of snapshots 
        character(len=24) :: list_snap
    end type ROM_DS_Snap
!
! - Parameters for lineic base numbering
!
    type ROM_DS_LineicNumb
! ----- Number of slices
        integer           :: nb_slice
! ----- For each node => which slice ?
        integer, pointer  :: v_nume_pl(:)
! ----- For each node => which IN slice ?
        integer, pointer  :: v_nume_sf(:)
! ----- Tolerance for separating nodes
        real(kind=8)      :: tole_node
    end type ROM_DS_LineicNumb
!
! - Datastructure for empiric modes
!
    type ROM_DS_Empi
! ----- Name of empiric base to save
        character(len=8)  :: base
! ----- Type of field for (NOM_CHAM)
        character(len=24) :: field_type
! ----- A field for reference (to manipulate real field)
        character(len=24) :: field_refe
! ----- Mesh
        character(len=8)  :: mesh
! ----- Model
        character(len=8)  :: model
! ----- Type of reduced base
        character(len=8)  :: base_type
! ----- Direction of the linear model
        character(len=8)  :: axe_line
! ----- First section of the linear model
        character(len=24) :: surf_num
! ----- Number of equations
        integer           :: nb_equa
! ----- Number of nodes
        integer           :: nb_node
! ----- Number of components by node
        integer           :: nb_cmp
! ----- Number of modes in base
        integer           :: nb_mode
! ----- Datastructure for lineic base numbering
        type(ROM_DS_LineicNumb) :: ds_lineic
    end type ROM_DS_Empi
!
! - Parameters for DEFI_BASE_REDUITE operator
!
    type ROM_DS_ParaDBR
! ----- Tolerance for SVD
        real(kind=8)            :: tole_svd
! ----- Maximum number of modes
        integer                 :: nb_mode_maxi
! ----- Datastructure for empiric modes
        type(ROM_DS_Empi)       :: ds_empi
! ----- Datastructure for snapshot selection
        type(ROM_DS_Snap)       :: ds_snap
    end type ROM_DS_ParaDBR
!
! - Parameters for DEFI_DOMAINE_REDUIT operator
!
    type ROM_DS_ParaDDR
! ----- Mesh
        character(len=8)  :: mesh
! ----- Datastructure for empiric modes (primal)
        type(ROM_DS_Empi) :: ds_empi_prim
! ----- Datastructure for empiric modes (dual)
        type(ROM_DS_Empi) :: ds_empi_dual
! ----- Name of group of elements for RID
        character(len=24) :: grelem_rid
! ----- Name of group of nodes for interface
        character(len=24) :: grnode_int
    end type ROM_DS_ParaDDR
!
! - Parameters for non_linear operator
!
    type ROM_DS_AlgoPara
! ----- Empiric modes
        type(ROM_DS_Empi) :: ds_empi
! ----- Empiric modes (on RID)
        type(ROM_DS_Empi) :: ds_empi_rid
! ----- Pointer to list of equations for interface nodes
        integer, pointer  :: v_equa_int(:)
! ----- Flag for reduced model
        aster_logical     :: l_rom
! ----- Flag for hyper-reduced model
        aster_logical     :: l_hrom
! ----- Name of GROUP_NO
        character(len=24) :: grnode_int
! ----- Table for reduced coordinates
        character(len=24) :: tabl_name
! ----- Object to save reduced coordinates
        character(len=24) :: gamma
    end type ROM_DS_AlgoPara
!     
end module
