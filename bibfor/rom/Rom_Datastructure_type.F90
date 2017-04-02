module Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
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
! ----- Number of snapshots when created base
        integer           :: nb_snap
! ----- Datastructure for lineic base numbering
        type(ROM_DS_LineicNumb) :: ds_lineic
    end type ROM_DS_Empi
!
! - Parameters for REST_REDUIT_COMPLET operator
!
    type ROM_DS_ParaRRC
! ----- Phenomenon
        character(len=16) :: type_resu
! ----- Number of time steps
        integer           :: nb_store
! ----- Reduced results datastructure to read
        character(len=8)  :: result_rom
! ----- Model for reduced model
        character(len=8)  :: model_rom
! ----- Complete results datastructure to create
        character(len=8)  :: result_dom
! ----- Model for complete model
        character(len=8)  :: model_dom
! ----- Datastructure for empiric modes (primal)
        type(ROM_DS_Empi) :: ds_empi_prim
! ----- Datastructure for empiric modes (dual)
        type(ROM_DS_Empi) :: ds_empi_dual
! ----- Table for reduced coordinates
        character(len=24) :: tabl_name
        character(len=24) :: coor_redu
    end type ROM_DS_ParaRRC
!
! - Parameters for DEFI_BASE_REDUITE operator (POD)
!
    type ROM_DS_ParaDBR_POD
! ----- Name of result datastructures to read
        character(len=8)        :: result_in
! ----- Name of field for read (NOM_CHAM)
        character(len=24)       :: field_type
! ----- Type of reduced base
        character(len=8)        :: base_type
! ----- Direction of the linear model
        character(len=8)        :: axe_line
! ----- First section of the linear model
        character(len=24)       :: surf_num
! ----- Tolerance for SVD
        real(kind=8)            :: tole_svd
! ----- Tolerance for incremental POD
        real(kind=8)            :: tole_incr
! ----- Datastructure for snapshot selection
        type(ROM_DS_Snap)       :: ds_snap
! ----- Name of table to save reduced coordinates
        character(len=19)       :: tabl_name
    end type ROM_DS_ParaDBR_POD
!
! - Parameters for definition of multiparametric reduced problem
!
    type ROM_DS_MultiPara
! ----- Type of system to solve
        character(len=1)         :: syst_type
! ----- Matrix
        integer                  :: nb_matr
        character(len=8)         :: matr_name(8)
        character(len=1)         :: matr_type(8)
        aster_logical            :: l_coefm_cplx(8)
        aster_logical            :: l_coefm_real(8)
        complex(kind=8)          :: coefm_cplx(8)
        real(kind=8)             :: coefm_real(8)
! ----- Second member
        character(len=8)         :: vect_name
        character(len=1)         :: vect_type
        aster_logical            :: l_coefv_cplx
        aster_logical            :: l_coefv_real
        complex(kind=8)          :: coefv_cplx
        real(kind=8)             :: coefv_real
! ----- Products
        character(len=24)        :: prod_mode(8)
    end type ROM_DS_MultiPara
!
! - Parameters for DEFI_BASE_REDUITE operator (RB)
!
    type ROM_DS_ParaDBR_RB
! ----- Datastructure for solver's parameters
        character(len=19)        :: solver
! ----- Parameters for solving system
        character(len=1)         :: syst_matr_type
        character(len=1)         :: syst_2mbr_type
        character(len=19)        :: syst_matr
        character(len=19)        :: syst_2mbr
        character(len=19)        :: syst_solu
        character(len=19)        :: vect_zero
! ----- Datastructure for multiparametric reduced problem
        type(ROM_DS_MultiPara)   :: ds_multipara
    end type ROM_DS_ParaDBR_RB
!
! - Parameters for DEFI_BASE_REDUITE operator
!
    type ROM_DS_ParaDBR
! ----- Type of operation (POD, POD_INCR, GREEDY, ...)
        character(len=16)        :: operation
! ----- Name of empiric base to save
        character(len=8)         :: result_out
! ----- Maximum number of modes
        integer                  :: nb_mode_maxi
! ----- Parameters for POD/POD_INCR method
        type(ROM_DS_ParaDBR_POD) :: para_pod
! ----- Parameters for RB method
        type(ROM_DS_ParaDBR_RB ) :: para_rb
! ----- Datastructure for empiric modes
        type(ROM_DS_Empi)        :: ds_empi
! ----- If operator is "reuse"
        aster_logical            :: l_reuse
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
