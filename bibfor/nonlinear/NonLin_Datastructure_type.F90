module NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
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
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
! Non-Linear operators
!
! Define types for datastructures
!
! --------------------------------------------------------------------------------------------------
!

!
! - Type: column for printing tables (convergence table for instance)
! 
    type NL_DS_Col
        aster_logical :: l_vale_affe
        aster_logical :: l_vale_inte
        aster_logical :: l_vale_real
        aster_logical :: l_vale_strg
        integer :: vale_inte
        integer :: width
        real(kind=8) :: vale_real
        character(len=24) :: vale_strg
        character(len=9) :: name
        character(len=16) :: title(3)
        character(len=1) :: mark
    end type NL_DS_Col
!
! - Type: table (convergence table for instance)
! 
    type NL_DS_Table
        integer :: nb_cols
        integer :: nb_cols_maxi = 37
        aster_logical :: l_csv
        integer :: unit_csv
        type(NL_DS_Col) :: cols(37)
        aster_logical :: l_cols_acti(37)
        integer :: width
        integer :: title_height
        character(len=255) :: sep_line
    end type NL_DS_Table
!
! - Type: print
! 
    type NL_DS_Print
        aster_logical :: l_print
        type(NL_DS_Table) :: table_cvg
        aster_logical :: l_info_resi
        aster_logical :: l_info_time
        aster_logical :: l_tcvg_csv
        integer :: tcvg_unit
        integer :: reac_print
        character(len=255) :: sep_line
    end type NL_DS_Print
!
! - Type: residuals
! 
    type NL_DS_Resi
        character(len=16) :: type
        character(len=16) :: col_name
        character(len=16) :: col_name_locus
        character(len=16) :: event_type
        real(kind=8)      :: vale_calc
        character(len=16) :: locus_calc
        real(kind=8)      :: user_para
        aster_logical     :: l_conv
    end type NL_DS_Resi
!
! - Type: reference residuals
! 
    type NL_DS_RefeResi
        character(len=16) :: type
        real(kind=8)      :: user_para
        character(len=8)  :: cmp_name
    end type NL_DS_RefeResi
!
! - Type: convergence management
! 
    type NL_DS_Conv
        integer :: nb_resi
        integer :: nb_resi_maxi = 6
        type(NL_DS_Resi)     :: list_resi(6)
        aster_logical        :: l_resi_test(6)
        integer :: nb_refe
        integer :: nb_refe_maxi = 11
        type(NL_DS_RefeResi) :: list_refe(11)
        aster_logical        :: l_refe_test(11)
        integer :: iter_glob_maxi
        integer :: iter_glob_elas
        aster_logical :: l_stop
        aster_logical :: l_iter_elas
        real(kind=8)  :: swap_trig
        real(kind=8)  :: line_sear_coef
        integer       :: line_sear_iter
    end type NL_DS_Conv
!
! - Type: Line search parameters
! 
    type NL_DS_LineSearch
        character(len=16) :: method
        real(kind=8)      :: resi_rela
        integer           :: iter_maxi
        real(kind=8)      :: rho_mini
        real(kind=8)      :: rho_maxi
        real(kind=8)      :: rho_excl
    end type NL_DS_LineSearch
!
! - Type: algorithm parameters
! 
    type NL_DS_AlgoPara
        character(len=16)      :: method
        character(len=16)      :: matrix_pred
        character(len=16)      :: matrix_corr
        integer                :: reac_incr
        integer                :: reac_iter
        real(kind=8)           :: pas_mini_elas
        integer                :: reac_iter_elas
        aster_logical          :: l_line_search
        type(NL_DS_LineSearch) :: line_search
        aster_logical          :: l_pilotage
        aster_logical          :: l_dyna
        character(len=8)       :: result_prev_disp
        aster_logical          :: l_matr_rigi_syme
    end type NL_DS_AlgoPara
!
! - Type: fields for input/output management
!
!     type            Name of field (type) in results datastructure
!     gran_name       Type of GRANDEUR
!     field_read      Name of field read in ETAT_INIT
!     disc_type       Spatial discretization of field (ELEM, ELGA, ELNO)
!     init_keyw       Keyword for ETAT_INIT
!     obsv_keyw       Keyword for OBSERVATION
!     l_read_init     Field can been read for initial state
!     l_store         Field can been store (ARCHIVAGE)
!     l_obsv          Field can been observed (OBSERVATION)
!     algo_name       Name of field in algorithm
!     init_name       Name of field for initial state (ETAT_INIT)
!     init_type       State of field during initialization
!                       ZERO: field for zero field (given by init_name)
!                       RESU: field from result datastructure
!                       READ: field from ETAT_INIT field by field
    type NL_DS_Field
        character(len=16) :: type           
        character(len=8)  :: gran_name
        character(len=8)  :: field_read
        character(len=4)  :: disc_type
        character(len=8)  :: init_keyw
        character(len=16) :: obsv_keyw
        aster_logical     :: l_read_init
        aster_logical     :: l_store
        aster_logical     :: l_obsv
        character(len=24) :: algo_name
        character(len=24) :: init_name
        character(len=4)  :: init_type                                                              
    end type NL_DS_Field
!
! - Type: input/output management
! 
    type NL_DS_InOut
        character(len=8)  :: result
        integer           :: nb_field
        integer           :: nb_field_maxi = 21
        type(NL_DS_Field) :: field(21)
        character(len=8)  :: stin_evol
        aster_logical     :: l_stin_evol
        aster_logical     :: l_field_acti(21)
        aster_logical     :: l_field_read(21)
        aster_logical     :: l_state_init
        aster_logical     :: l_reuse
        integer           :: didi_nume
        character(len=8)  :: criterion
        real(kind=8)      :: precision
        real(kind=8)      :: user_time
        aster_logical     :: l_user_time
        integer           :: user_nume
        aster_logical     :: l_user_nume
        real(kind=8)      :: stin_time
        aster_logical     :: l_stin_time
        real(kind=8)      :: init_time
        integer           :: init_nume
        character(len=19) :: list_load_resu
        aster_logical     :: l_init_stat
        aster_logical     :: l_init_vale
        real(kind=8)      :: temp_init
    end type NL_DS_InOut
!
! - Type: loop management
! 
    type NL_DS_Loop
        character(len=4)  :: type
        integer           :: counter
        aster_logical     :: conv
    end type NL_DS_Loop 
!
! - Type: pairing management
! 
    type NL_DS_Pairing

    end type NL_DS_Pairing
!
! - Type: contact management
! 
    type NL_DS_Contact
        aster_logical     :: l_contact
        aster_logical     :: l_meca_cont
        aster_logical     :: l_meca_unil
        character(len=8)  :: sdcont
        character(len=24) :: sdcont_defi
        character(len=24) :: sdcont_solv
        character(len=24) :: sdunil_defi
        character(len=24) :: sdunil_solv
        aster_logical     :: l_form_cont
        aster_logical     :: l_form_disc
        aster_logical     :: l_form_xfem
        aster_logical     :: l_form_lac
! ----- Name of <LIGREL> for slave elements (create in DEFI_CONTACT)
        character(len=8)  :: ligrel_elem_slav
        aster_logical     :: l_elem_slav
! ----- Name of <LIGREL> for contact elements (create in MECA_NON_LINE)
        character(len=19) :: ligrel_elem_cont
        aster_logical     :: l_elem_cont
! ----- Identity relations between dof (XFEM with ELIM_ARETE or LAC method)
        aster_logical     :: l_iden_rela
        character(len=24) :: iden_rela
! ----- Relations between dof (QUAD8 in discrete methods or XFEM, create in DEFI_CONTACT)
        aster_logical     :: l_dof_rela
        character(len=8)  :: ligrel_dof_rela
! ----- Name of <CHELEM> - Input field
        character(len=19) :: field_input
! ----- NUME_DOF for discrete friction methods 
        character(len=14) :: nume_dof_frot
! ----- Field for CONT_NODE 
        character(len=19) :: field_cont_node
        character(len=19) :: fields_cont_node
        character(len=19) :: field_cont_perc    
! ----- Loops
        integer           :: nb_loop
        integer           :: nb_loop_maxi = 3
        type(NL_DS_Loop)  :: loop(3)

    end type NL_DS_Contact
!
end module
