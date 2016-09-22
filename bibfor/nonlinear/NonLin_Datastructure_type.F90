module NonLin_Datastructure_type
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
! Non-Linear operators
!
! Define types for datastructures
!
! --------------------------------------------------------------------------------------------------
!

!
! - Type: column for table
! 
    type NL_DS_Column
        character(len=16)  :: name
        character(len=16)  :: title(3)
        character(len=1)   :: mark
        aster_logical      :: l_vale_affe
        aster_logical      :: l_vale_inte
        aster_logical      :: l_vale_real
        aster_logical      :: l_vale_cplx
        aster_logical      :: l_vale_strg
        integer            :: vale_inte
        real(kind=8)       :: vale_real
        complex(kind=8)    :: vale_cplx
        character(len=16)  :: vale_strg
    end type NL_DS_Column
!
! - Type: table 
! 
    type NL_DS_Table
! ----- Name of result datastructure
        character(len=8)       :: result
        character(len=19)      :: table_name
        character(len=24)      :: table_type
! ----- Number of active columns
        integer                :: nb_cols
! ----- Maximum number of columns in table
        integer                :: nb_cols_maxi = 37
! ----- List of columns in table
        type(NL_DS_Column)     :: cols(37)
! ----- List of _active_ columns in table
        aster_logical          :: l_cols_acti(37)
! ----- Total width of table
        integer                :: width
! ----- eNumber of lines for title
        integer                :: title_height
! ----- Separation line
        character(len=512)     :: sep_line
! ----- Flag for outside file (CSV)
        aster_logical          :: l_csv
! ----- Logical unit for outside file (CSV)
        integer                :: unit_csv
! ----- List of parameters (for table definition)
        integer                :: nb_para
        integer                :: nb_para_inte
        integer                :: nb_para_real
        integer                :: nb_para_cplx
        integer                :: nb_para_strg
        character(len=24)      :: list_para(37)
        character(len=3)       :: type_para(37)
! ----- Index to values
        integer                :: indx_vale(37)
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
        character(len=512) :: sep_line
    end type NL_DS_Print
!
! - Type: residuals
! 
    type NL_DS_Resi
        character(len=16) :: type
        character(len=24) :: col_name
        character(len=24) :: col_name_locus
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
        aster_logical     :: l_temp_nonl
        integer           :: nb_field
        integer           :: nb_field_maxi = 22
        type(NL_DS_Field) :: field(22)
        character(len=8)  :: stin_evol
        aster_logical     :: l_stin_evol
        aster_logical     :: l_field_acti(22)
        aster_logical     :: l_field_read(22)
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
        aster_logical     :: error
        real(kind=8)      :: vale_calc
        character(len=16) :: locus_calc
    end type NL_DS_Loop
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
! ----- Fields for CONT_NODE 
        character(len=19) :: field_cont_node
        character(len=19) :: fields_cont_node
        character(len=19) :: field_cont_perc
! ----- Fields for CONT_ELEM 
        character(len=19) :: field_cont_elem
        character(len=19) :: fields_cont_elem
! ----- Loops
        integer           :: nb_loop
        integer           :: nb_loop_maxi = 3
        type(NL_DS_Loop)  :: loop(3)
! ----- Flag for (re) numbering
        aster_logical     :: l_renumber
! ----- Geometric loop control
        real(kind=8)      :: geom_maxi
! ----- Get-off indicator
        aster_logical     :: l_getoff
! ----- First geometric loop
        aster_logical     :: l_first_geom
! ----- Flag for pairing
        aster_logical     :: l_pair
! ----- Total number of patches (for LAC method)
        integer           :: nt_patch
! ----- Total number of contact pairs
        integer           :: nb_cont_pair
    end type NL_DS_Contact
!
! - Type: timer management
! 
    type NL_DS_Timer
! ----- Type of timer
        character(len=9)  :: type
! ----- Internal name of timer
        character(len=24) :: cpu_name
! ----- Initial time
        real(kind=8)      :: time_init
    end type NL_DS_Timer
!
! - Type: device for measure
! 
    type NL_DS_Device
! ----- Type of device
        character(len=10) :: type
! ----- Name of timer
        character(len=9)  :: timer_name
! ----- Times: for Newton iteration, time step and complete computation
        real(kind=8)      :: time_iter
        real(kind=8)      :: time_step
        real(kind=8)      :: time_comp
        integer           :: time_indi_step
        integer           :: time_indi_comp
! ----- Counters: for Newton iteration, time step and complete computation
        aster_logical     :: l_count_add
        integer           :: count_iter
        integer           :: count_step
        integer           :: count_comp
        integer           :: count_indi_step
        integer           :: count_indi_comp
    end type NL_DS_Device
!
! - Type: measure and statistics management
! 
    type NL_DS_Measure
! ----- Output in table
        aster_logical      :: l_table
! ----- Table in results datastructures
        type(NL_DS_Table)  :: table
        integer            :: indx_cols(2*23)
! ----- List of timers
        integer            :: nb_timer
        integer            :: nb_timer_maxi = 7
        type(NL_DS_Timer)  :: timer(7)
! ----- List of devices
        integer            :: nb_device
        integer            :: nb_device_maxi = 23
        type(NL_DS_Device) :: device(23)
        integer            :: nb_device_acti
        aster_logical      :: l_device_acti(23)
! ----- Some special times
        real(kind=8)       :: store_mean_time
        real(kind=8)       :: iter_mean_time
        real(kind=8)       :: step_mean_time
        real(kind=8)       :: iter_remain_time
        real(kind=8)       :: step_remain_time
    end type NL_DS_Measure
!
! - Type: energy management
! 
    type NL_DS_Energy
! ----- Flag for energy computation
        aster_logical         :: l_comp
! ----- Command (MECA_NON_LINE or DYNA_VIBRA)
        character(len=16)     :: command
! ----- Table in results datastructures
        type(NL_DS_Table)     :: table
    end type NL_DS_Energy
!
! - Type: for exterior comportement
! 
    type NL_DS_ComporExte
        aster_logical      :: l_umat
        aster_logical      :: l_mfront_proto
        aster_logical      :: l_mfront_offi
        character(len=255) :: subr_name
        character(len=255) :: libr_name
        character(len=16)  :: model_mfront
        integer            :: model_dim
        integer            :: nb_vari_umat
    end type NL_DS_ComporExte
!
! - Type: for comportement
! 
    type NL_DS_Compor
        character(len=16) :: rela_comp
        character(len=16) :: defo_comp
        character(len=16) :: type_comp
        character(len=16) :: type_cpla
        character(len=16) :: kit_comp(4)
        character(len=16) :: mult_comp
        character(len=16) :: type_matg
        character(len=16) :: post_iter
        integer           :: nb_vari
        integer           :: nb_vari_comp(4)
        integer           :: nume_comp(4)
    end type NL_DS_Compor
!
! - Type: for preparation of comportment
! 
    type NL_DS_ComporPrep
! ----- Number of comportements
        integer                         :: nb_comp
! ----- List of comportements
        type(NL_DS_Compor), pointer     :: v_comp(:)
! ----- List of external comportements
        type(NL_DS_ComporExte), pointer :: v_exte(:)
    end type NL_DS_ComporPrep
!
! - Type: pointer to external constitutive laws
! 
    type NL_DS_ComporPointer
        integer      ::  nbvarext
        integer      ::  namevarext
        integer      ::  fct_ldc
        integer      ::  matprop
        integer      ::  nbprop
    end type NL_DS_ComporPointer
!
! - Type: for parameters for constitutive laws
! 
    type NL_DS_ComporPara
        integer      :: type_matr_t
        real(kind=8) :: parm_alpha
        real(kind=8) :: parm_theta
        integer      :: iter_inte_pas
        real(kind=8) :: vale_pert_rela
        real(kind=8) :: resi_deborst_max
        integer      :: iter_deborst_max
        real(kind=8) :: seuil
        real(kind=8) :: amplitude
        real(kind=8) :: taux_retour
        integer      :: post_iter
        integer      :: post_incr
        character(len=16)         :: rela_comp
        character(len=16)         :: algo_inte
        type(NL_DS_ComporPointer) :: c_pointer
        type(NL_DS_ComporExte)    :: comp_exte
    end type NL_DS_ComporPara
!
! - Type: for preparation of parameters for constitutive laws
! 
    type NL_DS_ComporParaPrep
! ----- Number of comportements
        integer                         :: nb_comp
! ----- List of parameters
        type(NL_DS_ComporPara), pointer :: v_para(:)
    end type NL_DS_ComporParaPrep
!
! - Type: constitutive laws management
! 
    type NL_DS_Constitutive
! ----- Name of field for constitutive laws
        character(len=24)     :: compor
! ----- Name of field for criteria of constitutive laws
        character(len=24)     :: carcri
! ----- Name of field for constitutive laws - Special crystal
        character(len=24)     :: mult_comp
! ----- Name of field for error field from constitutive laws
        character(len=24)     :: comp_error
! ----- Flag for De Borst algorithm
        aster_logical         :: l_deborst
! ----- Flag for DIS_CHOC
        aster_logical         :: l_dis_choc
! ----- Flag for POST_INCR
        aster_logical         :: l_post_incr
! ----- Flag for large strains in tangent matrix
        aster_logical         :: l_matr_geom
    end type NL_DS_Constitutive
!
end module
