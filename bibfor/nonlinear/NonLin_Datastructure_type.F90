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
    end type NL_DS_AlgoPara
!
end module
