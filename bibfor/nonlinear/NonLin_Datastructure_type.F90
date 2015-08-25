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
! - Type: row for printing tables (convergence table for instance)
!
    type NL_DS_Row
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
    end type NL_DS_Row
!
! - Type: table (convergence table for instance)
!
    type NL_DS_Table
        integer :: nb_rows
        integer :: nb_rows_maxi = 37
        aster_logical :: l_csv
        integer :: unit_csv
        type(NL_DS_Row) :: rows(37)
        aster_logical :: l_rows_acti(37)
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
end module NonLin_Datastructure_type
