subroutine CreateVoidTable(table)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/CreateVoidColumn.h"
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
    type(NL_DS_Table), intent(out) :: table
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Table management
!
! Create void table
!
! --------------------------------------------------------------------------------------------------
!
! Out table            : table
!
! --------------------------------------------------------------------------------------------------
!
    type(NL_DS_Column) :: column_void
    integer :: nb_cols_maxi
!
! --------------------------------------------------------------------------------------------------
!
    call CreateVoidColumn(column_void)
    nb_cols_maxi = table%nb_cols_maxi
!
    table%result                      = ' '
    table%table_name                  = ' '
    table%table_type                  = ' '
    table%nb_cols                     = 0
    table%cols(1:nb_cols_maxi)        = column_void
    table%l_cols_acti(1:nb_cols_maxi) = .false._1
    table%width                       = 0
    table%title_height                = 0
    table%sep_line                    = ' '
    table%l_csv                       = .false._1
    table%unit_csv                    = 0  
    table%nb_para                     = 0
    table%list_para(1:nb_cols_maxi)   = ' '
    table%type_para(1:nb_cols_maxi)   = ' '
    table%nb_para_inte                = 0
    table%nb_para_real                = 0
    table%nb_para_cplx                = 0
    table%nb_para_strg                = 0
    table%indx_vale(1:nb_cols_maxi)   = 0
!
end subroutine
