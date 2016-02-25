subroutine InitEnergy(result, ds_energy)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/CreateTable.h"
#include "asterfort/SetTablePara.h"
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
    character(len=8), intent(in) :: result
    type(NL_DS_Energy), intent(inout) :: ds_energy
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Energy management
!
! Initializations for energy
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! IO  ds_energy        : datastructure for energy management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_col
    character(len=24) :: col_name
    type(NL_DS_Table) :: table
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... Initializations for energy'
    endif
!
! - Get table
!
    table = ds_energy%table
!
! - Activate columns
!
    do i_col = 1, table%nb_cols
        col_name = table%cols(i_col)%name
        if (col_name.eq.'NUME_REUSE') then
            table%l_cols_acti(i_col) = .true.
        elseif (col_name.eq.'INST      ') then
            table%l_cols_acti(i_col) = .true.
        else
            if (ds_energy%l_comp) then
                table%l_cols_acti(i_col) = .true.
            endif
        endif
    end do
!
! - Create list of parameters
!
    call SetTablePara(table)
!
! - Create table in results datastructure
!
    table%table_type = 'PARA_CALC'
    call CreateTable(result, table)
!
! - Save table
!
    ds_energy%table  = table
!
end subroutine
