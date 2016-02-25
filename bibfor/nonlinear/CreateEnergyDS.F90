subroutine CreateEnergyDS(ds_energy)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/ismaem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/CreateVoidColumn.h"
#include "asterfort/CreateVoidTable.h"
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
    type(NL_DS_Energy), intent(out) :: ds_energy
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Energy management
!
! Create energy management datastructure
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_energy        : datastructure for energy management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_col
    integer, parameter :: nb_col_defi = 8
    type(NL_DS_Table) :: table
    type(NL_DS_Column) :: column
!
    character(len=24), parameter :: cols_name(nb_col_defi) = (/&
                    'NUME_REUSE','INST      ','TRAV_EXT  ',&
                    'ENER_CIN  ','ENER_TOT  ','TRAV_AMOR ',&
                    'TRAV_LIAI ','DISS_SCH  '/)
!
    character(len=3), parameter :: cols_type(nb_col_defi) = (/&
                    'I  ','R  ','R  ',&
                    'R  ','R  ','R  ',&
                    'R  ','R  '/)
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Create energy management datastructure'
    endif
!
! - Create table
!
    call CreateVoidTable(table)
    table%table_type     = 'PARA_CALC'
    table%nb_cols        = nb_col_defi
    ASSERT(table%nb_cols .le. table%nb_cols_maxi)
    do i_col = 1, nb_col_defi
        call CreateVoidColumn(column)
        column%name        = cols_name(i_col)
        column%l_vale_inte = .false._1
        column%l_vale_real = .false._1
        column%vale_inte   = ismaem()
        column%vale_real   = r8vide()
        if (cols_type(i_col) .eq. 'I') then
            column%l_vale_inte = .true._1
            column%vale_inte   = 0
        elseif (cols_type(i_col) .eq. 'R') then
            column%l_vale_real = .true._1
            column%vale_real   = 0.d0
        else
            ASSERT(.false.)
        endif
        table%cols(i_col)      = column
        table%indx_vale(i_col) = i_col
    end do
!
! - Set main parameters
!
    ds_energy%l_comp   = .false._1
    ds_energy%command  = ' '
    ds_energy%table    = table 
!
end subroutine
