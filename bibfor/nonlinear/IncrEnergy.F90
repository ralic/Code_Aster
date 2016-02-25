subroutine IncrEnergy(ds_energy, energy_type_, vale_r)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
    type(NL_DS_Energy), intent(inout) :: ds_energy
    character(len=*), intent(in) :: energy_type_
    real(kind=8), intent(in) :: vale_r
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Energy management
!
! Add energy
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_energy        : datastructure for energy management
! In  type_energy      : type of energy
! In  vale_r           : value of energy to increment - Out: current value
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cols, i_col, indx_col
    character(len=24) :: energy_type
    type(NL_DS_Table) :: table
!
! --------------------------------------------------------------------------------------------------
!
    energy_type = energy_type_
!
! - Get table
!
    table   = ds_energy%table
    nb_cols = table%nb_cols
!
! - Get column
!
    indx_col = 0
    do i_col = 1, nb_cols
        if (table%cols(i_col)%name .eq. energy_type) then
            ASSERT(indx_col .eq. 0)
            indx_col = i_col
        endif
    end do
    ASSERT(indx_col .ne. 0)
!
! - Add energy
!
    ASSERT(table%cols(indx_col)%l_vale_real)
    table%cols(indx_col)%vale_real = table%cols(indx_col)%vale_real + vale_r
!
! - Set table
!
    ds_energy%table = table
!
end subroutine
