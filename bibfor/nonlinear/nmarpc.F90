subroutine nmarpc(ds_energy, nume_reuse, time_curr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/tbajli.h"
#include "asterfort/GetEnergy.h"
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
    type(NL_DS_Energy), intent(in) :: ds_energy
    integer, intent(in) :: nume_reuse
    real(kind=8), intent(in) :: time_curr
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Storing results
!
! Save energy parameters in output table
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_energy        : datastructure for energy management
! In  nume_reuse       : index for reuse rsults datastructure
! In  time_curr        : current time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cols, i_col, i_para_real
    integer :: vali(1)
    character(len=8) :: k8bid = ' '
    complex(kind=8), parameter :: c16bid =(0.d0,0.d0)
    real(kind=8) :: valr(7), vale_r
    type(NL_DS_Table) :: table
    type(NL_DS_Column) :: column
    aster_logical :: l_acti
!
! --------------------------------------------------------------------------------------------------
!
    table    = ds_energy%table
!
! - Get table parameters
!
    nb_cols  = table%nb_cols
!
! - Set values
!
    i_para_real = 0
    do i_col = 1, nb_cols
        column = table%cols(i_col)
        l_acti = table%l_cols_acti(i_col)
        if (l_acti) then
            if (column%name .eq. 'NUME_REUSE') then
                vali(1)           = nume_reuse
            elseif (column%name .eq. 'INST') then
                i_para_real       = i_para_real + 1
                valr(i_para_real) = time_curr
            else
                vale_r            = table%cols(i_col)%vale_real
                i_para_real       = i_para_real + 1
                valr(i_para_real) = vale_r
            endif
        endif
    end do
!
! - Add line in table
!
    call tbajli(table%table_name, table%nb_para, table%list_para,&
                vali, valr, [c16bid], k8bid, 0)
!
end subroutine
