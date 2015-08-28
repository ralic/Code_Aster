subroutine CreateAlgoParaDS(ds_algopara)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_AlgoPara), intent(out) :: ds_algopara
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algorithm parameters management
!
! Create algorithm parameters datastructure
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_algopara      : datastructure for algorithm parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Create algorithm parameters datastructure'
    endif
!
! - General parameters
!
    ds_algopara%method           = ' '
    ds_algopara%matrix_pred      = ' '
    ds_algopara%matrix_corr      = ' '
    ds_algopara%reac_incr        = 0
    ds_algopara%reac_iter        = 0
    ds_algopara%pas_mini_elas    = -9999.0d0
    ds_algopara%reac_iter_elas   = 0
    ds_algopara%l_dyna           = .false._1
    ds_algopara%l_line_search    = .false._1
    ds_algopara%l_pilotage       = .false._1
    ds_algopara%result_prev_disp = ' '
!
! - Parameters for line search
!
    ds_algopara%line_search%method    = ' '
    ds_algopara%line_search%resi_rela = 0.d0
    ds_algopara%line_search%iter_maxi = 0
    ds_algopara%line_search%rho_mini  = 0.d0
    ds_algopara%line_search%rho_maxi  = 0.d0
    ds_algopara%line_search%rho_excl  = 0.d0
!
end subroutine
