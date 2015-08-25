subroutine nmimin(list_func_acti, sddisc, sdsuiv, nume_inst, ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/InitTableCvg.h"
#include "asterfort/InitPrint.h"
#include "asterfort/nmimpt.h"
#include "asterfort/nmimpx.h"
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
    integer, intent(in) :: list_func_acti(*)
    character(len=19), intent(in) :: sddisc
    character(len=24), intent(in) :: sdsuiv
    integer, intent(in) :: nume_inst
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Initializations for new step time
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  sddisc           : name of datastructure for time discretization
! In  sdsuiv           : datastructure for DOF monitoring
! In  nume_inst        : index of current time step
! IO  ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_print
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations for convergence table
!
    call InitTableCvg(list_func_acti, sdsuiv, ds_print)
!
! - Print or not ?
!
    l_print = mod(nume_inst+1,ds_print%reac_print) .eq. 0
    ds_print%l_print = l_print
!
! - Print separator line
!
    if (l_print) then
        call nmimpx(ds_print)
    endif
!
! - Print head of convergence table
!
    if (l_print) then
        call nmimpt(nume_inst, sddisc, ds_print)
    endif
!
end subroutine
