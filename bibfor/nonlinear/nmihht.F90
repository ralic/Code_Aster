subroutine nmihht(model      , nume_dof , mate     , compor        , comp_para  ,&
                  cara_elem  , list_load, varc_refe, list_func_acti, sdstat     ,&
                  sddyna     , sdtime   , sdnume   , sdcont_defi   , sdcont_solv,&
                  sdunil_solv, hval_incr, sddisc   , hval_algo     , hval_veasse,&
                  result)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmchht.h"
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
! aslint: disable=W1504
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: comp_para
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: varc_refe
    integer, intent(in) :: list_func_acti(*)
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sdtime
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sdnume
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
    character(len=24), intent(in) :: sdunil_solv
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_veasse(*)
    character(len=8), intent(in) :: result
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Initialiasations for multi-step schemes
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  compor           : name of comportment definition (field)
! In  comp_para        : parameters for comportment (field)
! In  nume_dof         : name of numbering (NUME_DDL)
! In  list_load        : name of datastructure for list of loads
! In  varc_refe        : name of reference command variables vector
! In  list_func_acti   : list of active functionnalities
! In  sdstat           : datastructure for statistics
! In  sddyna           : dynamic parameters datastructure
! In  sdtime           : datastructure for timers management
! In  sddisc           : datastructure for time discretization
! In  sdnume           : datastructure for dof positions
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact definition datastructure for solving
! In  sdunil_defi      : name of unilateral condition datastructure (from DEFI_CONTACT)
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_veasse      : hat-variable for vectors (node fields)
! In  result           : name of result datastructure (EVOL_NOLI)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_init_state, l_reuse
!
! --------------------------------------------------------------------------------------------------
!

!
! - Does ETAT_INIT (initial state) exist ?
!
    l_init_state = isfonc(list_func_acti,'ETAT_INIT')
!
! - REUSE ?
!
    l_reuse      = isfonc(list_func_acti,'REUSE')
!
! - Compute previous second member for multi-step schemes
!
    if (l_reuse.or.l_init_state) then
        call nmchht(model      , mate       , cara_elem, compor        , comp_para  ,&
                    list_load  , nume_dof   , varc_refe, list_func_acti, sdstat     ,&
                    sddyna     , sdtime     , sddisc   , sdnume        , sdcont_defi,&
                    sdcont_solv, sdunil_solv, hval_incr, hval_algo     , hval_veasse,&
                    result)
    endif

end subroutine
