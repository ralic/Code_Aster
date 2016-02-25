subroutine nmihht(model    , nume_dof , mate       , compor        , comp_para,&
                  cara_elem, list_load, varc_refe  , list_func_acti, ds_measure,&
                  sddyna   , sdnume     , ds_contact    , hval_incr,&
                  sddisc   , hval_algo, hval_veasse, hval_measse   , ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmchht.h"
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
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sdnume
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    character(len=19), intent(in) :: hval_veasse(*)
    character(len=19), intent(in) :: hval_measse(*)
    type(NL_DS_InOut), intent(in) :: ds_inout
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Initializations for multi-step schemes
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
! IO  ds_measure       : datastructure for measure and statistics management
! In  sddyna           : dynamic parameters datastructure
! In  sddisc           : datastructure for time discretization
! In  sdnume           : datastructure for dof positions
! In  ds_contact       : datastructure for contact management
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  hval_veasse      : hat-variable for vectors (node fields)
! In  hval_measse      : hat-variable for matrix
! In  ds_inout         : datastructure for input/output management
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
    if (l_reuse .or. l_init_state) then
        call nmchht(model    , mate     , cara_elem  , compor        , comp_para ,&
                    list_load, nume_dof , varc_refe  , list_func_acti, ds_measure ,&
                    sddyna   , sddisc   , sdnume     , ds_contact,&
                    hval_incr, hval_algo, hval_veasse, hval_measse   , ds_inout)
    endif

end subroutine
