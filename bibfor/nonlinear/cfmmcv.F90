subroutine cfmmcv(mesh     , model_   , nume_dof , list_func_acti, iter_newt ,&
                  nume_inst, sddyna   , ds_measure, sddisc       , &
                  sderro   , hval_incr, hval_algo, ds_print      , ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfconv.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mm_cycl_print.h"
#include "asterfort/mmbclc.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: model_
    character(len=24), intent(in) :: nume_dof
    integer, intent(in) :: list_func_acti(*)
    integer, intent(in) :: iter_newt
    integer, intent(in) :: nume_inst 
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24), intent(in) :: sderro
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! All methods - Evaluate convergence
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  iter_newt        : index of current Newton iteration
! In  nume_inst        : index of current time step
! In  sddisc           : datastructure for time discretization
! In  sddyna           : dynamic parameters datastructure
! IO  ds_measure       : datastructure for measure and statistics management
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! IO  ds_contact       : datastructure for contact management
! In  sderro           : datastructure for errors during algorithm
! In  hval_algo        : hat-variable for algorithms fields
! IO  ds_print         : datastructure for printing parameters
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_disc=.false._1, l_cont_cont=.false._1, l_newt_cont=.false._1
    aster_logical :: loop_cont_conv=.false._1
    character(len=8) :: model=' '
    real(kind=8) :: r8bid=0.d0, loop_cont_vale
    integer :: loop_cont_vali
!
! --------------------------------------------------------------------------------------------------
!
    model = model_(1:8)
!
! - Get parameters
!
    l_cont_disc = isfonc(list_func_acti,'CONT_DISCRET')
    l_cont_cont = isfonc(list_func_acti,'CONT_CONTINU')
    l_newt_cont = isfonc(list_func_acti,'CONT_NEWTON')
!
! - Values in convergence table: not affected
!
    call nmimck(ds_print, 'BOUC_NOEU', ' '  , .false._1)
    call nmimcr(ds_print, 'BOUC_VALE', r8bid, .false._1)
!
! - Convergence for contact discrete methods
!
    if (l_cont_disc) then
        call cfconv(mesh      , ds_measure, sderro, hval_algo, ds_print,&
                    ds_contact)
    endif
!
! - Applying generalized Newton method at Newton's iteration
!
    if (l_newt_cont) then
        call mmbclc(mesh  , model     , nume_dof  , iter_newt, nume_inst,&
                    sddisc, sddyna    , ds_measure, hval_incr,&
                    hval_algo, ds_contact)
        call mmbouc(ds_contact, 'Cont', 'Get_Vale'      , loop_vale_  = loop_cont_vale)
        call mmbouc(ds_contact, 'Cont', 'Is_Convergence', loop_state_ = loop_cont_conv)
        loop_cont_vali = nint(loop_cont_vale)
        if (loop_cont_conv) then
            call nmcrel(sderro, 'DIVE_CTCC', .false._1)
        else
            call nmcrel(sderro, 'DIVE_CTCC', .true._1)
        endif
        call nmimci(ds_print, 'CONT_NEWT', loop_cont_vali, .true._1)
    endif
!
! - Cycling informations printing in convergence table
!
    if (l_cont_cont) then
        call mm_cycl_print(ds_print, ds_measure)
    endif
!
end subroutine
