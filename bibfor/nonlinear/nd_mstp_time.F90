subroutine nd_mstp_time(ds_inout, list_func_acti, time_prev_step, l_comp_mstp)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/isfonc.h"
#include "asterc/r8vide.h"
#include "asterfort/getvid.h"
#include "asterfort/nmdoin.h"
#include "asterfort/utmess.h"
#include "asterfort/rs_getlast.h"
#include "asterfort/rsadpa.h"
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
    type(NL_DS_InOut), intent(in) :: ds_inout
    integer, intent(in) :: list_func_acti(*)
    real(kind=8), intent(out) :: time_prev_step
    aster_logical, intent(out) :: l_comp_mstp
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Dynamic
!
! Get previous time for multi-step schemes
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
! In  list_func_acti   : list of active functionnalities
! Out time_prev_step   : previous time for multi-step schemes
! Out l_comp_mstp      : .true. if compute second member
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_reuse, l_init_state, l_stin_evol
    integer :: init_nume, nume_prev_step, nume_last
    integer :: jv_para
    character(len=24) :: stin_evol
    character(len=8) :: result
!
! --------------------------------------------------------------------------------------------------
!
    time_prev_step = r8vide()
    l_comp_mstp    = .false.
    result         = ds_inout%result
    init_nume      = ds_inout%init_nume
!
! - Does ETAT_INIT (initial state) exist ?
!
    l_init_state = isfonc(list_func_acti,'ETAT_INIT')
!
! - Reuse previous results ?
!
    l_reuse      = isfonc(list_func_acti,'REUSE')
!
! - Get name of result datastructure in ETAT_INIT
!
    l_stin_evol  = ds_inout%l_stin_evol
    stin_evol    = ds_inout%stin_evol 
!
! - Initial state: get time if possible
!
    if (l_init_state) then
!
        nume_prev_step = init_nume - 1
!
! ----- Get previous time
!
        if (l_stin_evol) then
            if (nume_prev_step.le.0) then
                call utmess('I','DYNAMIQUE_50')
            else
                call rsadpa(stin_evol, 'L', 1, 'INST_PREC', nume_prev_step,&
                            0, sjv=jv_para, istop = 0)
                time_prev_step = zr(jv_para) 
                if (time_prev_step .eq. r8vide()) then
                    call utmess('I','DYNAMIQUE_51')
                else
                    l_comp_mstp    = .true.
                endif
            endif
        else
            call utmess('I','DYNAMIQUE_53')
        endif
    endif
!
! - Reuse old results: get time if possible
!
    if (l_reuse) then
        call rs_getlast(result, nume_last)
        call rsadpa(result, 'L', 1, 'INST_PREC', nume_last,&
                    0, sjv=jv_para)
        time_prev_step = zr(jv_para) 
        if (time_prev_step .eq. r8vide()) then
            call utmess('I','DYNAMIQUE_51')
        else
            l_comp_mstp    = .true.
        endif
    endif
!
end subroutine
