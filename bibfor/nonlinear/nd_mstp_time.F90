subroutine nd_mstp_time(result, list_func_acti, time_prev_step, l_comp_mstp)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/isfonc.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/getvid.h"
#include "asterfort/nmdoin.h"
#include "asterfort/utmess.h"
#include "asterfort/rs_getlast.h"
#include "asterfort/rsadpa.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! In  result           : name of result datastructure (EVOL_NOLI)
! In  list_func_acti   : list of active functionnalities
! Out time_prev_step   : previous time for multi-step schemes
! Out l_comp_mstp      : .true. if compute second member
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywf
    aster_logical :: l_reuse, l_init_state, l_init_evol
    integer :: nocc, nume_init, nume_prev_step, nume_last
    integer :: j_inst
    real(kind=8) :: inst_init
    character(len=24) :: result_init
!
! --------------------------------------------------------------------------------------------------
!
    keywf          = 'ETAT_INIT'
    time_prev_step = r8vide()
    l_comp_mstp    = .false.
    result_init    = ' '
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
    call getvid(keywf, 'EVOL_NOLI', iocc=1, scal=result_init, nbret=nocc)
    l_init_evol = nocc .gt. 0
!
! - Initial state: get time if possible
!
    if (l_init_state) then
!
! ----- Initial storing index and time
!
        call nmdoin(result_init, l_init_evol, inst_init, nume_init)
        nume_prev_step = nume_init - 1
!
! ----- Get previous time
!
        if (l_init_evol) then
            if (nume_prev_step.le.0) then
                call utmess('I','DYNAMIQUE_50')
            else
                call rsadpa(result_init, 'L', 1, 'INST_PREC', nume_prev_step,&
                            0, sjv=j_inst, istop = 0)
                if (j_inst.eq.isnnem()) then
                    call utmess('I','DYNAMIQUE_51')
                else
                    time_prev_step = zr(j_inst) 
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
                    0, sjv=j_inst)
        if (j_inst.eq.isnnem()) then
            call utmess('I','DYNAMIQUE_51')
        else
            time_prev_step = zr(j_inst)
            l_comp_mstp    = .true.
        endif
    endif
!
end subroutine
