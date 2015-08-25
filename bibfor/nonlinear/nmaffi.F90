subroutine nmaffi(list_func_acti, sdconv, ds_print, sderro, sddisc,&
                  loop_name     )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmaffm.h"
#include "asterfort/nmerim.h"
#include "asterfort/nmevim.h"
#include "asterfort/nmimpr.h"
#include "asterfort/nmimps.h"
#include "asterfort/nmimpx.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmltev.h"
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
    character(len=24), intent(in) :: sdconv
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=24), intent(in) :: sderro
    character(len=19), intent(in) :: sddisc
    character(len=4), intent(in) :: loop_name
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print during loop
!
! --------------------------------------------------------------------------------------------------
!
! In  list_func_acti   : list of active functionnalities
! In  ds_print         : datastructure for printing parameters
! In  sdconv           : name of datastructure convergence
! In  sderro           : name of datastructure for error management (events)
! In  sddisc           : name of datastructure for time discretization
! In  loop_name        : name of loop
!                         'NEWT' - Newton loop
!                         'FIXE' - Fixed points loop
!                         'INST' - Step time loop
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_error
    aster_logical :: cvnewt, cvinst
    aster_logical :: l_line_print
    aster_logical :: l_loop_cont, l_dyna_expl
!
! --------------------------------------------------------------------------------------------------
!
    l_line_print = .false.
!
! - Active functionnalites
!
    l_loop_cont = isfonc(list_func_acti,'BOUCLE_EXTERNE')
    l_dyna_expl = isfonc(list_func_acti,'EXPLICITE')
!
! - Convergence state of loops
!
    call nmlecv(sderro, 'NEWT', cvnewt)
    call nmlecv(sderro, 'INST', cvinst)
!
! - Set marks in rows
!
    call nmaffm(sderro, ds_print, loop_name)
!
! - Is error event occurred ?
!
    call nmltev(sderro, 'ERRI', loop_name, l_error)
!
! - Print line of convergence table ?
!
    if (loop_name .eq. 'NEWT') then
        if (cvnewt) then
            if (l_loop_cont) then
                l_line_print = .false.
            else
                l_line_print = .true.
            endif
        else
            l_line_print = .true.
        endif
    else if (loop_name.eq.'FIXE') then
        if (l_loop_cont) then
            l_line_print = .true.
        endif
        if (.not.cvnewt) then
            l_line_print = .false.
        endif
    else if (loop_name.eq.'INST') then
        l_line_print = .false.
    endif
!
! - Print line in convergence table
!
    if (l_line_print) then
        call nmimpr(ds_print)
    endif
!
! - Print separator line in convergence table
!
    if (l_line_print) then
        if (cvnewt .and. .not.(l_error)) then
            if (ds_print%l_print) then 
                call nmimpx(ds_print)
            endif
        endif
    endif
!
! - Print error
!
    if (l_error) then
        call nmimpx(ds_print)
        call nmerim(sderro)
    endif
!
! - Print event messages
!
    call nmevim(ds_print, sddisc, sderro, loop_name)
!
! - Print residuals summary at end of step
!
    if (cvinst .and. .not.l_dyna_expl) then
        call nmimps(ds_print, sdconv, sderro)
    endif
!
end subroutine
