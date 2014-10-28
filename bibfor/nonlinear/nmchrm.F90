subroutine nmchrm(phasis   , parmet     , method   , list_func_acti, sddisc   ,&
                  sddyna   , nume_inst  , iter_newt, sdcont_defi   , type_pred,&
                  type_corr, l_matr_asse)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/utmess.h"
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
    character(len=10), intent(in) :: phasis
    real(kind=8), intent(in) :: parmet(*)
    character(len=16), intent(in) :: method(*)
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    integer, intent(in) :: nume_inst
    integer, intent(in) :: iter_newt
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: list_func_acti(*)
    character(len=16), intent(out) :: type_corr
    character(len=16), intent(out) :: type_pred
    aster_logical, intent(out) :: l_matr_asse
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Options for assembling global matrix
!
! --------------------------------------------------------------------------------------------------
!
! In  phasis           : phasis in non-linear algorithm
!                            'PREDICTION'
!                            'CORRECTION'
!                            'FORCES_INT'
! In  sddisc           : datastructure for discretization
! In  nume_inst        : index of current time step
! In  iter_newt        : index of current Newton iteration
! In  list_func_acti   : list of active functionnalities
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  method           : method options for non-linear solver
! In  parmet           : method parameters for non-linear solver
! In  sddyna           : name of dynamic parameters datastructure
! Out type_corr        : type of matrix for correction (Newton)
! Out type_pred        : type of matrix for prediction (Euler)
! Out l_matr_asse      : .true. if re-compute matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8) :: time_prev, time_curr, pas_mini_elas, time_incr
    integer :: reac_incr, reac_iter
    aster_logical :: l_matr_cont
    aster_logical :: l_cont_elem, l_cont_disc, l_matr_elas
    aster_logical :: l_first_step, l_dyna, l_amor, l_dischoc, l_varc, l_elas_fo
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><CALC> CHOIX D''ASSEMBLAGE DE MATRICE GLOBALE'
    endif
!
! - Initializations
!
    l_matr_asse = .false.
    l_matr_cont      = .false.
!
! - Parameters
!
    reac_incr     = nint(parmet(1))
    reac_iter     = nint(parmet(2))
    type_corr     = method(2)
    type_pred     = method(5)
    time_prev     = diinst(sddisc, nume_inst-1)
    time_curr     = diinst(sddisc, nume_inst )
    time_incr     = time_curr-time_prev
    pas_mini_elas = parmet(3)
!
! - First step ?
!
    l_first_step = nume_inst.le.1
!
! - Active functionnalities
!
    l_dyna      = ndynlo(sddyna,'DYNAMIQUE')
    l_amor      = ndynlo(sddyna,'MAT_AMORT')
    l_cont_disc = isfonc(list_func_acti,'CONT_DISCRET')
    l_cont_elem = isfonc(list_func_acti,'ELT_CONTACT')
    l_dischoc   = isfonc(list_func_acti,'DIS_CHOC')
    l_varc      = isfonc(list_func_acti,'EXI_VARC' )
    l_elas_fo   = isfonc(list_func_acti,'ELAS_FO' )
!
! - Add contact matrix in global matrix ?
!
    if (l_cont_disc) then
        l_matr_cont = cfdisl(sdcont_defi,'MODI_MATR_GLOB')
    endif
!
! - Elastic matrix for time_incr < PAS_MINI_ELAS
!
    if (abs(time_incr) .lt. pas_mini_elas) then
        reac_incr = 1
        reac_iter = nint(parmet(4))
        type_pred = 'SECANTE'
        type_corr = 'SECANTE'
    endif
!
! - Re-compute matrix ?
!
    if (phasis .eq. 'CORRECTION' .or. phasis .eq. 'FORCES_INT') then
        if ((type_corr.eq.'TANGENTE') .or. (type_corr.eq.'SECANTE')) then
            l_matr_asse = .false.
            if (reac_iter .ne. 0) then
                l_matr_asse = mod(iter_newt+1,reac_iter) .eq. 0
            endif
        else
            l_matr_asse = .false.
        endif
    else if (phasis.eq.'PREDICTION') then
        if ((reac_incr.eq.0) .and. (nume_inst.ne.1)) then
            l_matr_asse = .false.
        endif
        if (nume_inst .eq. 1) then
            l_matr_asse = .true.
        endif
        if ((reac_incr.ne.0) .and. (nume_inst.ne.1)) then
            l_matr_asse = mod(nume_inst-1,reac_incr) .eq. 0
        endif
    else
        ASSERT(.false.)
    endif
!
! - Is matrix is elastic ?
!
    if ((type_corr.eq.'ELASTIQUE') .or. (type_pred.eq.'ELASTIQUE')) then
        l_matr_elas = .true.
    else
        l_matr_elas = .false.
    endif
!
! - Re-compute matrix if Rayleigh damping
!
    if (phasis .eq. 'PREDICTION' .and. l_dyna .and. l_amor) then
        if (l_first_step) then
            l_matr_asse = .true.
        endif
    endif
!
! - Re-compute matrix if elementary contact
!
    if (l_cont_elem) then
        if (.not.l_matr_asse) then
            if (type_corr.ne.'ELASTIQUE') then
                call utmess('A', 'MECANONLINE5_4')
            endif
            l_matr_asse = .true.
        endif
    endif
!
! - Re-compute matrix if DIS_CHOC elements
!
    if (l_dischoc) then
        if (.not.l_matr_asse) then
            call utmess('A', 'MECANONLINE5_5')
            l_matr_asse = .true.
        endif
    endif
!
! - Re-compute matrix if contact matrix in global matrix
!
    if (l_matr_cont) then
        if (.not.l_matr_asse) then
            call utmess('A', 'MECANONLINE5_5')
            l_matr_asse = .true.
        endif
    endif
!
! - Change matrix if DIS_CHOC
!
    if (l_dischoc) then
        type_corr = 'TANGENTE'
    endif
!
! - Re-compute matrix if command variables and elastic function
!
    if (l_matr_elas .and. phasis .eq. 'PREDICTION') then
        if (l_varc .and. l_elas_fo) then
            l_matr_asse = .true.
        endif
    endif
!
! - Print
!
    if (niv .ge. 2) then
        if (l_matr_asse) then
            write (ifm,*) '<MECANONLINE><CALC> ON ASSEMBLE LA MATRICE'
        else
            write (ifm,*) '<MECANONLINE><CALC> ON N''ASSEMBLE PAS LA MATRICE'
        endif
    endif
!
end subroutine
