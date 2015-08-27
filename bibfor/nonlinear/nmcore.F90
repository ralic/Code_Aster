subroutine nmcore(sdcrit          , sderro, list_func_acti, nume_inst, iter_newt,&
                  line_search_iter, eta   , resi_norm     , load_norm, ds_conv )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcoru.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmevcv.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmerge.h"
#include "asterfort/GetResi.h"
#include "asterfort/SetResi.h"
#include "asterfort/nmcore_swap.h"
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
    character(len=19), intent(in) :: sdcrit
    character(len=24), intent(in) :: sderro
    integer, intent(in) :: list_func_acti(*)
    integer, intent(in) :: nume_inst
    integer, intent(in) :: iter_newt
    integer, intent(in) :: line_search_iter
    real(kind=8), intent(in) :: eta
    real(kind=8), intent(in) :: resi_norm
    real(kind=8), intent(in) :: load_norm
    type(NL_DS_Conv), intent(inout) :: ds_conv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Evaluate convergence of residuals
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcrit           : name of datastructure to save convergence parameters
! In  sderro           : name of datastructure for error management (events)
! In  list_func_acti   : list of active functionnalities
! In  nume_inst        : index of current time step
! In  iter_newt        : index of current Newton iteration
! In  line_search_iter : number of iterations for line search
! In  eta              : coefficient for pilotage (continuation)
! In  resi_norm        : norm of equilibrium residual
! In  load_norm        : norm of exterior loads
! IO  ds_conv          : datastructure for convergence management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcrit_crtr
    real(kind=8), pointer :: v_sdcrit_crtr(:) => null()
    character(len=16) :: event_type
    real(kind=8) :: load_mini, last_resi_conv, user_para, vale_calc
    integer :: i_resi, nb_resi
    aster_logical :: l_resi_test, l_conv, l_swap_rela_maxi, l_swap_comp_rela
    aster_logical :: cvresi
!
! --------------------------------------------------------------------------------------------------
!
    nb_resi = ds_conv%nb_resi
    cvresi  = .true._1
!
! - Get previous convergence informations
!
    sdcrit_crtr = sdcrit(1:19)//'.CRTR'
    call jeveuo(sdcrit_crtr, 'E', vr = v_sdcrit_crtr)
    last_resi_conv = v_sdcrit_crtr(7)
    load_mini      = v_sdcrit_crtr(6)
!
! - Event: no convergence
!
    call SetResi(ds_conv, l_conv_ = .false._1)
    do i_resi = 1, nb_resi
        event_type = ds_conv%list_resi(i_resi)%event_type
        call nmcrel(sderro, event_type, .false._1)
    end do
!
! - Swap convergence criterias if necessary
!
    call nmcore_swap(sderro , nume_inst, load_norm, load_mini, last_resi_conv,&
                     ds_conv)
!
! - Check residuals stop criterias
!
    do i_resi = 1, nb_resi
        vale_calc = ds_conv%list_resi(i_resi)%vale_calc
        user_para = ds_conv%list_resi(i_resi)%user_para
        if (ds_conv%l_resi_test(i_resi)) then
            call nmcoru(vale_calc, user_para, l_conv)
        else
            l_conv = .true._1
        endif
        ds_conv%list_resi(i_resi)%l_conv = l_conv
    end do
!
! - Save events
!
    do i_resi = 1, nb_resi
        event_type  = ds_conv%list_resi(i_resi)%event_type
        l_conv      = ds_conv%list_resi(i_resi)%l_conv
        l_resi_test = ds_conv%l_resi_test(i_resi)
        if (l_resi_test) then
            call nmcrel(sderro, event_type, .not.l_conv)
        endif
    end do
!
! - Event: evaluate convergence of residual
!
    call nmevcv(sderro, list_func_acti, 'RESI')
    call nmlecv(sderro, 'RESI', cvresi)
!
! - If swapped: retrieve old convergence system for next step
!
    call nmerge(sderro, 'RESI_MAXR', l_swap_rela_maxi)
    if (l_swap_rela_maxi) then
        call SetResi(ds_conv, type_ = 'RESI_GLOB_RELA' , l_resi_test_ = .true._1)
        call SetResi(ds_conv, type_ = 'RESI_GLOB_MAXI' , l_resi_test_ = .false._1)
    endif
    call nmerge(sderro, 'RESI_MAXN', l_swap_comp_rela)
    if (l_swap_comp_rela) then
        call SetResi(ds_conv, type_ = 'RESI_GLOB_RELA' , l_resi_test_ = .false._1)
        call SetResi(ds_conv, type_ = 'RESI_COMP_RELA' , l_resi_test_ = .true._1)
    endif
!
! - New minimum exterior load
!
    if ((nume_inst.eq.1) .and. (iter_newt.eq.0)) then
        load_mini = load_norm
    else
        if (cvresi .and. (.not.l_swap_rela_maxi)) then
            load_mini = min(load_norm, load_mini)
        endif
    endif
!
! - Save informations
!
    call GetResi(ds_conv, type = 'RESI_GLOB_RELA' , vale_calc_ = v_sdcrit_crtr(3))
    call GetResi(ds_conv, type = 'RESI_GLOB_MAXI' , vale_calc_ = v_sdcrit_crtr(4))
    call GetResi(ds_conv, type = 'RESI_REFE_RELA' , vale_calc_ = v_sdcrit_crtr(8))
    call GetResi(ds_conv, type = 'RESI_COMP_RELA' , vale_calc_ = v_sdcrit_crtr(9))
    v_sdcrit_crtr(1) = iter_newt+1
    v_sdcrit_crtr(2) = line_search_iter
    v_sdcrit_crtr(5) = eta
    v_sdcrit_crtr(6) = load_mini
    if (cvresi) then
        v_sdcrit_crtr(7) = resi_norm
    endif
!
end subroutine
