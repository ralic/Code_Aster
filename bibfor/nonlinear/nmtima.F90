subroutine nmtima(ds_measure, timer_type_, vali)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
    type(NL_DS_Measure), intent(in) :: ds_measure
    character(len=*), intent(in) :: timer_type_
    integer, intent(out) :: vali
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Measure and statistic management
!
! Evaluate remaining type
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_measure       : datastructure for measure and statistics management
! In  timer_type       : type of current timer
! Out vali             : 0 - enough time
!                        1 - not enough time
!
! --------------------------------------------------------------------------------------------------
!
    character(len=9) :: timer_type
    integer :: i_timer, timer_indx, nb_timer
    type(NL_DS_Timer) :: timer
    real(kind=8) :: remaining_time, store_mean_time, iter_mean_time, step_mean_time
!
! --------------------------------------------------------------------------------------------------
!
    nb_timer   = ds_measure%nb_timer
    timer_indx = 0
    vali       = 0
    timer_type = timer_type_
!
! - Find timer
!
    do i_timer = 1, nb_timer
        if (ds_measure%timer(i_timer)%type .eq. timer_type) then
            ASSERT(timer_indx.eq.0)
            timer_indx = i_timer
        endif
    end do
!
! - Get current timer
!
    ASSERT(timer_indx.ne.0)
    timer     = ds_measure%timer(timer_indx)
!
! - Get mean times
!
    store_mean_time = ds_measure%store_mean_time
    iter_mean_time  = ds_measure%iter_mean_time
    step_mean_time  = ds_measure%step_mean_time
!
! - Enough time ?
!
    if (timer_type .eq. 'Newt_Iter') then
        remaining_time = ds_measure%iter_remain_time
        if ((2.d0*iter_mean_time) .le. (0.95d0*remaining_time-store_mean_time)) then
            vali = 0
        else
            vali = 1
        endif
    else if (timer_type .eq. 'Time_Step') then
        remaining_time = ds_measure%step_remain_time
        if (step_mean_time .le. 0.90d0*remaining_time) then
            vali = 0
        else
            vali = 1
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
