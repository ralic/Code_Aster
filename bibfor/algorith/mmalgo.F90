subroutine mmalgo(sd_cont_defi  , sd_cont_solv  , l_loop_cont   , l_frot_zone   , l_vite        , &
                  l_glis_init   , l_coef_adap   , zone_index    , point_index   , indi_cont_init, &
                  indi_cont_eval, indi_frot_eval, dist_cont_curr, vite_cont_curr, pres_cont_curr, &
                  dist_frot_curr, pres_frot_curr, cycl_hist     , cycl_coef     , indi_cont_curr, &
                  indi_frot_curr, ctcsta        , mmcvca        , scotch        )
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/mm_cycl_detect.h"
#include "asterfort/mm_cycl_trait.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
    logical, intent(in) :: l_loop_cont
    logical, intent(in) :: l_frot_zone
    logical, intent(in) :: l_vite
    logical, intent(in) :: l_glis_init
    logical, intent(in) :: l_coef_adap
    integer, intent(in) :: point_index
    integer, intent(in) :: zone_index
    integer, intent(in) :: indi_cont_init
    integer, intent(in) :: indi_cont_eval
    integer, intent(in) :: indi_frot_eval
    real(kind=8), intent(in) :: dist_cont_curr
    real(kind=8), intent(in) :: vite_cont_curr
    real(kind=8), intent(in) :: pres_cont_curr
    real(kind=8), intent(in) :: dist_frot_curr(3)
    real(kind=8), intent(in) :: pres_frot_curr(3)
    real(kind=8), intent(inout) :: cycl_hist(*) 
    real(kind=8), intent(inout) :: cycl_coef(*) 
    integer, intent(out) :: indi_cont_curr
    integer, intent(out) :: indi_frot_curr
    integer, intent(out) :: ctcsta
    logical, intent(out) :: mmcvca
    logical, intent(out) :: scotch
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CONTRAINTES ACTIVES)
!
! TRAITEMENT DES DIFFERENTS CAS
!
! --------------------------------------------------------------------------------------------------
!
!
! In  sd_cont_solv   : data structure for contact solving
! In  sd_cont_defi   : data structure from contact definition 
! In  l_frot_zone    : .true. if friction on zone
! In  l_loop_cont    : .true. if fixed poitn on contact loop
! In  l_coef_adap    : .true. if automatic lagrangian adaptation
! In  l_vite         : .true. if velocity scheme (dynamic)
! In  l_glis_init    : .true. if bilateral contact for first step
! In  point_index    : contact point index
! In  indi_cont_init : previous contact status (but not for cycling)
! In  indi_cont_eval : evaluation of new contact status
! In  indi_frot_eval : evaluation of new friction status
! In  dist_cont_curr : current contact gap
! In  vite_cont_curr : current contact velocity gap
! In  pres_cont_curr : current contact pressure
! In  dist_frot_curr : current friction distance
! In  pres_frot_curr : current friction pressure
! I/O cycl_hist      : cycling history
! I/O cycl_coef      : coefficient history
! Out indi_cont_curr : current contact status
! Out indi_frot_curr : current friction status
! Out mmcvca         : .true. if contact loop converged
! Out ctcsta         : number of contact points has changed their status
! Out scotch         : .true. if contact point glued
!
! --------------------------------------------------------------------------------------------------
!
    integer :: hist_index
    real(kind=8) :: coef_cont_prev, coef_frot_prev
    real(kind=8) :: coef_cont_curr, coef_frot_curr
    real(kind=8) :: pres_frot_prev(3), pres_cont_prev
    real(kind=8) :: dist_frot_prev(3), dist_cont_prev
    integer :: indi_cont_prev, indi_frot_prev
    real(kind=8) :: coef_frot_mini, coef_frot_maxi
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations
!
    scotch = .false.
!
! - Velocity theta-scheme (dynamic)
!
    if (indi_cont_init .eq. 1) then
        scotch = .true.
    else
        scotch = .false.
    endif
!
! - Save old history
!
    do hist_index = 1, 12
        cycl_hist(25*(point_index-1)+12+hist_index) = cycl_hist(25*(point_index-1)+hist_index)
    enddo
!
! - Previous informations
!
    indi_cont_prev = nint(cycl_hist(25*(point_index-1)+12+1))
    coef_cont_prev = cycl_hist(25*(point_index-1)+12+2)
    pres_cont_prev = cycl_hist(25*(point_index-1)+12+3)
    dist_cont_prev = cycl_hist(25*(point_index-1)+12+4)
    indi_frot_prev = nint(cycl_hist(25*(point_index-1)+12+5))
    coef_frot_prev = cycl_hist(25*(point_index-1)+12+6)
    pres_frot_prev(1) = cycl_hist(25*(point_index-1)+12+7)
    pres_frot_prev(2) = cycl_hist(25*(point_index-1)+12+8)
    pres_frot_prev(3) = cycl_hist(25*(point_index-1)+12+9)
    dist_frot_prev(1) = cycl_hist(25*(point_index-1)+12+10)
    dist_frot_prev(2) = cycl_hist(25*(point_index-1)+12+11)
    dist_frot_prev(3) = cycl_hist(25*(point_index-1)+12+12)
!
! - Current max/min ratio
!
    coef_frot_mini = cycl_coef(6*(zone_index-1)+5)
    coef_frot_maxi = cycl_coef(6*(zone_index-1)+6)
!
! - Cycling detection
!
    call mm_cycl_detect(sd_cont_defi  , sd_cont_solv  , &
                        l_loop_cont   , l_frot_zone   , point_index, &
                        coef_cont_prev, pres_cont_prev, dist_cont_prev, &
                        indi_frot_prev, dist_frot_prev, &
                        indi_cont_eval, indi_frot_eval, &
                        dist_cont_curr, pres_cont_curr, dist_frot_curr)
!
! - Cycling treatment: automatic adaptation of augmented lagrangian ratio
!
    if (l_coef_adap) then
        call mm_cycl_trait(sd_cont_solv, point_index, &
                           coef_cont_prev,&
                           coef_frot_prev, pres_frot_prev, dist_frot_prev,&
                           pres_frot_curr, dist_frot_curr, &
                           indi_cont_eval, indi_frot_eval, &
                           indi_cont_curr, coef_cont_curr, &
                           indi_frot_curr, coef_frot_curr)
    else
        coef_cont_curr = coef_cont_prev
        coef_frot_curr = coef_frot_prev
        indi_cont_curr = indi_cont_eval
        indi_frot_curr = indi_frot_eval
    endif
!
! - Saving max/min ratio
!
    if (coef_frot_curr.ge.coef_frot_maxi) coef_frot_maxi = coef_frot_curr
    if (coef_frot_curr.le.coef_frot_mini) coef_frot_mini = coef_frot_curr
    cycl_coef(6*(zone_index-1)+5) = coef_frot_mini
    cycl_coef(6*(zone_index-1)+6) = coef_frot_maxi
!
! - Special treatment if velocity scheme
!
    if (l_vite) then
        if ((indi_cont_eval.eq.0) .and. (vite_cont_curr.le.0.d0)) then
            indi_cont_curr = 0
        endif
    endif
!
! - Special treatment if bilateral contact : every point is in contact
!
    if (l_glis_init) indi_cont_curr = 1
!
! - Save history for automatic cycling algorithm
!
    cycl_hist(25*(point_index-1)+1) = indi_cont_curr
    cycl_hist(25*(point_index-1)+2) = coef_cont_curr
    cycl_hist(25*(point_index-1)+3) = pres_cont_curr
    cycl_hist(25*(point_index-1)+4) = dist_cont_curr
    cycl_hist(25*(point_index-1)+5) = indi_frot_curr
    cycl_hist(25*(point_index-1)+6) = coef_frot_curr
    cycl_hist(25*(point_index-1)+7) = pres_frot_curr(1)
    cycl_hist(25*(point_index-1)+8) = pres_frot_curr(2)
    cycl_hist(25*(point_index-1)+9) = pres_frot_curr(3)
    cycl_hist(25*(point_index-1)+10) = dist_frot_curr(1)
    cycl_hist(25*(point_index-1)+11) = dist_frot_curr(2)
    cycl_hist(25*(point_index-1)+12) = dist_frot_curr(3)
!
! - Convergence ?
!
    if (indi_cont_init .ne. indi_cont_curr) then
        mmcvca = .false.
        ctcsta = ctcsta+1
    endif
!
end subroutine
