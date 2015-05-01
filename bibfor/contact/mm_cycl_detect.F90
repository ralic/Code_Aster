subroutine mm_cycl_detect(sd_cont_defi, sd_cont_solv, l_loop_cont, l_frot_zone, point_index,&
                          coef_cont, pres_cont_prev, dist_cont_prev, indi_frot_prev,&
                          dist_frot_prev, indi_cont_eval, indi_frot_eval, dist_cont_curr,&
                          pres_cont_curr, dist_frot_curr)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/mm_cycl_d1.h"
#include "asterfort/mm_cycl_d2.h"
#include "asterfort/mm_cycl_d3.h"
#include "asterfort/mm_cycl_d4.h"
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
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
    aster_logical, intent(in) :: l_loop_cont
    aster_logical, intent(in) :: l_frot_zone
    integer, intent(in) :: point_index
    real(kind=8), intent(in) :: coef_cont
    real(kind=8), intent(in) :: pres_cont_prev
    real(kind=8), intent(in) :: dist_cont_prev
    integer, intent(in) :: indi_frot_prev
    real(kind=8), intent(in) :: dist_frot_prev(3)
    integer, intent(in) :: indi_cont_eval
    integer, intent(in) :: indi_frot_eval
    real(kind=8), intent(in) :: dist_cont_curr
    real(kind=8), intent(in) :: pres_cont_curr
    real(kind=8), intent(in) :: dist_frot_curr(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Detection
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_cont_solv   : data structure for contact solving
! In  sd_cont_defi   : data structure from contact definition 
! In  l_frot_zone    : .true. if friction on zone
! In  l_loop_cont    : .true. if fixed point on contact loop
! In  point_index    : contact point index
! In  coef_cont      : augmented ratio for contact
! In  pres_cont_prev : previous pressure contact in cycle
! In  dist_cont_prev : previous pressure distance in cycle
! In  indi_frot_prev : previous friction indicator in cycle
! In  dist_frot_prev : previous friction distance in cycle
! In  indi_cont_eval : evaluation of new contact status
! In  dist_cont_curr : current contact gap
! In  pres_cont_curr : current contact pressure
! In  indi_frot_eval : evaluation of new friction status
! In  dist_frot_curr : current friction distance
!
! --------------------------------------------------------------------------------------------------
!
!
!
! - Detection of cycling: contact/no contact
!
    call mm_cycl_d1(sd_cont_solv, point_index, pres_cont_prev, dist_cont_prev, coef_cont,&
                    indi_cont_eval, dist_cont_curr, pres_cont_curr)
!
! - Detection of cycling: sliding/sticking
!
    if (l_frot_zone) then
        call mm_cycl_d2(sd_cont_defi, sd_cont_solv, point_index, indi_cont_eval, indi_frot_eval)
    endif
!
! - Detection of cycling: sliding forward/backward
!
    if (l_frot_zone) then
        call mm_cycl_d3(sd_cont_defi, sd_cont_solv, point_index, indi_frot_prev, dist_frot_prev,&
                        indi_cont_eval, indi_frot_eval, dist_frot_curr)
    endif
!
! - Detection of cycling: old flip/flop
!
    if (l_loop_cont) then
        call mm_cycl_d4(sd_cont_solv, point_index, indi_cont_eval)
    endif
!
end subroutine
