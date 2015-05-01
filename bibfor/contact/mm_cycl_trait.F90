subroutine mm_cycl_trait(sd_cont_solv  , point_index, &
                         coef_cont_prev, &
                         coef_frot_prev, pres_frot_prev, dist_frot_prev, &
                         pres_frot_curr, dist_frot_curr, &
                         indi_cont_eval, indi_frot_eval, &
                         indi_cont_curr, coef_cont_curr, &
                         indi_frot_curr, coef_frot_curr)
!
    implicit     none
!
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mm_cycl_t2.h"
#include "asterfort/mm_cycl_t3.h"
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
    character(len=24), intent(in) :: sd_cont_solv
    integer, intent(in) :: point_index
    real(kind=8), intent(in) :: coef_cont_prev
    real(kind=8), intent(in) :: coef_frot_prev
    real(kind=8), intent(in) :: pres_frot_prev(3)
    real(kind=8), intent(in) :: dist_frot_prev(3)
    real(kind=8), intent(in) :: pres_frot_curr(3)
    real(kind=8), intent(in) :: dist_frot_curr(3)
    integer, intent(in) :: indi_cont_eval
    integer, intent(in) :: indi_frot_eval
    integer, intent(out) :: indi_cont_curr
    integer, intent(out) :: indi_frot_curr
    real(kind=8), intent(out) :: coef_cont_curr
    real(kind=8), intent(out) :: coef_frot_curr
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Treatment
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_cont_solv   : data structure for contact solving
! In  point_index    : contact point index
! In  coef_cont_prev : previous augmented ratio for contact
! In  coef_frot_prev : previous augmented ratio for friction
! In  pres_frot_prev : previous friction pressure in cycle
! In  dist_frot_prev : previous friction distance in cycle
! In  dist_frot_curr : friction distance
! In  pres_frot_curr : friction pressure
! In  indi_cont_eval : evaluation of new contact status
! In  indi_frot_eval : evaluation of new friction status
! Out indi_cont_curr : current contact status
! Out indi_frot_curr : current friction status
! Out coef_cont_curr : current augmented ratio for contact
! Out coef_frot_curr : current augmented ratio for friction
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_eta
    integer, pointer :: p_cycl_eta(:) => null()
    integer :: cycl_type, cycl_stat_prev, cycl_stat_curr
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Access to cycling objects
!
    sd_cycl_eta = sd_cont_solv(1:14)//'.CYCETA'
    call jeveuo(sd_cycl_eta, 'E' , vi = p_cycl_eta)
!
! - No specific treatment
!
    coef_cont_curr = coef_cont_prev
    coef_frot_curr = coef_frot_prev
    indi_cont_curr = indi_cont_eval
    indi_frot_curr = indi_frot_eval
!
! - Cycling 1: no treatment
!
    cycl_type = 1
    cycl_stat_prev = p_cycl_eta(4*(point_index-1)+cycl_type)
    if (cycl_stat_prev.gt.0) then
        cycl_stat_curr = -2
        p_cycl_eta(4*(point_index-1)+cycl_type) = cycl_stat_curr
    endif
!
! - Cycling 2
!
    cycl_type = 2
    cycl_stat_prev = p_cycl_eta(4*(point_index-1)+cycl_type)
    if (cycl_stat_prev.gt.0) then
        call mm_cycl_t2(pres_frot_prev, dist_frot_prev, coef_frot_prev, &
                        cycl_stat_prev, pres_frot_curr, dist_frot_curr, &
                        coef_frot_curr, cycl_stat_curr)
        p_cycl_eta(4*(point_index-1)+cycl_type) = cycl_stat_curr
        goto 99
    endif
!
! - Cycling 3
!
    cycl_type = 3
    cycl_stat_prev = p_cycl_eta(4*(point_index-1)+cycl_type)
    if (cycl_stat_prev.gt.0) then
        call mm_cycl_t3(pres_frot_prev, dist_frot_prev, coef_frot_prev, &
                        cycl_stat_curr)
        p_cycl_eta(4*(point_index-1)+cycl_type) = cycl_stat_curr
        goto 99
    endif
!
99  continue
!
    call jedema()
end subroutine
