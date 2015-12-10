subroutine mm_cycl_trait(ds_contact    , i_cont_poin, &
                         coef_cont_prev, &
                         coef_frot_prev, pres_frot_prev, dist_frot_prev, &
                         pres_frot_curr, dist_frot_curr, &
                         indi_cont_eval, indi_frot_eval, &
                         indi_cont_curr, coef_cont_curr, &
                         indi_frot_curr, coef_frot_curr)
!
use NonLin_Datastructure_type
!
implicit none
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: i_cont_poin
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
! Contact - Solve - Cycling
!
! Treatment
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  i_cont_poin      : contact point index
! In  coef_cont_prev   : previous augmented ratio for contact
! In  coef_frot_prev   : previous augmented ratio for friction
! In  pres_frot_prev   : previous friction pressure in cycle
! In  dist_frot_prev   : previous friction distance in cycle
! In  dist_frot_curr   : friction distance
! In  pres_frot_curr   : friction pressure
! In  indi_cont_eval   : evaluation of new contact status
! In  indi_frot_eval   : evaluation of new friction status
! Out indi_cont_curr   : current contact status
! Out indi_frot_curr   : current friction status
! Out coef_cont_curr   : current augmented ratio for contact
! Out coef_frot_curr   : current augmented ratio for friction
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_cyceta
    integer, pointer :: p_sdcont_cyceta(:) => null()
    integer :: cycl_type, cycl_stat_prev, cycl_stat_curr
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Access to cycling objects
!
    sdcont_cyceta = ds_contact%sdcont_solv(1:14)//'.CYCETA'
    call jeveuo(sdcont_cyceta, 'E' , vi = p_sdcont_cyceta)
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
    cycl_stat_prev = p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type)
    if (cycl_stat_prev.gt.0) then
        cycl_stat_curr = -2
        p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type) = cycl_stat_curr
    endif
!
! - Cycling 2
!
    cycl_type = 2
    cycl_stat_prev = p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type)
    if (cycl_stat_prev.gt.0) then
        call mm_cycl_t2(pres_frot_prev, dist_frot_prev, coef_frot_prev, &
                        cycl_stat_prev, pres_frot_curr, dist_frot_curr, &
                        coef_frot_curr, cycl_stat_curr)
        p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type) = cycl_stat_curr
        goto 99
    endif
!
! - Cycling 3
!
    cycl_type = 3
    cycl_stat_prev = p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type)
    if (cycl_stat_prev.gt.0) then
        call mm_cycl_t3(pres_frot_prev, dist_frot_prev, coef_frot_prev, &
                        cycl_stat_curr)
        p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type) = cycl_stat_curr
        goto 99
    endif
!
99  continue
!
    call jedema()
end subroutine
