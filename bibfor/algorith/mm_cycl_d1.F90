subroutine mm_cycl_d1(sd_cont_solv, point_index, pres_cont_prev, dist_cont_prev, coef_cont, &
                      indi_cont   , dist_cont  , pres_cont)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/iscode.h"
#include "asterfort/iscycl.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mm_cycl_shift.h" 
#include "asterfort/mm_cycl_d1_ss.h" 
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
!
    character(len=24), intent(in) :: sd_cont_solv
    integer, intent(in) :: point_index
    real(kind=8), intent(in) :: pres_cont_prev
    real(kind=8), intent(in) :: dist_cont_prev
    real(kind=8), intent(in) :: coef_cont
    real(kind=8), intent(in) :: pres_cont
    real(kind=8), intent(in) :: dist_cont
    integer, intent(in) :: indi_cont
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Cycling
!
! Detection of cycling: contact/no-contact
!
! --------------------------------------------------------------------------------------------------
!
!
! In  sd_cont_solv   : data structure for contact solving
! In  point_index    : contact point index
! In  pres_cont_prev : previous contact pressure in cycle
! In  dist_cont_prev : previous contact distance in cycle
! In  coef_cont      : augmented ratio for contact
! In  pres_cont      : contact pressure
! In  dist_cont      : contact distance
! In  indi_cont      : contact indicator
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_lis, sd_cycl_nbr, sd_cycl_eta
    integer :: jcylis, jcynbr, jcyeta
    integer :: statut(30)
    integer :: cycl_type, cycl_long_acti
    integer :: cycl_ecod(1), cycl_long, cycl_sub_type, cycl_stat
    logical :: detect
    real(kind=8) :: laug_cont_prev, laug_cont_curr
    real(kind=8) :: pres_near
    integer :: zone_cont_prev, zone_cont_curr
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    cycl_long_acti = 3
    cycl_type = 1
    pres_near = 1.d+2
!
! - Access to cycling objects
!
    sd_cycl_lis = sd_cont_solv(1:14)//'.CYCLIS'
    sd_cycl_nbr = sd_cont_solv(1:14)//'.CYCNBR'
    sd_cycl_eta = sd_cont_solv(1:14)//'.CYCETA'
    call jeveuo(sd_cycl_lis,'E',jcylis)
    call jeveuo(sd_cycl_nbr,'E',jcynbr)
    call jeveuo(sd_cycl_eta,'E',jcyeta)
!
! - Previous state
!
    laug_cont_prev = pres_cont_prev - coef_cont * dist_cont_prev
!
! - Current state
!
    laug_cont_curr = pres_cont - coef_cont * dist_cont
!
! - Cycle state
!
    cycl_ecod(1) = zi(jcylis-1+4*(point_index-1)+cycl_type)
    cycl_long = zi(jcynbr-1+4*(point_index-1)+cycl_type)
    call isdeco(cycl_ecod(1),statut,30)
!
! - New iteration in cycle
!
    cycl_long = cycl_long + 1
    statut(cycl_long) = indi_cont
    call iscode(statut,cycl_ecod(1),30)
!
! - Cycling detection
!
    cycl_stat = 0
    if (cycl_long .eq. cycl_long_acti) then
        detect = iscycl(cycl_ecod(1), cycl_long_acti)
        if (detect) then
            cycl_stat = 10
            call mm_cycl_d1_ss(pres_near, laug_cont_prev, laug_cont_curr, zone_cont_prev, &
                               zone_cont_curr, cycl_sub_type)
            cycl_stat = cycl_stat + cycl_sub_type
        endif
    endif
!
! - End of cycling detection zone: shifting
!
    if (cycl_long .eq. cycl_long_acti) call mm_cycl_shift(cycl_long_acti, cycl_ecod(1), cycl_long)
!
! - Cycling save
!
    zi(jcyeta-1+4*(point_index-1)+cycl_type) = cycl_stat
    zi(jcylis-1+4*(point_index-1)+cycl_type) = cycl_ecod(1)
    zi(jcynbr-1+4*(point_index-1)+cycl_type) = cycl_long
!
    call jedema()
end subroutine
