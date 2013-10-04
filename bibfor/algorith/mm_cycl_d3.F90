subroutine mm_cycl_d3(sd_cont_defi, sd_cont_solv, point_index, indi_cont, indi_frot)
!
    implicit     none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterc/r8rddg.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/mm_cycl_init.h"
#include "blas/ddot.h"
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
    character(len=24), intent(in) :: sd_cont_defi, sd_cont_solv
    integer, intent(in) :: point_index
    integer, intent(in) :: indi_cont, indi_frot
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Cycling
!
! Detection of cycling: sliding forward/backward
!
! --------------------------------------------------------------------------------------------------
!
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition 
! In  indi_cont    : contact status
! In  indi_frot    : friction status
! In  point_index  : contact point index
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_lis, sd_cycl_nbr, sd_cycl_typ, sd_cycl_gli
    integer :: jcylis, jcynbr, jcytyp, jcygli
    integer :: state_prev,cycl_type
    logical :: detect
    real(kind=8) :: module_prev,module_curr,dist_sliding 
    real(kind=8) :: angle, prosca, val, tole_angl,tole_dist
    real(kind=8) :: pres_frot_curr(3),dist_frot_curr(3)
    real(kind=8) :: pres_frot_prev(3),dist_frot_prev(3)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    detect = .false.
    tole_angl = 2.d0
    tole_dist = 1.d-2
    cycl_type = 3
!
! - Access to cycling objects
!
    sd_cycl_lis = sd_cont_solv(1:14)//'.CYCLIS'
    sd_cycl_nbr = sd_cont_solv(1:14)//'.CYCNBR'
    sd_cycl_typ = sd_cont_solv(1:14)//'.CYCTYP'
    sd_cycl_gli = sd_cont_solv(1:14)//'.CYCGLI'
    call jeveuo(sd_cycl_lis,'E',jcylis)
    call jeveuo(sd_cycl_nbr,'E',jcynbr)
    call jeveuo(sd_cycl_typ,'E',jcytyp)
    call jeveuo(sd_cycl_gli,'E',jcygli)
!
! - Current state
!
    pres_frot_curr(1) = zr(jcygli-1+12*(point_index-1)+1) 
    pres_frot_curr(2) = zr(jcygli-1+12*(point_index-1)+2) 
    pres_frot_curr(3) = zr(jcygli-1+12*(point_index-1)+3) 
    dist_frot_curr(1) = zr(jcygli-1+12*(point_index-1)+4) 
    dist_frot_curr(2) = zr(jcygli-1+12*(point_index-1)+5) 
    dist_frot_curr(3) = zr(jcygli-1+12*(point_index-1)+6) 
!
! - Previous state
!
    state_prev = zi(jcylis-1+4*(point_index-1)+3)
    pres_frot_prev(1) = zr(jcygli-1+12*(point_index-1)+7) 
    pres_frot_prev(2) = zr(jcygli-1+12*(point_index-1)+8) 
    pres_frot_prev(3) = zr(jcygli-1+12*(point_index-1)+9) 
    dist_frot_prev(1) = zr(jcygli-1+12*(point_index-1)+10) 
    dist_frot_prev(2) = zr(jcygli-1+12*(point_index-1)+11) 
    dist_frot_prev(3) = zr(jcygli-1+12*(point_index-1)+12) 
!
! - Cycling break if: no contact, no sliding or previous state was not sliding
!
    if ((indi_cont .eq. 0).or.&
        (indi_frot .eq. 0).or.&
        (state_prev .eq. 1)) then
        call mm_cycl_init(sd_cont_defi,sd_cont_solv,cycl_type)
        goto 99
    endif
!
! - Cycling detection
!
    call normev(dist_frot_prev, module_prev)
    call normev(dist_frot_curr, module_curr)
    prosca = ddot(3,dist_frot_prev,1,dist_frot_curr,1)
    angle  = 0.d0
    if ((module_prev*module_curr) .gt. r8prem()) then
        val = prosca/(module_prev*module_curr)
        if (val .gt. 1.d0) val = 1.d0
        if (val .lt. -1.d0) val = -1.d0
        angle = acos(val)
        angle = angle*r8rddg()
    endif
!
! - Sliding distance
!
    dist_sliding = 1.d2
    if (module_curr .gt. r8prem()) then
        dist_sliding = abs(module_prev-module_curr)/module_curr
    else if (module_prev .gt. r8prem()) then
        dist_sliding = abs(module_prev-module_curr)/module_prev
    endif 
!
! - Detection
!
    detect = .false.
    if ((abs(angle-180.d0) .le. tole_angl).and.&
        (dist_sliding.le.tole_dist)) then            
        detect = .true.
    endif
!
! - For next cycling
!
    zr(jcygli-1+12*(point_index-1)+7)  = pres_frot_prev(1)
    zr(jcygli-1+12*(point_index-1)+8)  = pres_frot_prev(2) 
    zr(jcygli-1+12*(point_index-1)+9)  = pres_frot_prev(3)
    zr(jcygli-1+12*(point_index-1)+10) = dist_frot_prev(1) 
    zr(jcygli-1+12*(point_index-1)+11) = dist_frot_prev(2) 
    zr(jcygli-1+12*(point_index-1)+12) = dist_frot_prev(3) 
    zi(jcylis-1+4*(point_index-1)+3)   = 0
!
! - Cycling detected
!
    if (detect) zi(jcytyp-1+4*(point_index-1)+3) = 1
!
99  continue
!
    call jedema()
end subroutine
