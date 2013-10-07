subroutine mm_cycl_d3(sd_cont_defi, sd_cont_solv, point_index, indi_frot_prev, dist_frot_prev,&
                      indi_cont   , indi_frot   , dist_frot)
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
#include "asterfort/mm_cycl_erase.h"
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
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
    integer, intent(in) :: point_index
    integer, intent(in) :: indi_frot_prev
    real(kind=8), intent(in) :: dist_frot_prev(3)
    integer, intent(in) :: indi_cont
    integer, intent(in) :: indi_frot
    real(kind=8), intent(in) :: dist_frot(3)
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
! In  sd_cont_solv   : data structure for contact solving
! In  sd_cont_defi   : data structure from contact definition 
! In  point_index    : contact point index
! In  indi_frot_prev : previous friction indicator
! In  dist_frot_prev : previous friction distance
! In  indi_cont      : contact indicator
! In  indi_frot      : friction indicator
! In  indi_frot      : current friction indicator
! In  dist_frot      : current friction distance
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_lis, sd_cycl_nbr, sd_cycl_eta
    integer :: jcylis, jcynbr, jcyeta
    real(kind=8) :: module_prev, module_curr
    real(kind=8) :: angle, prosca, val, tole_angl
    integer :: cycl_type, cycl_ecod, cycl_long, cycl_stat
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    cycl_type = 3
    cycl_stat = 0
    cycl_ecod = 0
    cycl_long = 0
    tole_angl = 2.d0
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
! - Cycling break if: no contact, sticking or previous state was sticking
!
    if ((indi_cont .eq. 0).or.&
        (indi_frot .eq. 1).or.&
        (indi_frot_prev .eq. 1)) then
        call mm_cycl_erase(sd_cont_defi, sd_cont_solv, cycl_type, point_index)
        goto 99
    endif
!
! - Cycling detection
!
    prosca = ddot(3,dist_frot_prev,1,dist_frot,1)
    module_curr = sqrt(dist_frot(1)*dist_frot(1)+&
                       dist_frot(2)*dist_frot(2)+&
                       dist_frot(3)*dist_frot(3))
    module_prev = sqrt(dist_frot_prev(1)*dist_frot_prev(1)+&
                       dist_frot_prev(2)*dist_frot_prev(2)+&
                       dist_frot_prev(3)*dist_frot_prev(3))
    angle  = 0.d0
    if ((module_prev*module_curr) .gt. r8prem()) then
        val = prosca/(module_prev*module_curr)
        if (val .gt. 1.d0) val = 1.d0
        if (val .lt. -1.d0) val = -1.d0
        angle = acos(val)
        angle = angle*r8rddg()
    endif
!
! - Detection
!
    cycl_stat = 0
    if (abs(angle-180.d0) .le. tole_angl) then            
        cycl_stat = 10
    endif
!
! - Cycling save
!
    zi(jcyeta-1+4*(point_index-1)+cycl_type) = cycl_stat
    zi(jcylis-1+4*(point_index-1)+cycl_type) = cycl_ecod
    zi(jcynbr-1+4*(point_index-1)+cycl_type) = cycl_long
!
99  continue
!
    call jedema()
end subroutine
