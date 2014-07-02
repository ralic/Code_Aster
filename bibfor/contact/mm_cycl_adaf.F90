subroutine mm_cycl_adaf(adap_type, tole_stick, tole_slide, coef_init, pres_frot,&
                        dist_frot, coef_adap, stat_adap)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/mm_cycl_laugf.h"
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
    character(len=8), intent(in) :: adap_type
    real(kind=8), intent(in) :: tole_stick
    real(kind=8), intent(in) :: tole_slide
    real(kind=8), intent(in) :: coef_init
    real(kind=8), intent(in) :: pres_frot(3)
    real(kind=8), intent(in) :: dist_frot(3)
    real(kind=8), intent(out) :: coef_adap
    integer, intent(out) :: stat_adap
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Adaptation of augmented ratio for friction
!
! --------------------------------------------------------------------------------------------------
!
! In  adap_type : type of adaptation
!                 'Sticking' - Too near from discontinuity point. Try to transform in sticking mode
!                 'Sliding' - Too near from discontinuity point. Try to transform in sliding mode
! In  tole_stick : tolerance for "near" discontinuity point by inferior value (sticking)
! In  tole_slide : tolerance for "near" discontinuity point by superior value (sliding)
! In  coef_init  : initial augmented ratio for friction
! In  pres_frot  : friction pressure
! In  dist_frot  : friction distance
! Out coef_adap  : augmented ratio for friction after adaptation
! Out stat_adap  : results of adaptation
!                       0 : OK
!                      -1 : NOOK
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: nrese, new_coef
    integer :: icoef
    aster_logical :: l_augm, l_stop
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initialisations
!
    stat_adap = -1
    l_augm = .true.
    l_stop = .false.
!
! - Trying to adapt coef
!
 10 continue
    new_coef = coef_init
    do icoef = 1, 30
        call mm_cycl_laugf(pres_frot, dist_frot, new_coef, nrese)
!
        if (adap_type .eq. 'Sticking') then
            if (nrese .le. tole_stick) then
                stat_adap = 0
                goto 99
            endif
        endif
!
        if (adap_type .eq. 'Sliding') then
            if (nrese .ge. tole_slide) then
                ASSERT(.false.)
                stat_adap = 0
                goto 99
            endif
        endif
!
        if (l_augm) then
            new_coef = new_coef*2.d0
        else
            new_coef = new_coef/2.d0
        endif
!
        if ((new_coef.ge.1.d8) .or. (new_coef.le.1.d-8)) l_stop = .true.
!
        if (l_stop) goto 15
    enddo
!
 15 continue
    if (l_stop) then
        new_coef = coef_init
        if (l_augm) then
            l_augm = .false.
            goto 10
        endif
        stat_adap = -1
    endif
!
 99 continue
!
    coef_adap = new_coef
end subroutine
