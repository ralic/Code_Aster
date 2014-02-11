subroutine mm_cycl_zonf(lagr_frot_norm, tole_stick, tole_slide, zone_frot)
!
    implicit     none
!
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
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
    real(kind=8), intent(in) :: lagr_frot_norm
    real(kind=8), intent(in) :: tole_stick
    real(kind=8), intent(in) :: tole_slide
    integer, intent(out) :: zone_frot
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Detection of zone for friction
!
! --------------------------------------------------------------------------------------------------
!
!
! In  lagr_frot_norm : norm of augmented lagrangian for friction
! In  tole_stick     : tolerance for "near" discontinuity point by inferior value (sticking)
! In  tole_slide     : tolerance for "near" discontinuity point by superior value (sliding)
! Out zone_frot      : zone of friction
!                       -2 - Sticking far from discontinuity point
!                       -1 - Sticking near discontinuity point
!                        0 - Near discontinuity point
!                       +1 - Sliding near discontinuity point
!                       +2 - Sliding far from discontinuity point
!
! --------------------------------------------------------------------------------------------------
!
    if (lagr_frot_norm.lt.tole_stick) then 
        zone_frot = -2
    elseif ((lagr_frot_norm.ge.tole_stick).and.(lagr_frot_norm.le.1.d0)) then
        zone_frot = -1
    elseif ((lagr_frot_norm.gt.tole_stick).and.(lagr_frot_norm.lt.tole_slide)) then
        zone_frot = 0
    elseif ((lagr_frot_norm.ge.1.d0).and.(lagr_frot_norm.le.tole_slide)) then
        zone_frot = +1
    elseif (lagr_frot_norm.gt.tole_slide) then
        zone_frot = +2
    else
        ASSERT(.false.)
    endif

end subroutine
