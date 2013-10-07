subroutine mm_cycl_zonc(pres_near, laug_cont, zone_cont)
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
    real(kind=8), intent(in) :: laug_cont
    real(kind=8), intent(in) :: pres_near
    integer, intent(out) :: zone_cont
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Cycling
!
! Detection of zone for contact
!
! --------------------------------------------------------------------------------------------------
!
!
! In  laug_cont : augmented lagrangian for contact
! In  pres_near : tolerance for "near" contact - pressure
! Out zone_cont : zone of contact
!
! --------------------------------------------------------------------------------------------------
!
    if (laug_cont.gt.pres_near) then
        zone_cont = 1
    elseif ((laug_cont.le.pres_near).and.(laug_cont.ge.r8prem())) then
        zone_cont = 2
    elseif ((laug_cont.ge.-pres_near).and.(laug_cont.le.r8prem())) then
        zone_cont = 3
    elseif (laug_cont.lt.-pres_near) then
        zone_cont = 4
    else
        ASSERT(.false.)
    endif

end subroutine
