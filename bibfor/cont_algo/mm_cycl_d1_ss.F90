subroutine mm_cycl_d1_ss(pres_near, laug_cont_prev, laug_cont_curr, zone_cont_prev, zone_cont_curr,&
                         cycl_sub_type,alpha_cont_matr,alpha_cont_vect)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/mm_cycl_zonc.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    real(kind=8), intent(in) :: pres_near
    real(kind=8), intent(in) :: laug_cont_prev
    real(kind=8), intent(in) :: laug_cont_curr
    real(kind=8), intent(out) :: alpha_cont_matr, alpha_cont_vect
    integer, intent(out) :: zone_cont_prev
    integer, intent(out) :: zone_cont_curr
    integer, intent(out) :: cycl_sub_type
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Detection: contact/no-contact sub-cycling
!
! --------------------------------------------------------------------------------------------------
!
! In  pres_near        : tolerance for "near" contact - pressure
! In  laug_cont_prev   : previous augmented lagrangien for contact
! In  laug_cont_curr   : current augmented lagrangien for contact
! Out zone_cont_prev   : previous zone of contact
! Out zone_cont_curr   : current zone of contact
! Out cycl_sub_type    : sub-cycling type
!
! --------------------------------------------------------------------------------------------------
!
    cycl_sub_type = 0
    zone_cont_prev = 0
    zone_cont_curr = 0
!
! - Zoning detection - Previous
!
    call mm_cycl_zonc(pres_near, laug_cont_prev, zone_cont_prev)
!
! - Zoning detection - Current
!
    call mm_cycl_zonc(pres_near, laug_cont_curr, zone_cont_curr)
!
! - Sub-cycling 1 : grazing cycling
! 
!   alpha_cont_matr = 0.3
!   alpha_cont_vect = 0.9
    if (((zone_cont_prev.eq.3).and.(zone_cont_curr.eq.2)).or. &
        ((zone_cont_prev.eq.2).and.(zone_cont_curr.eq.3))) then
        cycl_sub_type = 1
        alpha_cont_matr = 0.5
        alpha_cont_vect = 1.0
!        if (zone_cont_prev.eq.3) then 
!            alpha_cont_matr = 0.7
!            alpha_cont_vect = 0.7
!        else
!            alpha_cont_matr = 0.3 
!            alpha_cont_vect = 0.3
!        endif
    
!
! - Sub-cycling 2
!
    elseif (((zone_cont_prev.eq.2).and.(zone_cont_curr.eq.4)).or. &
        ((zone_cont_prev.eq.4).and.(zone_cont_curr.eq.2))) then
        cycl_sub_type = 2
        if (zone_cont_prev.eq.4) then 
            alpha_cont_matr = 0.5
            alpha_cont_vect = 1.0
        else
            alpha_cont_matr = 0.5
            alpha_cont_vect = 1.0
        endif
    
!
! - Sub-cycling 3
!
    elseif (((zone_cont_prev.eq.1).and.(zone_cont_curr.eq.3)).or. &
        ((zone_cont_prev.eq.3).and.(zone_cont_curr.eq.1))) then
        cycl_sub_type = 3
        if (zone_cont_prev.eq.3) then 
            alpha_cont_matr = 0.5
            alpha_cont_vect = 1.0
        else
            alpha_cont_matr = 0.5
            alpha_cont_vect = 1.0
        endif
    
!
! - Sub-cycling 4
!
    elseif (((zone_cont_prev.eq.1).and.(zone_cont_curr.eq.4)).or. &
        ((zone_cont_prev.eq.4).and.(zone_cont_curr.eq.1))) then
        cycl_sub_type = 4
        alpha_cont_matr = 0.5
        alpha_cont_vect = 1.0
    
    else 
        cycl_sub_type = 5
        alpha_cont_matr = 0.5
        alpha_cont_vect = 1.0
!        ASSERT(.false.)
    endif

end subroutine
