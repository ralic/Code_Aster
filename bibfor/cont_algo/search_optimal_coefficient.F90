subroutine search_optimal_coefficient(coef, indi, pres_cont, dist_cont,&
                                      coef_opt , terminate)

!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/mmstac.h"
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
    real(kind=8), intent(in) :: coef(2)
    integer       :: indi(2)
    real(kind=8), intent(inout) :: pres_cont(2)
    real(kind=8), intent(inout) :: dist_cont(2)
    real(kind=8), intent(out) :: coef_opt
    aster_logical, intent(out) :: terminate
 
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Optimal search of coefficient : WARNING CYCLAGE
!
! --------------------------------------------------------------------------------------------------
!
! In  
! Out 
!
! --------------------------------------------------------------------------------------------------
!
    integer      :: indi_curr,indi_prev, it,mode_cycl = 1
    real(kind=8) :: save_coefficient, coefficient
    real(kind=8) :: valmin, valmax
    
    terminate   = .false.
    coefficient = coef(1)
    save_coefficient = coefficient
    valmin      = coef(1)
    valmax      = coef(2)
    it = 1 
    
    do while ( (coefficient .lt. valmax) .and. (.not. terminate))
       
! Optimalite du coefficient : 
!     (pres_prev -coef*dist_prev)*(pres_curr -coef*dist_curr) > 0
       if (mode_cycl .eq. 1) then
           if (dist_cont(1) .gt. 1.d-6 )   dist_cont(1) = 0.0
           if (pres_cont(1) .gt. 1.d-6 )  pres_cont(1) = -1.d-15
           if (dist_cont(2) .gt. 1.d-6 )  dist_cont(1) = 0.0
           if (pres_cont(2) .gt. 1.d-6 )  pres_cont(1) = -1.d-15
       endif
       call mmstac(dist_cont(1), pres_cont(1),coefficient,indi_curr)
       call mmstac(dist_cont(2), pres_cont(2),coefficient,indi_prev)
        
       if ((indi_curr + indi_prev .eq. 0) .or.&
           (indi_curr + indi_prev .eq. 2)     ) then 
           terminate = .true.
           indi(1)   = indi_prev
           indi(2)   = indi_curr
           
       elseif (indi_curr + indi_prev .eq. 1) then 
           terminate = .false.
           
       else  
           ASSERT(.false.)
           
       endif
       
! Dichotomie : continue iteration using dichotomie 
       if (terminate .and. (it .lt. 500)) then
           it = it + 1 
           save_coefficient = coefficient  
           valmax = (log(coefficient) + log(valmax)) / 2
           if (valmax .lt. log(coef(2))) then 
               valmax = 10**valmax
               terminate = .false. 
           else 
               terminate = .true.
           endif
           
       else
           save_coefficient = coefficient
           
       endif
      coefficient = coefficient *4.0d0     
! Dichotomie :
      
    end do
    
    coef_opt = save_coefficient
    
end subroutine
