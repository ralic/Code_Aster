subroutine bussetta_algorithm(dist_cont_curr, dist_cont_prev,dist_max, coef_bussetta)

!
implicit none
!
#include "asterfort/assert.h"
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

    real(kind=8), intent(in) :: dist_cont_curr
    real(kind=8), intent(in) :: dist_cont_prev
    real(kind=8), intent(in) :: dist_max
    real(kind=8), intent(inout) :: coef_bussetta
 
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Optimal search of penalization coefficient : WARNING CYCLAGE il faut commenter
!
! --------------------------------------------------------------------------------------------------
!
! In  
! Out 
!
! --------------------------------------------------------------------------------------------------
!


    if (dist_cont_prev*dist_cont_curr .lt. 0.0) then
       if (dist_cont_prev .gt. dist_max) then 
           if (abs(dist_cont_curr) .gt. 0.0d0 .and. &
               abs(dist_cont_curr-dist_cont_prev) .gt. 0) &
               coef_bussetta = abs((coef_bussetta*dist_cont_prev)/dist_cont_curr*&
                                   (abs(dist_cont_curr)+dist_max)/(dist_cont_curr-dist_cont_prev))
       else
            if (abs(dist_cont_curr) .gt. 0) &
                coef_bussetta = abs((coef_bussetta*dist_cont_prev)/(10*dist_cont_curr))
       endif

    elseif (dist_cont_curr .gt. dist_max) then
        if (abs(dist_cont_curr-dist_cont_prev) .gt. &
            max(dist_cont_curr/10,dist_cont_prev/10,5*dist_max)) then
            coef_bussetta = 2*coef_bussetta
        elseif  (abs(dist_cont_curr) .lt. 10*dist_max) then 
            coef_bussetta = coef_bussetta*(sqrt(abs(dist_cont_curr)/dist_max -1.0)+1)**2
        elseif (abs(dist_cont_curr) .gt. (abs(dist_cont_prev)+0.01*abs(dist_cont_curr))) then 
            coef_bussetta = 2.0*coef_bussetta*(dist_cont_prev/dist_cont_curr)
        else 
            coef_bussetta = coef_bussetta*(sqrt(abs(dist_cont_curr)/dist_max -1.0)+1)
        endif
    
    endif
    
end subroutine
