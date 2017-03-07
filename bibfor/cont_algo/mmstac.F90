subroutine mmstac(dist_cont, pres_cont, coef_cont, indi_cont_eval,cycling_type)
!
implicit none
!
#include "asterc/r8prem.h"
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
    real(kind=8), intent(in) :: dist_cont
    real(kind=8), intent(in) :: pres_cont
    real(kind=8), intent(in) :: coef_cont
    integer, intent(out) :: indi_cont_eval
    integer, intent(in), optional :: cycling_type
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method)
!
! Evaluate contact status
!
! --------------------------------------------------------------------------------------------------
!
! In  dist_cont        : contact gap
! In  pres_cont        : contact pressure
! In  coef_cont        : augmented ratio for contact
! Out indi_cont_eval   : evaluation of new contact status
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: laug_cont
!
! --------------------------------------------------------------------------------------------------
!
!
!
! - Augmented lagrangian for contact
!
    laug_cont = pres_cont - coef_cont * dist_cont
!
! - New status of contact (sign of augmented lagrangian)
!
!    if (laug_cont .lt. r8prem()) then
    if (laug_cont .lt. 0.0) then
        indi_cont_eval = 1
        if (present(cycling_type)) then
            if (abs(pres_cont) .lt. abs(-coef_cont*dist_cont) ) then
                    indi_cont_eval = 0
            endif
        endif
    else
        indi_cont_eval = 0
        if (present(cycling_type)) then
            if (abs(dist_cont) .lt. 1.d6*r8prem() ) then
                indi_cont_eval = 1
            endif
        endif
    endif
    
    
!    if (present(cycling_type)) then
!       if (cycling_type .eq. -4)  indi_cont_eval = 0
!       if (cycling_type .eq. -5)  indi_cont_eval = 0
!       if (cycling_type .eq. -6)  indi_cont_eval = 0
!       if (cycling_type .eq. -7)  indi_cont_eval = 0
!       if (cycling_type .eq. -8)  indi_cont_eval = 0
!    endif
    
end subroutine
