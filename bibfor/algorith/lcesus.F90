subroutine lcesus(x,val,der)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none

    real(kind=8),intent(in) :: x
    real(kind=8),intent(out):: val,der
! --------------------------------------------------------------------------------------------------
!   smoothed unilateral function and its pseudo (secant) derivative:
!     f(x) = (x - 0.5/gamma) * exp(1/(gamma*x)) if x<0
!     f(x) = 0                                  if x>0
! --------------------------------------------------------------------------------------------------
! x:   argument
! val: f(x)
! der: f'(x)  
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, gamma, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,gamma,rigmin,pc,pr,epsth
! --------------------------------------------------------------------------------------------------
    if (x*gamma.ge.-1.d-3) then
        val = 0
        der = 0
    else
        val = (x-0.5d0/gamma) * exp(1/(gamma*x))
        der = val/x
    end if

end subroutine lcesus
