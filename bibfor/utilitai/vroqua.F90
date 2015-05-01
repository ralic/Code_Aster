subroutine vroqua(theta, quater)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "blas/ddot.h"
    real(kind=8) :: quater(4), theta(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (QUATERNION)
!
! CALCULE LE QUATERNION CORRESPONDANT AU VECTEUR-ROTATION THETA
!
! ----------------------------------------------------------------------
!
!
! OUT QUATER : QUATERNION
! IN  THETA  : VECTEUR ROTATION
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: epsil, demi, un
    real(kind=8) :: angle, coef, prosca
    integer :: i
!
! ----------------------------------------------------------------------
!
    epsil = 1.d-4
    demi = 5.d-1
    un = 1.d0
!
    prosca = ddot(3,theta,1,theta,1)
    angle = demi * sqrt(prosca)
    quater(4) = cos(angle)
    if (angle .gt. epsil) then
        coef = demi * sin(angle) / angle
    else
        coef = un - angle**2/6.d0
        coef = demi * coef
    endif
    do 1 i = 1, 3
        quater(i) = coef * theta(i)
 1  end do
!
end subroutine
