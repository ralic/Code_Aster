subroutine quavro(quater, theta)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=
    implicit none
#include "asterc/r8prem.h"
#include "asterc/r8pi.h"
#include "blas/ddot.h"
    real(kind=8) :: quater(4), theta(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (QUATERNION)
!
! CALCULE LE VECTEUR-ROTATION THETA CORRESPONDANT AU QUATERNION
! QUATER
!
! ----------------------------------------------------------------------
!
!
! IN  QUATER : QUATERNION
! OUT THETA  : VECTEUR ROTATION
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: reste, zero, epsil, deux, coef
    real(kind=8) :: pi
    real(kind=8) :: prosca, anorx
    integer :: i
!
! ----------------------------------------------------------------------
!C
    zero = 0.d0
    epsil = r8prem( )**2
    deux = 2.d0
    pi = r8pi()
!
    prosca = ddot(3,quater,1,quater,1)
    anorx = sqrt(prosca)
    if (anorx .gt. 1.d0) anorx = 1.d0
    if (anorx .lt. epsil) then
        do 1 i = 1, 3
            theta(i) = zero
 1      continue
        goto 9999
    endif
    reste = asin(anorx)
!
    if (quater(4) .lt. zero) reste = pi - reste
!
    coef = deux*reste/anorx
!
    do 10 i = 1, 3
        theta(i) = coef * quater(i)
10  end do
9999  continue
end subroutine
