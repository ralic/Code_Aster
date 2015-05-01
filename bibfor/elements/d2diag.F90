subroutine d2diag(a, d, s, theta)
!
    implicit  none
!-----------------------------------------------------------------------
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
!======================================================================
!
!     CALCUL DE LA MATRICE DIAGONALE D UNE MATRICE 2x2
!
! IN  A : MATRICE 2x2 A DIAGONALISER
!
! OUT D : MATRICE DIAGONALISER
! OUT S : MATRICE DE PASSAGE
! OUT THETA : ANGLE DE ROTATION
!
#include "asterfort/di2epx.h"
    real(kind=8) :: zero, a(2, 2), d(2), s(2, 2), theta, b(3), aux
    real(kind=8) :: aux1
!
    aux1 = 1.d0
    zero = 1.d-10
!
    b(1)=a(1,1)
    b(2)=a(2,2)
    b(3)=a(1,2)
!
!     CALCUL DE LA MATRICE DIAGONALISEE D DE B
    call di2epx(b, d)
!
    if (abs(a(1,2)) .gt. zero*(abs(d(1))+abs(d(2)))) then
        aux = a(2,2)-a(1,1)
        aux = (aux+sqrt(aux**2+4.d0*a(1,2)**2))/(2.d0*a(1,2))
!
        theta = atan2(aux,aux1)
!
        s(1,1) = cos(theta)
        s(2,2) = s(1,1)
        s(1,2) = sin(theta)
        s(2,1) = -s(1,2)
    else
        theta = 0.d0
        s(1,1) = 1.d0
        s(2,2) = 1.d0
        s(1,2) = 0.d0
        s(2,1) = 0.d0
    endif
end subroutine
