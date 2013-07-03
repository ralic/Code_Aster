subroutine zerop3(a, b, c, x, n)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterc/r8pi.h"
    real(kind=8) :: a, b, c, x(3)
    integer :: n
!
! ----------------------------------------------------------------------
! RESOLUTION D'UN POLYNOME DE DEGRE 3 : X**3 + A X**2 + B X + C = 0
! ----------------------------------------------------------------------
! IN  A,B,C COEFFICIENTS DU POLYNOME
! OUT X     RACINES DANS L'ORDRE DECROISSANT
! OUT N     NOMBRE DE RACINES
! ----------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: p, q, delta
    real(kind=8) :: tau, cs, alpha, u, t, y(3)
    real(kind=8) :: pi, v1, v2
!
! -- ON SE RAMENE A : Y**3 - P Y - Q = 0   AVEC Y = X + A/3
!
    p = a**2 / 3.d0 - b
    q = a*b/3 - c - 2*a**3/27.d0
!
! -- TRAITEMENT DES CAS PARTICULIERS
!
    if (p .eq. 0 .and. q .eq. 0) then
        n = 3
        y(1) = 0
        y(2) = 0
        y(3) = 0
        goto 1000
    else if (p.eq.0) then
        n = 1
        v1=abs(q)**(1.d0/3.d0)
        y(1) = sign(v1, q)
        goto 1000
    else if (p.lt.0 .and. q.eq.0) then
        n = 1
        y(1) = 0
        goto 1000
    else if (p.gt.0 .and. q.eq.0) then
        n = 3
        y(1) = sqrt(p)
        y(2) = 0
        y(3) = -sqrt(p)
        goto 1000
    endif
!
! -- SOLUTION UNIQUE SI P<0  OU  ABS(Q) > 2 (P/3) ** 3/2
!
    if (p .lt. 0 .or. abs(q) .gt. 2*abs(p/3)**1.5d0) then
        n = 1
        delta = 27*q**2 - 4*p**3
        t = (27*q + sign(sqrt(abs(27*delta)),q) ) / 2
        v2 = abs(t)**(1.d0/3.d0)
        u = sign(v2, t)
        y(1) = p/u + u/3
!
! -- SINON : TROIS RACINES
!
    else
        n = 3
        pi = r8pi()
        tau = 2 * sqrt(p/3.d0)
        cs = 4*q/tau**3
        if (cs .ge. 1) then
            alpha = 0
        else if (cs.le.-1) then
            alpha = pi/3
        else
            alpha=atan2(sqrt(1.d0-(cs**2.d0)),cs)
            alpha = alpha / 3.d0
        endif
!
        if (alpha .le. pi/3) then
            y(1) = tau*cos(alpha)
            y(2) = tau*cos(alpha - 2*pi/3.d0)
            y(3) = tau*cos(alpha + 2*pi/3.d0)
        else if (alpha.le. 2*pi/3.d0) then
            y(1) = tau*cos(alpha - 2*pi/3.d0)
            y(2) = tau*cos(alpha)
            y(3) = tau*cos(alpha + 2*pi/3.d0)
        else
            y(1) = tau*cos(alpha - 2*pi/3.d0)
            y(2) = tau*cos(alpha + 2*pi/3.d0)
            y(3) = tau*cos(alpha)
        endif
    endif
!
1000  continue
!
    do 1010 i = 1, n
        x(i) = y(i) - a/3
1010  end do
!
end subroutine
