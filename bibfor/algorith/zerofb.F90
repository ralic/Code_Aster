subroutine zerofb(func, x1, x2, tol, itmax,&
                  zbrent, iret, iter)
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! aslint: disable=
    implicit none
!
#include "asterc/r8prem.h"
    interface
        function func(x)
            real(kind=8) :: func, x
        end function func
    end interface
    integer :: itmax, iter, iret
    real(kind=8) :: zbrent, tol, x1, x2, eps
    real(kind=8) :: a, b, c, d, e, fa, fb, fc, p, q, r, s, tol1, xm
! ----------------------------------------------------------------------
!     BUT : TROUVER LE ZERO D'UNE FONCTION SCALAIRE REELLE
!     AVEC LA METHODE DE BRENT
!
!     USING BRENT'S METHOD, FIND THE ROOT OF A FUNCTION func KNOWN TO
!     LIE BETWEEN X1 AND X2. THE ROOT, RETURNED AS ZBRENT, WILL BE
!     REFINED UNTIL ITS ACCURACY IS TOL.
!     PARAMETERS: MAXIMUM ALLOWED NUMBER OF ITERATIONS
!
! IN  func       : FONCTION func
! IN  X1, X2  : INTERVELLE DE RECHERCHE
! IN  TOL     : PRECISION ABSOLUE : LA SOLUTION X EST TELLE QUE func(X)<TOL
! IN  ITMAX   : NOMBRE D'ITERATIONS MAXIMUM
! OUT ZBRENT  : ZERO DE func
! OUT IRET    : CODE RETOUR : IRET = 0 : OK
!             :               IRET = 1 : NITER INSUFFISANT OU AUTRE PB
! OUT ITER    : NOMBRE D'ITERATIONS EFFECTUEES
! ----------------------------------------------------------------------
!
    eps=r8prem()
    iret=0
    iter=0
    a=x1
    b=x2
    fa=func(a)
    fb=func(b)
!
    if (fa .gt. 0.d0 .and. fb .gt. 0.d0 .or. fa .lt. 0.d0 .and. fb .lt. 0.d0) then
!
        iret=1
        goto 9999
!
    endif
!
    c=b
    fc=fb
!
    do 11 iter = 1, itmax
!
        if (fb .gt. 0.d0 .and. fc .gt. 0.d0 .or. fb .lt. 0.d0 .and. fc .lt. 0.d0) then
!         RENAME A, B, C AND ADJUST BOUNDING INTERVAL D.
            c=a
            fc=fa
            d=b-a
            e=d
        endif
!
        if (abs(fc) .lt. abs(fb)) then
            a=b
            b=c
            c=a
            fa=fb
            fb=fc
            fc=fa
        endif
!
!       CONVERGENCE CHECK.
        tol1=2.d0*eps*abs(b)
        xm=0.5d0*(c-b)
        if (abs(xm) .le. tol1 .or. abs(fb) .lt. tol) then
            zbrent=b
            goto 9999
        endif
!
        if (abs(e) .ge. tol1 .and. abs(fa) .gt. abs(fb)) then
!         ATTEMPT INVERSE QUADRATIC INTERPOLATION.
            s=fb/fa
            if (a .eq. c) then
                p=2.d0*xm*s
                q=1.d0-s
            else
                q=fa/fc
                r=fb/fc
                p=s*(2.d0*xm*q*(q-r)-(b-a)*(r-1.d0))
                q=(q-1.d0)*(r-1.d0)*(s-1.d0)
            endif
!         CHECK WHETHER IN BOUNDS.
            if (p .gt. 0.d0) q=-q
            p=abs(p)
            if (2.d0*p .lt. min(3.d0*xm*q-abs(tol1*q),abs(e*q))) then
!           ACCEPT INTERPOLATION.
                e=d
                d=p/q
            else
!           INTERPOLATION FAILED, USE BISECTION.
                d=xm
                e=d
            endif
        else
!         BOUNDS DECREASING TOO SLOWLY, USE BISECTION.
            d=xm
            e=d
        endif
!
!       MOVE LAST BEST GUESS TO A.
        a=b
        fa=fb
!
!       EVALUATE NEW TRIAL ROOT.
        if (abs(d) .gt. tol1) then
            b=b+d
        else
            b=b+sign(tol1,xm)
        endif
        fb=func(b)
11  end do
!
    iret=1
    zbrent=b
!
9999  continue
end subroutine
