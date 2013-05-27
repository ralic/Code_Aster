function r8sqrt(a, b)
    implicit none
    real(kind=8) :: a, b
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DE SQRT(A**2+B**2) SANS UNDER OU OVERFLOW
!-----------------------------------------------------------------------
! IN  : A    : PREMIER PARAMETRE.
!     : B    : SECOND PARAMETRE.
! OUT : HYPOT: SQRT(A**2+B**2).
!-----------------------------------------------------------------------
    real(kind=8) :: p, r, s, t, u
!
!-----------------------------------------------------------------------
    real(kind=8) :: r8sqrt
!-----------------------------------------------------------------------
    p = max(abs(a),abs(b))
    if (p .eq. 0.0d0) goto 20
    r = (min(abs(a),abs(b))/p)**2
10  continue
    t = 4.0d0 + r
    if (t .eq. 4.0d0) goto 20
    s = r/t
    u = 1.0d0 + 2.0d0*s
    p = u*p
    r = (s/u)**2*r
    goto 10
20  continue
    r8sqrt = p
end function
