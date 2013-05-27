subroutine orien1(xp, xq, angl)
    implicit none
    include 'asterc/r8pi.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: xp(*), xq(*), angl(*)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     ORIENTATION D'UN AXE(XQ,XP) DEFINI PAR DEUX POINTS
! ----------------------------------------------------------------------
! IN  : XP     : EXTREMITE INITIALE DE L'AXE
! IN  : XQ     : EXTREMITE FINALE DE L'AXE
! OUT : A B G  : ANGLES D'ORIENTATION DE L'AXE
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    real(kind=8) :: d, pi, r, s, t, zero
!
!-----------------------------------------------------------------------
    pi = r8pi()
    zero = 0.d0
!
    r = xq(1) - xp(1)
    s = xq(2) - xp(2)
    t = xq(3) - xp(3)
    d = sqrt( r*r + s*s )
!
    angl(3) = zero
    if (d .ne. zero) then
        angl(1) = atan2(s,r)
        angl(2) = -atan2(t,d)
    else
        angl(1) = zero
        if (t .eq. zero) then
            call u2mess('F', 'UTILITAI3_39')
        else if (t .lt. zero) then
            angl(2) = pi / 2.d0
        else
            angl(2) = -pi / 2.d0
        endif
    endif
!
end subroutine
