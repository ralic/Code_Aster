subroutine orien2(xp, xq, xr, angl)
    implicit none
#include "asterfort/matrot.h"
#include "asterfort/orien1.h"
#include "asterfort/pmavec.h"
#include "asterfort/utmess.h"
    real(kind=8) :: xp(*), xq(*), xr(*), angl(*)
! ----------------------------------------------------------------------
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
!     ORIENTATION D'UN TRIEDRE(XQ,XP,XR) DEFINI PAR TROIS POINTS (X,Y)
! ----------------------------------------------------------------------
! IN  : X..    : COORDONNEES DES POINTS
! OUT : A B G  : ANGLES D'ORIENTATION DE L'AXE
! ----------------------------------------------------------------------
    real(kind=8) :: xpr(3), xpq(3), xxpr(3), mro(3, 3)
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: r, s, zero
!-----------------------------------------------------------------------
    zero = 0.d0
!
    r = zero
    s = zero
    do 10 i = 1, 3
        xpq(i) = xq(i) - xp(i)
        r = r + xpq(i)*xpq(i)
        xpr(i) = xr(i) - xp(i)
        s = s + xpr(i)*xpr(i)
10  end do
    if (r .eq. zero) then
        call utmess('F', 'UTILITAI3_39')
    endif
    if (s .eq. zero) then
        call utmess('F', 'UTILITAI3_39')
    endif
    r = sqrt( r )
    s = sqrt( s )
    call orien1(xp, xq, angl)
    call matrot(angl, mro)
    call pmavec('ZERO', 3, mro, xpr, xxpr)
    if (xxpr(2) .eq. zero .and. xxpr(3) .eq. zero) then
        angl(3) = zero
    else
        angl(3) = atan2 ( xxpr(3) , xxpr(2) )
    endif
!
end subroutine
