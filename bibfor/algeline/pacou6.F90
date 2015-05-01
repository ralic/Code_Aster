subroutine pacou6(r, qt, n, i, a,&
                  b)
    implicit none
! ---------------------------------------------------------------------
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
! ARGUMENTS
! ---------
#include "jeveux.h"
    integer :: n, i
    real(kind=8) :: a, b, r(n, *), qt(n, *)
! ---------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: j
    real(kind=8) :: c, fact, s, w, y
!-----------------------------------------------------------------------
    if (abs(a) .le. 1.0d-30) then
        c = 0.0d0
        s = sign(1.0d0,b)
!
    else if (abs(a) .gt. abs(b)) then
        fact = b/a
        c = sign ( 1.0d0/sqrt(1.0d0+fact*fact), a )
        s = fact*c
!
    else
        fact = a/b
        s = sign ( 1.0d0/sqrt(1.0d0+fact*fact), b )
        c = fact*s
!
    endif
!
    do 11 j = 1, n
        y = r(i,j)
        w = r(i+1,j)
        r(i,j) = c*y - s*w
        r(i+1,j) = s*y + c*w
11  end do
!
    do 12 j = 1, n
        y = qt(i,j)
        w = qt(i+1,j)
        qt(i,j) = c*y - s*w
        qt(i+1,j) = s*y + c*w
12  end do
!
end subroutine
