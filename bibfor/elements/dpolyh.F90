function dpolyh(n, a, x)
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
!     EVALUE LE POLYNOME Y = A1 + A2 * X + ... + A(N+1) * X**N
!
! IN  N : DEGRE DU POLYNOME
! IN  A : COEFFICIENT DU POLYNOME
! IN  X : VALEUR DE X OU EST EVALUE LE POLYNOME
!
! OUT DPOLYH : EVALUATION DU POLYNOME A X
!
#include "asterfort/assert.h"
    integer :: i, n
    real(kind=8) :: dpolyh, a(*), x
!
    if (n .gt. 0) then
!     LE POLYNOME EST DE DIMENSION SUPERIEUR A 0
        dpolyh = a(n+1)
        do 10, i = n, 1, -1
        dpolyh = dpolyh*x + a(i)
10      continue
    else if (n.eq.0) then
!     LE POLYNOME EST DE DIMENSION 0 DONC UNE CONSTANTE
        dpolyh = a(1)
    else
        ASSERT(.false.)
    endif
!
end function
