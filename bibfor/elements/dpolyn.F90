function dpolyn(n, coef, x)
!
    implicit none
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
! ======================================================================
!
!     EVALUE LA DERIVEE DU POLYNOME
!     Y = A1 + A2 * X + ... + A(N+1) * X**N
!     SOIT DY/DX = A2 + ... + N * A(N+1) * X**(N-1)
!
! IN  N : DEGRE DU POLYNOME
! IN  A : COEFFICIENT DU POLYNOME
! IN  X : VALEUR DE X OU EST EVALUE LE POLYNOME
!
! OUT DPOLYN : EVALUATION DE LA DERIVEE DU POLYNOME A X
!
    integer :: n, i
    real(kind=8) :: x, coef(n+1), dpolyn
!
!     INITIALISATION DE LA VARIABLE MAIS AUSSI VALEUR DE
!     LA DERIVEE POUR UN POLYNOME DE DEGRE 0
    dpolyn=0.d0
!
    if (n .gt. 0) then
        do 10, i = 1, n
        dpolyn = x*dpolyn + (n-i-1)*coef(n-i)
10      continue
    endif
!
end function
