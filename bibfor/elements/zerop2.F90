subroutine zerop2(b, c, x, n)
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
    implicit none
    real(kind=8) :: b, c, x(2)
    integer :: n
!
! ----------------------------------------------------------------------
! RESOLUTION D'UN POLYNOME DE DEGRE 2 : X**2 + B X + C = 0
! ----------------------------------------------------------------------
! IN  B,C   COEFFICIENTS DU POLYNOME
! OUT X     RACINES DANS L'ORDRE DECROISSANT
! OUT N     NOMBRE DE RACINES
! ----------------------------------------------------------------------
!
    real(kind=8) :: delta, rac
!
    delta = b**2 - 4*c
    if (delta .lt. 0) then
        n = 0
    else
        n = 2
        rac = sqrt(delta)
        x(1) = (-b+rac)/2
        x(2) = (-b-rac)/2
    endif
!
end subroutine
