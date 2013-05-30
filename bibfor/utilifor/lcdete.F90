subroutine lcdete(a, deta)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       DETERMINANT D UNE MATRICE EN FONCTION DE LA MODELISATION
!       3D   : A = A11, A22, A33, RAC2 A12, RAC2 A13, RAC2 A23
!       D_PLAN OU AXIS A = A11, A22, A33, RAC2 A12
!       IN  A      :  MATRICE
!       OUT LCDETE :  DETERMINANT
!       ----------------------------------------------------------------
    integer :: n, nd
    real(kind=8) :: a(6), deta, invrc2
    common /tdim/   n , nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    invrc2 = 1.d0 / sqrt(2.d0)
!
    if (n .eq. 6) then
        deta = a(1)*a(2)*a(3) + invrc2*a(4)*a(5)*a(6) - ( a(3)*a(4)*a( 4)+a(2)*a(5)*a(5)+a(1)*a(6&
               &)*a(6) )/2.d0
    endif
!
    if (n .eq. 4) then
        deta = a(1)*a(2)*a(3) - a(3)*a(4)*a(4)/2.d0
    endif
!
end subroutine
