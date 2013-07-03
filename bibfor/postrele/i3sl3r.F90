subroutine i3sl3r(a, b, ez, cs)
! aslint: disable=
    implicit none
#include "asterfort/provec.h"
    real(kind=8) :: a(*), b(*), ez(*), cs(3, *)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL D'UNE MATRICE DE PASSAGE EN 3D PLUS TRANSLATION
!     ------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: cstmp(3, 4), dif(3, 4), norma, norme, angle
    real(kind=8) :: p(3, 3), p1(3, 3), r1t(3, 3), r1(3, 3), r2(3, 3)
    real(kind=8) :: ex1(3), ey1(3), ez1(3), ab, dproj
!     ------------------------------------------------------------------
!
    ab = 0.0d0
    norma = sqrt( a(1)**2 + a(2)**2 + a(3)**2 )
    do 10, i = 1, 3, 1
    ex1(i) = a(i) / norma
    ab = ab + (a(i)*b(i))
    10 end do
    angle = acos(ab)
!
    call provec(a, b, ez1)
    norme = sqrt( ez1(1)**2 + ez1(2)**2 + ez1(3)**2 )
    do 12, i = 1, 3, 1
    ez1(i) = ez1(i) / norme
    12 end do
!
    call provec(ez1, ex1, ey1)
    norme = sqrt( ey1(1)**2 + ey1(2)**2 + ey1(3)**2 )
    do 14, i = 1, 3, 1
    ey1(i) = ey1(i) / norme
    14 end do
!
    do 20, i = 1, 3, 1
    r1(1,i) = ex1(i)
    r1(2,i) = ey1(i)
    r1(3,i) = ez1(i)
    r1t(i,1) = ex1(i)
    r1t(i,2) = ey1(i)
    r1t(i,3) = ez1(i)
    20 end do
!
    r2(1,1) = cos(angle)
    r2(1,2) = -sin(angle)
    r2(1,3) = 0
    r2(2,1) = sin(angle)
    r2(2,2) = cos(angle)
    r2(2,3) = 0
    r2(3,1) = 0
    r2(3,2) = 0
    r2(3,3) = 1
!
    do 30, i = 1, 3, 1
    do 32, j = 1, 3, 1
    p1(i,j) = (r1t(i,1)*r2(1,j)) + (r1t(i,2)*r2(2,j)) + (r1t( i,3)*r2(3,j))
32  continue
    30 end do
!
    dproj = 0.0d0
    do 40, i = 1, 3, 1
    do 42, j = 1, 3, 1
    p(i,j) = (p1(i,1)*r1(1,j)) + (p1(i,2)*r1(2,j)) + (p1(i,3)* r1(3,j))
42  continue
    dproj = dproj - (cs(i,1)*ez(i))
    40 end do
    do 50, i = 2, 4, 1
    do 52, j = 1, 3, 1
    dif(j,i) = cs(j,i)-cs(j,1)
52  continue
    50 end do
!
    do 60, i = 2, 4, 1
    do 62, j = 1, 3, 1
    cstmp(j,i) = (p(j,1)*dif(1,i)) + (p(j,2)*dif(2,i)) + (p(j,3)*dif(3,i))
    cs(j,i) = cs(j,i) - dif(j,i) + cstmp(j,i)
62  continue
    60 end do
!
    do 70, i = 1, 4, 1
    do 72, j = 1, 3, 1
    cs(j,i) = cs(j,i) + ( dproj * ez(j) )
72  continue
    70 end do
!
end subroutine
