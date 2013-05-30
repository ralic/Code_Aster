subroutine i3rpqp(pto, e1, e2, e3, pt,&
                  nbpt)
    implicit none
!
    integer :: nbpt
    real(kind=8) :: pto(*), e1(*), e2(*), e3(*), pt(3, *)
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     COORDONEES DES POINTS DE PT DANS LE REPERE (PTO,E1,E2,E3)
!     ------------------------------------------------------------------
! IN  PTO  : R : TABLE(1..3) : ORIGINE
! IN  NBPT : I : NOMBRE DE POINTS
! IN  E1   : R : TABLE(1..3) : VECTEUR 1
! IN  E2   : R : TABLE(1..3) : VECTEUR 2
! IN  E3   : R : TABLE(1..3) : VECTEUR 3
! VAR PT X : R : TABLE(1..3,1..NBPT) : POINT A TRAITER
!     ------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: x(3), c, zero
!
!======================================================================
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.0d0
    do 20, j = 1, nbpt, 1
    do 21, i = 1, 3, 1
    x(i) = pt(i,j) - pto(i)
21  continue
    c = zero
    do 22, i = 1, 3, 1
    c = c + e1(i)*x(i)
22  continue
    pt(1,j) = c
    c = zero
    do 23, i = 1, 3, 1
    c = c + e2(i)*x(i)
23  continue
    pt(2,j) = c
    c = zero
    do 24, i = 1, 3, 1
    c = c + e3(i)*x(i)
24  continue
    pt(3,j) = c
    20 end do
end subroutine
