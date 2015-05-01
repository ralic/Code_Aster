subroutine wprest(a, x, n, m, y)
    implicit none
!
    complex(kind=8) :: y(*)
    real(kind=8) :: a(n, *), x(*)
    integer :: n, m
!     --------------------------------------------------------------
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
!     RESTITUTION DES VECTEUR PROPRES DU PB QUADRATIQUE
!     IE :         Y := A*X
!     --------------------------------------------------------------
! IN  A : R : MATRICE DES VECTEUR DE LANCZOS
! IN  X : C : MATRICE DES VECTEUR PROPRES DU PB REDUIT
! IN  N : I : TAILLE DES VECTEURS PROPRES DU PB QUADRATIQUE
! IN  M : I : TAILLE DES VECTEURS PROPRES DU PB REDUIT
! IN  Y : C : MATRICE DES VECTEUR PROPRES DU PB QUADRATIQUE
!     --------------------------------------------------------------
    complex(kind=8) :: cval, czero
    integer :: i, j
!     --------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
    czero = dcmplx(0.0d0,0.0d0)
    do 100, i = 1, n, 1
    cval = czero
    k = 1
    do 110, j = 1, m, 1
    cval = cval + a(i,j)*dcmplx(x(k),x(k+1))
    k = k+2
110  continue
    y(i) = cval
    100 end do
!
end subroutine
