subroutine mtcro3(m, n, a, nmax, x,&
                  y)
    implicit none
!
    include 'asterfort/u2mess.h'
    integer :: nmax, m, n
    real(kind=8) :: a(nmax, *), x(*), y(*)
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
!     ROUTINE UTILITAIRE POUR CALCULER Y = Y - A*X,
!     OPERATEUR APPELANT : MTCROG
! ----------------------------------------------------------------------
!     RESOLUTION D UN SYSTEME LINEAIRE AX = B PAR LA METHODE DE CROUT
!     POUR UNE MATRICE A QUELCONQUE DE DIMENSION N*N
!     SI B EST DE DIMENSION N*1, IL S AGIT D UN SIMPLE SYSTEME
!     LINEAIRE. SI B EST DE DIMENSION N*N, IL S AGIT DE L INVERSION
!     D UNE MATRICE
! ----------------------------------------------------------------------
! IN  : M      : NOMBRE DE LIGNES EFFECTIVES DE A
! IN  : N      : NOMBRE DE COLONNES EFFECTIVES DE A
! IN  : A      : MATRICE A DE DIMESION NMAX*N
! IN  : NMAX   : PREMIERE DIMENSION DU TABLEAU A
! IN  : X      : VECTEUR DE DIMESION SUPERIEURE OU EGALE A N
! IN/OUT: Y    : VECTEUR DE DIMESION SUPERIEURE OU EGALE A M
! ----------------------------------------------------------------------
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    data zero    /0.d0/
! ----------------------------------------------------------------------
!
!
    if (( m.le.0 ) .and. ( n.le.0 )) then
        call u2mess('A', 'ALGELINE2_13')
    endif
!
    do 20, j = 1, n
    if (x( j ) .ne. zero) then
        do 10, i = 1, m
        y( i ) = y( i ) - x( j )*a( i, j )
10      continue
    endif
    20 end do
!
!
end subroutine
