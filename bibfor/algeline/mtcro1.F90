subroutine mtcro1(n, a, nmax, x)
    implicit none
!
    include 'asterfort/u2mess.h'
    integer :: nmax, n
    real(kind=8) :: a(nmax, *), x(*)
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
!     ROUTINE UTILITAIRE POUR RESOUDRE UNE DES EQUATIONS DU SYSTEME
!     A*X = B
!     OPERATEUR APPELANT : MTCROG
! ----------------------------------------------------------------------
!     RESOLUTION D UN SYSTEME LINEAIRE AX = B PAR LA METHODE DE CROUT
!     POUR UNE MATRICE A QUELCONQUE DE DIMENSION N*N
!     SI B EST DE DIMENSION N*1, IL S AGIT D UN SIMPLE SYSTEME
!     LINEAIRE. SI B EST DE DIMENSION N*N, IL S AGIT DE L INVERSION
!     D UNE MATRICE
! ----------------------------------------------------------------------
! IN  : M      : NOMBRE DE LIGNES EFFECTIVES DE A
! IN  : A      : MATRICE A DE DIMESION NMAX*N. A EST UNE MATRICE
!                TRIANGULAIRE INFERIEURE, NON UNITAIRE.
! IN  : NMAX   : PREMIERE DIMENSION DU TABLEAU A
! IN/OUT: X    : VECTEUR DE DIMESION SUPERIEURE OU EGALE A N
!                X CONTIENT EN ENTREE LES N ELEMENTS DU VECTEUR B, ET
!                EN SORTIE, LA SOLUTION X
! ----------------------------------------------------------------------
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    data zero    /0.d0/
! ----------------------------------------------------------------------
!
!
    if (n .lt. 0 .or. nmax .lt. 1 .or. nmax .lt. n) then
        call u2mess('A', 'ALGELINE2_12')
!
    else
        do 20, j = 1, n
        if (x( j ) .ne. zero) then
            x( j ) = x( j )/a( j, j )
            do 10, i = j + 1, n
            x( i ) = x( i ) - x( j )*a( i, j )
10          continue
        endif
20      continue
    endif
!
end subroutine
