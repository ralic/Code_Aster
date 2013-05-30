subroutine mtcrou(a, b, nmax, n, nbscmb,&
                  l, d)
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
    implicit none
    real(kind=8) :: a(nmax, nmax), b(nmax, nbscmb), l(n, n), d(n)
!     ------------------------------------------------------------------
!     RESOLUTION PAR LA METHODE DE CROUT D'UN SYSTEME LINEAIRE
!     ------------------------------------------------------------------
! VAR A      : R8 : MATRICE CARREE PLEINE
! VAR B      : R8 : TABLEAU BI-INDICES DE REELS
!               EN ENTREE : LES SECONDS MEMBRES
!               EN SORTIE : LES SOLUTIONS
! IN  NMAX   : IS : DIM MAXI DE LA MATRICE
! IN  N      : IS : ORDRE DE LA MATRICE
! IN  NBSCMB : IS : NOMBRE DE SECOND MEMBRE
!     ------------------------------------------------------------------
    real(kind=8) :: zero, s
!
!-----------------------------------------------------------------------
    integer :: i, is, j, k, n, nbscmb, nmax
!
!-----------------------------------------------------------------------
    zero = 0.d0
    do 1 i = 1, n
        do 2 j = 1, i-1
            s = zero
            do 3 k = 1, j-1
                s = s + l(i,k)*d(k)*l(j,k)
 3          continue
            l(i,j) = (a(i,j)-s)/d(j)
 2      continue
        s = zero
        do 4 k = 1, i-1
            s = s + l(i,k)*l(i,k)*d(k)
 4      continue
        d(i) = a(i,i)-s
 1  end do
!
!   BOUCLE SUR LES SECONDS MEMBRES
!
    do 5 is = 1, nbscmb
!
!  DESCENTE
!
        do 6 i = 1, n
            s = zero
            do 7 k = 1, i-1
                s = s + l(i,k)*b(k,is)
 7          continue
            b(i,is) = b(i,is)-s
 6      end do
!
!  DIVISION PAR LA DIAGONALE
!
        do 10 i = 1, n
            b(i,is) = b(i,is)/d(i)
10      end do
!
!  REMONTEE
!
        do 8 i = n, 1, -1
            s = zero
            do 9 k = i+1, n
                s = s + l(k,i)*b(k,is)
 9          continue
            b(i,is) = b(i,is)-s
 8      end do
 5  end do
end subroutine
