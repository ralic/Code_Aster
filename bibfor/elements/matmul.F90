subroutine matmul(a, b, n1, n2, n3,&
                  ab)
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
!     REALISE LA MULTIPLICATION DE DEUX MATRICES
!
! IN  A : MATRICE
! IN  B : MATRICE
! IN N1 : DIMENSION DE MATRICE
! IN N2 : DIMENSION DE MATRICE
! IN N3 : DIMENSION DE MATRICE
!
! OUT AB : PRODUIT DES MATRICES A x B
!
!
#include "asterfort/r8inir.h"
    integer :: n1, n2, n3, i, j, k
!      REAL*8 A(N1,*),B(N2,*),AB(N1,*)
    real(kind=8) :: a(n1, n2), b(n2, n3), ab(n1, n3)
!
    call r8inir(n1*n3, 0.d0, ab, 1)
!
    do 30, k = 1,n3
    do 20, j = 1,n2
    do 10, i = 1,n1
    ab(i,k) = ab(i,k) + a(i,j)*b(j,k)
10  continue
20  continue
    30 end do
!
end subroutine
