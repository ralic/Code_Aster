subroutine lcptmv(a, x, y)
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
!                                                             T
!       PRODUIT  MATRICES CARRE TRANSPOSEE . VECTEUR  Y =  (A)  . X
!       IN  A      :  MATRICE
!       IN  X      :  VECTEUR
!       OUT Y      :  VECTEUR RESULTAT
!       ----------------------------------------------------------------
    integer :: n, nd, i, j
    real(kind=8) :: a(6, 6), x(6), y(6)
    common /tdim/   n , nd
!
    do 1 i = 1, n
        y(i) = 0.d0
 1  continue
    do 2 i = 1, n
        do 2 j = 1, n
            y(i) = y(i) + a(j,i) * x(j)
 2      continue
end subroutine
