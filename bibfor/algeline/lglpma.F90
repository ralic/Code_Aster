subroutine lglpma(n, a, b, c)
!
    implicit    none
    integer :: n
    real(kind=8) :: a(6, 6), b(6, 6), c(6, 6)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DU PRODUIT MATRICIEL --------------------------------
! ======================================================================
! IN  : N      : DIMENSION REELLE DES MATRICES -------------------------
! --- : DIM    : DIMENSION EFFECTIVE DES MATRICES ----------------------
! --- : A      : MATRICE A ---------------------------------------------
! --- : B      : MATRICE B ---------------------------------------------
! OUT : C      : MATRICE C = A * B -------------------------------------
! ======================================================================
    integer :: i, j, k
! ======================================================================
    real(kind=8) :: v
    do 1 i = 1, n
        do 2 j = 1, n
            v = 0.0d0
            do 3 k = 1, n
                v = v + a(i,k)*b(k,j)
 3          continue
            c(i,j) = v
 2      continue
 1  continue
! ======================================================================
end subroutine
