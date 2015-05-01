subroutine lglpmv(cumul, n, a, x, y)
!
    implicit       none
    character(len=*) :: cumul
    integer :: n
    real(kind=8) :: a(6, 6), x(6), y(6)
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DU PRODUIT MATRICE-VECTEUR --------------------------
! ======================================================================
! IN  : CUMUL  : K* :   ON CUMULE OU NON DANS LE VECTEUR RESULTAT Y ----
! --- : CUMUL = 'ZERO' ON MET Y A ZERO AVANT DE COMMENCER --------------
! --- : CUMUL = 'CUMU' ON ACCUMULE DANS Y ------------------------------
! --- : N      : DIMENSION REELLE DES MATRICES -------------------------
! --- : DIM    : DIMENSION EFFECTIVE DES MATRICES ----------------------
! --- : A      : MATRICE A ---------------------------------------------
! --- : X      : VECTEUR X ---------------------------------------------
! OUT : Y      : VECTEUR Y = A * X -------------------------------------
! ======================================================================
    integer :: i, j
! ======================================================================
    if (cumul .eq. 'ZERO') then
        do 1 i = 1, n
            y(i) = 0.0d0
 1      continue
    endif
! ======================================================================
    do 3 j = 1, n
        do 2 i = 1, n
            y(i) = y(i) + a(i,j) * x(j)
 2      continue
 3  continue
! ======================================================================
end subroutine
