subroutine antisy(axial, coef, amat)
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
!
! FONCTION: FORME LA MATRICE ANTISYMETRIQUE 'AMAT' DE VECTEUR AXIAL
!           'AXIAL' ET MULTIPLIE LES ELEMENTS DE LA MATRICE PAR COEF.
!
!     IN  : AXIAL     : VECTEUR D'ORDRE 3
!           COEF      : SCALAIRE
!
!     OUT : AMAT      : MATRICE D'ORDRE 3
! ------------------------------------------------------------------
    implicit none
    real(kind=8) :: axial(3), amat(3, 3)
!
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: coef, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    do 2 j = 1, 3
        do 1 i = 1, 3
            amat(i,j) = zero
 1      end do
 2  end do
    amat(1,2) = -coef * axial(3)
    amat(1,3) = coef * axial(2)
    amat(2,1) = coef * axial(3)
    amat(2,3) = -coef * axial(1)
    amat(3,1) = -coef * axial(2)
    amat(3,2) = coef * axial(1)
end subroutine
