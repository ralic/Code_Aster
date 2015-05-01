subroutine cumuma(i, j, pmat, coef, gmat)
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
! FONCTION: CUMULE LES ELEMENTS DE LA MATRICE PMAT, MULTIPLIES PAR COEF,
!           A CEUX DE LA MATRICE GMAT, SELON LA FORMULE:
! GMAT((I-1)*6+I1,(J-1)*6+J1) = GMAT((I-1)*6+I1,(J-1)*6+J1) +
!                               PMAT(I1,J1)
!
!     IN  : I,J       : POINTEURS SUR LA MATRICE GMAT
!           PMAT      : MATRICE D'ORDRE 6
!           COEF      : SCALAIRE
!
!     OUT : GMAT      : MATRICE D'ORDRE 18
! ------------------------------------------------------------------
    implicit none
    real(kind=8) :: pmat(6, 6), gmat(18, 18)
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, ii, j, j1, j2
    integer :: jj
    real(kind=8) :: coef
!-----------------------------------------------------------------------
    i1 = (i-1) * 6
    j1 = (j-1) * 6
    do 12 j2 = 1, 6
        jj = j1 + j2
        do 11 i2 = 1, 6
            ii = i1 + i2
            gmat(ii,jj) = gmat(ii,jj) + coef*pmat(i2,j2)
11      end do
12  end do
end subroutine
