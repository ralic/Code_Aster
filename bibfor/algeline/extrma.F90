subroutine extrma(amatst, nlig, ncol, nmat, amat)
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
! FONCTION: EXTRAIT LA MATRICE AMAT(I,J) A NLIG LIGNES ET NCOL COLONNES
!           DU TABLEAU AMATST(I,J,NMAT)
!
!     IN  : AMATST    : TABLEAU DIMENSIONNE 9,6,6
!           NLIG      : ENTIER
!           NCOL      : ENTIER
!           NMAT      : ENTIER
!
!     OUT : AMAT      : MATRICE DIMENSIONNEE NLIG,NCOL
! ------------------------------------------------------------------
    implicit none
    integer :: ncol, nlig
    real(kind=8) :: amatst(9, 6, 6), amat(nlig, ncol)
    integer :: i, j, nmat
!-----------------------------------------------------------------------
!
    do 2 j = 1, ncol
        do 1 i = 1, nlig
            amat(i,j) = amatst(i,j,nmat)
 1      end do
 2  end do
end subroutine
