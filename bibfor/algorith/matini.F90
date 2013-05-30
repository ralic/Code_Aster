subroutine matini(nlig, ncol, s, mat)
    implicit none
!
    integer :: nlig, ncol
    real(kind=8) :: s, mat(nlig, ncol)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!            INITIALISATION D'UNE MATRICE PAR UN SCALAIRE (RÉEL)
!
!       IN   NLIG    :  NOMBRE DE LIGNES DE LA MATRICE
!       IN   NCOL    :  NOMBRE DE COLONNES DE LA MATRICE
!       IN   S       :  SCALAIRE
!       OUT  MAT     :  MATRICE
!-----------------------------------------------------------------------
!
! NOTE SUR L'UTILISATION DE LA ROUTINE :
! ====================================
! CETTE ROUTINE PERMET D'INITIALISER UNE MATRICE MAIS NE PERMET PAS
! D'INITIALISER UN SOUS-BLOC D'UNE MATRICE. CECI A CAUSE DE LA
! DECLARATION DE LA MATRICE DANS LA PRESENTE ROUTINE.
! SI L'ON SOUHAITE QUAND MEME ECONOMISER DE LA MEMOIRE EN N'INITIALISANT
! QUE LE NECESSAIRE ALORS IL FAUT DONNER AU MINIMUM LE VRAI NOMBRE DE
! LIGNE DE LA MATRICE. A NOTER QUE CELA N'ECONOMISE PAS GRAND CHOSE.
!
!-----------------------------------------------------------------------
!
    integer :: i, j
!
    do 10 j = 1, ncol
        do 20 i = 1, nlig
            mat(i,j) = s
20      continue
10  end do
end subroutine
