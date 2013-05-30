subroutine btsig(lonlig, loncol, jacgau, bmat, sigma,&
                 bsigma)
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
    real(kind=8) :: jacgau, bmat(loncol, 1), sigma(1), bsigma(1)
    integer :: i, j, loncol, lonlig
    real(kind=8) :: valbsi
!-----------------------------------------------------------------------
! --- CALCUL DU PRODUIT (BT)*(SIGMA) ,
! --- AVEC LES NOTATIONS DE LA ROUTINE , CA DONNE :
! ---       (BSIGMA) = (BMAT)*(SIGMA)*JACGAU
!     ------------------------------------------------------------------
!     IN  LONLIG  : LONGUEUR D'UNE LIGNE DE (B), SOIT NBNO*NBDDL
!     IN  LONCOL  : LONGUEUR D'UNE COLONNE DE (B), SOIT NBSIG
!     IN  JACGAU  : PRODUIT DU JACOBIEN PAR LE POIDS AU POINT
!                   D'INTEGRATION COURANT
!     IN  BMAT    : MATRICE (B) AU POINT D'INTEGRATION COURANT
!     IN  SIGMA   : VECTEUR DES CONTRAINTES AU POINT D'INTEGRATION
!                   COURANT
!     OUT BSIGMA  : VECTEUR (BT)*(SIGMA)*JACGAU
!     ------------------------------------------------------------------
!
    do 10 i = 1, lonlig
        valbsi = 0.0d0
        do 20 j = 1, loncol
            valbsi = valbsi + bmat(j,i)*sigma(j)
20      continue
!
        bsigma(i) = bsigma(i) + valbsi*jacgau
10  end do
!
end subroutine
