subroutine dbudef(depl, b, d, nbsig, nbinco,&
                  sigma)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!.======================================================================
    implicit none
!
!      DBUDEF   -- CALCUL DU VECTEUR DES CONTRAINTES AUX POINTS
!                  D'INTEGRATION SUR L'ELEMENT COURANT
!                  EN FAISANT LE PRODUIT D*B*DEPL
!
!   ARGUMENT        E/S  TYPE         ROLE
!    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
!                                   L'ELEMENT
!    B(NBSIG,1)     IN     R        MATRICE (B) RELIANT LES
!                                   DEFORMATIONS DU PREMIER ORDRE
!                                   AUX DEPLACEMENTS AU POINT
!                                   D'INTEGRATION COURANT
!    D(NBSIG,1)     IN     R        MATRICE DE HOOKE
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NBINCO         IN     I        NOMBRE D'INCONNUES DE L'ELEMENT
!    SIGMA(1)       OUT    R        CONTRAINTES AU POINT D'INTEGRATION
!                                   COURANT
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    real(kind=8) :: depl(1), b(nbsig, 1), d(nbsig, 1), sigma(1)
! -----  VARIABLES LOCALES
    real(kind=8) :: eps(6)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATION :
!     ----------------
!-----------------------------------------------------------------------
    integer :: i, j, nbinco, nbsig
    real(kind=8) :: s, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
!
! --- CALCUL DU VECTEUR DES COMPOSANTES DU TENSEUR DES DEFORMATIONS
! --- AU POINT D'INTEGRATION COURANT
!      -----------------------------
    do 10 i = 1, nbsig
!
        s = zero
!
        do 20 j = 1, nbinco
            s = s + depl(j)*b(i,j)
20      continue
!
        eps(i) = s
10  end do
!
! --- VECTEUR DES CONTRAINTES
!      ----------------------
    do 30 i = 1, nbsig
!
        s = zero
!
        do 40 j = 1, nbsig
            s = s + eps(j)*d(i,j)
40      continue
!
        sigma(i) = s
30  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
