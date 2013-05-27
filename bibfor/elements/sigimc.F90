subroutine sigimc(fami, nno, ndim, nbsig, npg,&
                  ni, xyz, instan, mater, repere,&
                  epsini, sigma)
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
!      SIGIMC   -- CALCUL DES  CONTRAINTES INITIALES
!                  AUX POINTS D'INTEGRATION
!                  POUR LES ELEMENTS ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K4       FAMILLE DES POINTS DE GAUSS
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    NI(1)          IN     R        FONCTIONS DE FORME
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    MATER          IN     I        MATERIAU
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    EPSINI(1)      IN     R        VECTEUR DES DEFORMATIONS INITIALES
!    SIGMA(1)       OUT    R        CONTRAINTES INITIALES
!                                   AUX POINTS D'INTEGRATION
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'asterfort/dmatmc.h'
    character(len=4) :: fami
    real(kind=8) :: ni(1), xyz(1), repere(7), epsini(1)
    real(kind=8) :: sigma(1), instan
! -----  VARIABLES LOCALES
    real(kind=8) :: d(36), xyzgau(3)
    character(len=2) :: k2bid
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
!-----------------------------------------------------------------------
    integer :: i, idim, igau, j, mater, nbsig, ndim
    integer :: nno, npg
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    k2bid = '  '
    zero = 0.0d0
!
    do 10 i = 1, nbsig*npg
        sigma(i) = zero
10  end do
!
! --- CALCUL DES CONTRAINTES INITIALES :
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
    do 20 igau = 1, npg
!
!  --      COORDONNEES AU POINT D'INTEGRATION
!  --      COURANT
!          -------
        xyzgau(1) = zero
        xyzgau(2) = zero
        xyzgau(3) = zero
!
        do 30 i = 1, nno
!
            do 40 idim = 1, ndim
                xyzgau(idim) = xyzgau(idim) + ni(i+nno*(igau-1))*xyz( idim+ndim*(i-1))
40          continue
30      continue
!
!  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
!  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
!          -------------------------------------------------
        call dmatmc(fami, k2bid, mater, instan, '+',&
                    igau, 1, repere, xyzgau, nbsig,&
                    d)
!
!  --      CONTRAINTES INITIALES AU POINT D'INTEGRATION COURANT
!          ------------------------------------------------------
        do 50 i = 1, nbsig
            do 60 j = 1, nbsig
                sigma(i+nbsig*(igau-1)) = sigma(&
                                          i+nbsig*(igau-1)) + d(j+(i-1)*nbsig)*epsini(j+nbsig*(ig&
                                          &au-1)&
                                          )
60          continue
50      continue
!
20  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
