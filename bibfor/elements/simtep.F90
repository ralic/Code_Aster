subroutine simtep(fami, nno, ndim, nbsig, npg,&
                  ipoids, ivf, idfde, xyz, depl,&
                  instan, repere, mater, nharm, sigma)
    implicit none
!-----------------------------------------------------------------------
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
!      SIGVMC   -- CALCUL DES  CONTRAINTES 'VRAIES'
!                  POUR LE CALCUL DE L'ENERGIE POTENTIELLE
!                  (I.E.  1/2*SIGMA_MECA - SIGMA_THERMIQUES)
!                  AUX POINTS D'INTEGRATION POUR LES ELEMENTS
!                  ISOPARAMETRIQUES
!
!   ROUTINE IDENTIQUE A SIGVMC MAIS 1/2 POUR LE TERME DE MECANIQUE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    NI(1)          IN     R        FONCTIONS DE FORME
!    DNIDX(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
!                                   / X SUR L'ELEMENT DE REFERENCE
!    DNIDY(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
!                                   / Y SUR L'ELEMENT DE REFERENCE
!    DNIDZ(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
!                                   / Z SUR L'ELEMENT DE REFERENCE
!    POIDS(1)       IN     R        POIDS D'INTEGRATION
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
!                                   L'ELEMENT
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    MATER          IN     I        MATERIAU
!    NHARM          IN     R        NUMERO D'HARMONIQUE
!    SIGMA(1)       OUT    R        CONTRAINTES AUX POINTS D'INTEGRATION
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'jeveux.h'
    include 'asterfort/sigmmc.h'
    include 'asterfort/sigtmc.h'
    integer :: ipoids, ivf, idfde
    character(len=*) :: fami
    real(kind=8) :: xyz(1), depl(1), repere(7), sigma(1)
    real(kind=8) :: instan, nharm
! -----  VARIABLES LOCALES
    integer :: i, mater, nbsig, ndim, nno, npg
    character(len=16) :: k16bid
    real(kind=8) :: sigth(162), zero
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    k16bid = ' '
!
    do 10 i = 1, nbsig*npg
        sigma(i) = zero
10  end do
!
! --- CALCUL DES CONTRAINTES MECANIQUES AUX POINTS D'INTEGRATION
!      ---------------------------------------------------------
    call sigmmc(fami, nno, ndim, nbsig, npg,&
                ipoids, ivf, idfde, xyz, depl,&
                instan, repere, mater, nharm, sigma)
!
! --- CALCUL DES CONTRAINTES THERMIQUES AUX POINTS D'INTEGRATION
!      ---------------------------------------------------------
    call sigtmc(fami, nno, ndim, nbsig, npg,&
                zr(ivf), xyz, instan, mater, repere,&
                k16bid, sigth)
!
! --- CALCUL DES CONTRAINTES TOTALES AUX POINTS D'INTEGRATION
!      ---------------------------------------------------------
    do 20 i = 1, nbsig*npg
        sigma(i) = 0.5d0*sigma(i) - sigth(i)
20  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
