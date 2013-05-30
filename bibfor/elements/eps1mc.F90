subroutine eps1mc(nno, ndim, nbsig, npg, ipoids,&
                  ivf, idfde, xyz, depl, nharm,&
                  eps1)
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
!      EPS1MC   -- CALCUL DES  DEFORMATIONS AUX POINTS D'INTEGRATION
!                  POUR LES ELEMENTS ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    IVF            IN     I        POINTEUR FONCTIONS DE FORME
!    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
!    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
!                                   L'ELEMENT
!    NHARM          IN     R        NUMERO D'HARMONIQUE
!    EPS1(1)        OUT    R        DEFORMATIONS DU PREMIER ORDRE
!                                   AUX POINTS D'INTEGRATION
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'asterfort/bmatmc.h'
    real(kind=8) :: xyz(1), depl(1), eps1(1)
    real(kind=8) :: nharm
! -----  VARIABLES LOCALES
    real(kind=8) :: b(486), jacgau
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
!-----------------------------------------------------------------------
    integer :: i, idfde, igau, ipoids, ivf, j, nbinco
    integer :: nbsig, ndim, nno, npg
    real(kind=8) :: s, undemi, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
    undemi = 0.5d0
    nbinco = ndim*nno
!
    do 10 i = 1, nbsig*npg
        eps1(i) = zero
10  end do
!
! --- CALCUL DES DEFORMATIONS AUX POINTS D'INTEGRATION
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
    do 20 igau = 1, npg
!
!  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU
!  --      PREMIER ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION
!  --      COURANT : (EPS_1) = (B)*(UN)
!          ----------------------------
        call bmatmc(igau, nbsig, xyz, ipoids, ivf,&
                    idfde, nno, nharm, jacgau, b)
!
! ---      CALCUL DU VECTEUR DES COMPOSANTES DU TENSEUR DES
! ---      DEFORMATIONS AU POINT D'INTEGRATION COURANT
!          -------------------------------------------
        do 30 i = 1, nbsig
!
            s = zero
!
            do 40 j = 1, nbinco
                s = s + depl(j)*b((j-1)*nbsig+i)
40          continue
!
            eps1(nbsig*(igau-1)+i) = s
30      continue
!
        do 50 i = 4, nbsig
            eps1(nbsig*(igau-1)+i) = undemi*eps1(nbsig*(igau-1)+i)
50      continue
!
20  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
