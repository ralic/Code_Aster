subroutine epthmc(fami, nno, ndim, nbsig, npg,&
                  ni, xyz, repere, instan, mater,&
                  option, epsith)
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
!      EPTHMC   -- CALCUL DES  DEFORMATIONS THERMIQUES+RETRAIT
!                  AUX POINTS D'INTEGRATION
!                  POUR LES ELEMENTS ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K4       FAMILLE DU POINT DE GAUSS
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    NI(1)          IN     R        FONCTIONS DE FORME
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    MATER          IN     I        MATERIAU
!    OPTION         IN     K16      OPTION DE CALCUL
!    EPSITH(1)      OUT    R        DEFORMATIONS THERMIQUES
!                                   AUX POINTS D'INTEGRATION
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'asterfort/epstmc.h'
    include 'asterfort/lteatt.h'
    character(len=*) :: fami
    character(len=16) :: k16bid, option
    integer :: ndim
    real(kind=8) :: ni(1), epsith(1)
    real(kind=8) :: instan, repere(7), xyz(*)
! -----  VARIABLES LOCALES
    real(kind=8) :: epsth(6), epshy(6), epsse(6), xyzgau(3), epsan(6), epspt(6)
    character(len=16) :: optio2, optio3, optio4, optio5
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
!-----------------------------------------------------------------------
    integer :: i, idim, igau, mater, nbsig, ndim2, nno
    integer :: npg
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    zero = 0.0d0
    k16bid = ' '
!
    do 10 i = 1, nbsig*npg
        epsith(i) = zero
10  end do
!
    ndim2 = ndim
    if (lteatt(' ','FOURIER','OUI')) then
        ndim2 = 2
    endif
!
! --- CALCUL DES CONTRAINTES D'ORIGINE THERMIQUE :
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
    do 20 igau = 1, npg
!
        xyzgau(1) = zero
        xyzgau(2) = zero
        xyzgau(3) = zero
!
        do 30 i = 1, nno
            do 40 idim = 1, ndim2
                xyzgau(idim) = xyzgau(idim) + ni(i+nno*(igau-1))*xyz( idim+ndim2*(i-1))
40          continue
30      continue
!
!  --      CALCUL DES DEFORMATIONS THERMIQUES  AU POINT D'INTEGRATION
!  --      COURANT
!          -------
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, mater, k16bid,&
                    epsth)
!
!  --      DEFORMATIONS THERMIQUES SUR L'ELEMENT
!          -------------------------------------
        do 50 i = 1, nbsig
            epsith(i+nbsig*(igau-1)) = epsith(i+nbsig*(igau-1)) + epsth(i)
50      continue
!
        optio2 = option(1:9) // '_HYDR'
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, mater, optio2,&
                    epshy)
        optio3 = option(1:9) // '_SECH'
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, mater, optio3,&
                    epsse)
        optio4 = option(1:9) // '_EPSA'
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, mater, optio4,&
                    epsan)
        optio5 = option(1:9) // '_PTOT'
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, mater, optio5,&
                    epspt)
!
!  --     DEFORMATIONS DE RETRAIT SUR L'ELEMENT
!         -------------------------------------
        do 60 i = 1, nbsig
            epsith(i+nbsig*(igau-1)) = epsith(&
                                       i+nbsig*(igau-1)) + epshy(i) + epsse(i) + epsan(i) + epspt&
                                       &(i&
                                       )
60      continue
!
20  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
