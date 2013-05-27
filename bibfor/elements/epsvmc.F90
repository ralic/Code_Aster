subroutine epsvmc(fami, nno, ndim, nbsig, npg,&
                  ipoids, ivf, idfde, xyz, depl,&
                  instan, mater, repere, nharm, option,&
                  epsm)
    implicit none
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!      EPSVMC   -- CALCUL DES  DEFORMATIONS MECANIQUES
!                  (I.E. EPS_TOTALES - EPS_THERMIQUES )
!                  AUX POINTS D'INTEGRATION POUR LES ELEMENTS
!                  ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K4       TYPE DE FAMILLE DE POINT DE GAUSS
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
!    IVF            IN     I        POINTEUR FONCTIONS DE FORME
!    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
!                                   L'ELEMENT
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    MATER          IN     I        MATERIAU
!    NHARM          IN     R        NUMERO D'HARMONIQUE
!    OPTION         IN     K16      OPTION DE CALCUL
!    EPSM(1)        OUT    R        DEFORMATIONS MECANIQUES AUX
!                                   POINTS D'INTEGRATION
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'jeveux.h'
    include 'asterfort/dmatmc.h'
    include 'asterfort/eps1mc.h'
    include 'asterfort/eps2mc.h'
    include 'asterfort/epthmc.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/u2mesk.h'
    character(len=16) :: option
    character(len=4) :: fami
    real(kind=8) :: xyz(1), depl(1), epsm(1), repere(7)
    real(kind=8) :: instan, nharm
    integer :: idfde, ipoids, ivf, mater, nbsig, ndim, nno, npg
! -----  VARIABLES LOCALES
    character(len=8) :: phenom
    real(kind=8) :: epsth(162), eps2(162), xyzgau(3), d(4, 4)
    real(kind=8) :: zero, un, deux
    integer :: i, igau, icodre
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
!
    do 10 i = 1, nbsig*npg
        epsm(i) = zero
        eps2(i) = zero
        epsth(i)= zero
10  end do
!
!
! --- CALCUL DES DEFORMATIONS DU PREMIER ORDRE
! --- AUX POINTS D'INTEGRATION :
!      -----------------------
    call eps1mc(nno, ndim, nbsig, npg, ipoids,&
                ivf, idfde, xyz, depl, nharm,&
                epsm)
!
! ---   CALCUL DES DEFORMATIONS DU SECOND ORDRE AUX POINTS
! ---   D'INTEGRATION POUR LES GRANDES TRANSFORMATIONS :
!       ----------------------------------------------
    if (option(4:4) .eq. 'G') then
        call eps2mc(nno, ndim, nbsig, npg, ipoids,&
                    ivf, idfde, xyz, depl, eps2)
    endif
!
! --- CALCUL DES DEFORMATIONS THERMIQUES AUX POINTS D'INTEGRATION
! --- (AJOUTEES AUX DEFORMATIONS DE RETRAIT ENDOGENE/DESSICCATION)
!      ----------------------------------------------------------
    call rccoma(mater, 'ELAS', 1, phenom, icodre)
    if (phenom(1:8) .ne. 'ELAS_MET') then
        call epthmc(fami, nno, ndim, nbsig, npg,&
                    zr(ivf), xyz, repere, instan, mater,&
                    option, epsth)
    else if (option(1:4).eq.'EPME'.or.option(1:4).eq.'EPMG') then
        call u2mesk('F', 'ELEMENTS_15', 1, phenom)
    endif
!
! --- CALCUL DES DEFORMATIONS MECANIQUES AUX POINTS D'INTEGRATION :
!      ----------------------------------------------------------
    do 30 i = 1, nbsig*npg
        epsm(i) = epsm(i) + eps2(i)
30  end do
!
    if (option(1:4) .eq. 'EPME' .or. option(1:4) .eq. 'EPMG') then
        do 40 i = 1, nbsig*npg
            epsm(i) = epsm(i) - epsth(i)
40      continue
    endif
!
! --- CAS DES CONTRAINTES PLANES, ON CALCULE EPSZZ A PARTIR
! --- DE SIGZZ = 0 :
!     ------------
    if (lteatt(' ','C_PLAN','OUI')) then
!
! ---   BOUCLE SUR LES POINTS D'INTEGRATION :
!       -----------------------------------
        do 50 igau = 1, npg
!
!  --      COORDONNEES AU POINT D'INTEGRATION
!  --      COURANT
!          -------
            xyzgau(1) = zero
            xyzgau(2) = zero
            xyzgau(3) = zero
!
!  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
!  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
!          -------------------------------------------------
            call dmatmc(fami, 'DP', mater, instan, '+',&
                        igau, 1, repere, xyzgau, nbsig,&
                        d)
!
            if (option(1:4) .eq. 'EPME' .or. option(1:4) .eq. 'EPMG') then
                epsm(nbsig*(igau-1)+3) = -un/d(3,3)* ( d(3,1)*epsm( nbsig*(igau-1)+1) + d(3,2)*ep&
                                         &sm(nbsig*(igau-1)+2) + d(3,4)*epsm(nbsig*(igau-1)+4)*de&
                                         &ux)
            else
                epsm(nbsig*(igau-1)+3) = -un/d(3,3)* (d(3,1)*(epsm( nbsig*(igau-1)+1)-epsth(nbsig&
                                         &*(igau-1)+1)) +d(3,2)*( epsm(nbsig*(igau-1)+2)-epsth(nb&
                                         &sig*(igau-1)+2)) +d(3,4)*(epsm(nbsig*(igau-1)+4) -epsth&
                                         &(nbsig*(igau-1)+ 4))*deux)+epsth(nbsig*(igau-1)+3)
            endif
50      continue
!
! --- CAS DES DEFORMATIONS PLANES,  EPSZZ = 0 :
!     ---------------------------------------
    else if (lteatt(' ','D_PLAN','OUI')) then
!
! ---   BOUCLE SUR LES POINTS D'INTEGRATION :
!       -----------------------------------
        do 70 igau = 1, npg
            epsm(nbsig*(igau-1)+3) = zero
70      continue
!
    endif
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
