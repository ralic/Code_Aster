subroutine te0022(option, nomte)
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
!.......................................................................
!
!     BUT: CALCUL DES CONTRAINTES AUX POINTS DE GAUSS
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'SIEF_ELGA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nbsigm.h'
    include 'asterfort/ortrep.h'
    include 'asterfort/sigvmc.h'
    character(len=16) :: nomte, option
    real(kind=8) :: sigma(162), repere(7), instan, nharm
    real(kind=8) :: bary(3)
    integer :: ndim, nno, nnos, npg1, ipoids, ivf, idfde, jgano
    integer :: idim
!
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!-----------------------------------------------------------------------
    integer :: i, icont, idepl, igeom, imate, nbsig
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    instan = zero
    nharm = zero
!
    do 10 i = 1, nbsig*npg1
        sigma(i) = zero
10  end do
!
! ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
!      ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! ---- RECUPERATION DU MATERIAU
!      ------------------------
    call jevech('PMATERC', 'L', imate)
!
! ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
!      ------------------------------------------------------------
!     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
!
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do 150 i = 1, nno
        do 140 idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
140      continue
150  end do
    call ortrep(zi(imate), ndim, bary, repere)
!
! ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
!      --------------------------------------------------
    call jevech('PDEPLAR', 'L', idepl)
!
    call sigvmc('RIGI', nno, ndim, nbsig, npg1,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                instan, repere, zi(imate), nharm, sigma)
!
!
! ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
! ---- AVEC LE VECTEUR DES CONTRAINTES AUX POINTS D'INTEGRATION
!      --------------------------------------------------------
    call jevech('PCONTRR', 'E', icont)
!
    do 40 i = 1, nbsig*npg1
        zr(icont+i-1) = sigma(i)
40  end do
!
end subroutine
