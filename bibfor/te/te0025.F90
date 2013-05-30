subroutine te0025(option, nomte)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/epsvmc.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nbsigm.h'
    include 'asterfort/ortrep.h'
    include 'asterfort/tecach.h'
    character(len=16) :: option, nomte
!.......................................................................
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
!     BUT: CALCUL DES DEFORMATIONS AUX POINTS D'INTEGRATION
!          DES ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTIONS : 'EPSI_ELGA'
!                    'EPSG_ELGA'
!                    'EPME_ELGA'
!                    'EPMG_ELGA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    integer :: jgano, ndim, nno, i, nnos, npg, ipoids, ivf, idfde, nbsig, igau
    integer :: isig, igeom, idepl, itemps, idefo, imate, iret, idim
    real(kind=8) :: epsm(162), repere(7), bary(3)
    real(kind=8) :: nharm, instan, zero
! DEB ------------------------------------------------------------------
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
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
    do 20 i = 1, nbsig*npg
        epsm(i) = zero
20  end do
!
! ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!      ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT :
!      --------------------------------------------------
    call jevech('PDEPLAR', 'L', idepl)
!
! ---- RECUPERATION DE L'INSTANT DE CALCUL :
!      -----------------------------------
    call tecach('NNN', 'PTEMPSR', 'L', 1, itemps,&
                iret)
    if (itemps .ne. 0) then
        instan = zr(itemps)
    endif
!
! ---- RECUPERATION DU VECTEUR DES DEFORMATIONS EN SORTIE :
!      --------------------------------------------------
    call jevech('PDEFOPG', 'E', idefo)
!
! ---- CALCUL DES DEFORMATIONS MECANIQUES AUX POINTS D'INTEGRATION
! ---- DE L'ELEMENT , I.E. SI ON NOTE EPSI_MECA = B*U
! ---- ON CALCULE SIMPLEMENT EPSI_MECA POUR LES OPTIONS EPSI ET EPSG
! ----                    ET EPSI_MECA - EPSI_THERMIQUES POUR LES
! ----                    OPTIONS EPME ET EPMG :
!      ---------------------------------------
    call jevech('PMATERC', 'L', imate)
!
! ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
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
    call epsvmc('RIGI', nno, ndim, nbsig, npg,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                instan, zi(imate), repere, nharm, option,&
                epsm)
!
!         --------------------
! ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
! ---- POINTS D'INTEGRATION :
!      --------------------
    do 80 igau = 1, npg
        do 70 isig = 1, nbsig
            zr(idefo+nbsig* (igau-1)+isig-1) = epsm(nbsig* (igau-1)+ isig)
70      continue
80  end do
!
end subroutine
