subroutine te0086(option, nomte)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/ppgan2.h"
#include "asterfort/sigvmc.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES CONTRAINTES EN 2D
!                          OPTION : 'SIGM_ELNO  '
!                             OU  : 'SIEF_ELGA  '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: sigma(54), bary(3), repere(7)
    real(kind=8) :: nharm, instan, contno(54)
    integer :: idim
!
!
!-----------------------------------------------------------------------
    integer :: i, icont, idepl, idfde, igau, igeom, imate
    integer :: ino, ipoids, isig, ivf, j, nbsig, jgano
    integer :: nbsig1, nbsig2, ndim, nno, nnos, npg
!
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig1 = nbsigm()
    nbsig2 = 6
    nbsig = nbsig1
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    instan = zero
    nharm = zero
!
    do 10 i = 1, nbsig2*npg
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
    do 160 i = 1, nno
        do 150 idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
150      continue
160  end do
    call ortrep(zi(imate), ndim, bary, repere)
!
! ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
!      --------------------------------------------------
    call jevech('PDEPLAR', 'L', idepl)
!
!
    do 20 i = 1, nbsig*npg
        sigma(i) = zero
20  end do
!
!
!
!
!       CALCUL DES CONTRAINTES 'VRAIES' AUX POINTS D'INTEGRATION
!       DE L'ELEMENT : (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
!       --------------------------------------------------------
    call sigvmc('RIGI', nno, ndim, nbsig1, npg,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                instan, repere, zi(imate), nharm, sigma)
!
    if (option(6:9) .eq. 'ELGA') then
!
        call jevech('PCONTRR', 'E', icont)
!
!         --------------------
! ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES CONTRAINTES AUX
! ---- POINTS D'INTEGRATION
!      --------------------
        do 80 igau = 1, npg
            do 81 isig = 1, nbsig
                zr(icont+nbsig*(igau-1)+isig-1) = sigma(nbsig*(igau-1) +isig)
81          continue
80      continue
!
    else
!
        call ppgan2(jgano, 1, nbsig, sigma, contno)
!
!
! ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
! ---- AVEC LE VECTEUR DES CONTRAINTES AUX NOEUDS
!      ------------------------------------------
!
        call jevech('PCONTRR', 'E', icont)
        do 140 ino = 1, nno
            do 130 j = 1, nbsig
                zr(icont+nbsig* (ino-1)-1+j) = contno(nbsig* (ino-1)+ j)
130          continue
140      continue
    endif
!
end subroutine
