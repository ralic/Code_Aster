subroutine te0115(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ......................................................................
    implicit none
!     BUT: CALCUL DES CONTRAINTES AUX POINTS DE GAUSS EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 2D FOURIER
!
!            OPTION : 'SIEF_ELGA   '
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/r8inir.h"
#include "asterfort/sigvmc.h"
    character(len=16) :: option, nomte
    character(len=4) :: fami
    real(kind=8) :: sigma(54), repere(7), instan, nharm
    real(kind=8) :: r8bid1(9), bary(3)
    integer :: ndim, nno, nnos, npg1, ipoids, ivf, dimmod
    integer :: idfde, jgano, idim
!
!
!-----------------------------------------------------------------------
    integer :: i, icont, idepl, igeom, iharmo, imate, nbsig
    integer :: nh
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    dimmod = 3
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
    call r8inir(9, 0.d0, r8bid1, 1)
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
! ---- RECUPERATION  DU NUMERO D'HARMONIQUE
!      ------------------------------------
    call jevech('PHARMON', 'L', iharmo)
    nh = zi(iharmo)
    nharm = dble(nh)
!
! ---- RECUPERATION DU VECTEUR DES CONTRAINTES EN SORTIE
!      -------------------------------------------------
    call jevech('PCONTRR', 'E', icont)
!
! ---- CALCUL DES CONTRAINTES 'VRAIES' AUX POINTS D'INTEGRATION
! ---- DE L'ELEMENT :
! ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
!      ------------------------------------
    call sigvmc(fami, nno, dimmod, nbsig, npg1,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                instan, repere, zi(imate), nharm, sigma)
!
! ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES CONTRAINTES AUX
! ---- POINTS D'INTEGRATION
!      --------------------
    do 20 i = 1, nbsig*npg1
        zr(icont+i-1) = sigma(i)
20  end do
!
end subroutine
