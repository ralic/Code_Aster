subroutine te0218(option, nomte)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/bsigmc.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/ethdst.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/simtep.h"
    character(len=16) :: option, nomte
!.......................................................................
!
! FONCTION REALISEE:
!
!      CALCUL DE L'ENERGIE POTENTIELLE THERMOELASTIQUE A L'EQUILIBRE
!      ELEMENTS ISOPARAMETRIQUES 3D
!
!      OPTION : 'EPOT_ELEM'
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    real(kind=8) :: sigma(162), bsigma(81), repere(7)
    real(kind=8) :: instan, nharm, bary(3)
    integer :: idim
!
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!-----------------------------------------------------------------------
    integer :: i, idepl, idfde, iener, igeom, imate, ipoids
    integer :: ivf, jgano, nbsig, ndim, nno, nnos, npg1
!
    real(kind=8) :: enthth, epot, undemi, zero
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    undemi = 0.5d0
    instan = zero
    nharm = zero
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
!
    do 10 i = 1, nbsig*npg1
        sigma(i) = zero
10  end do
!
    do 20 i = 1, ndim*nno
        bsigma(i) = zero
20  end do
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
    call ortrep(ndim, bary, repere)
!
! ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
!      --------------------------------------------------
    call jevech('PDEPLAR', 'L', idepl)
!
! ---- CALCUL DES CONTRAINTES 'VRAIES' SUR L'ELEMENT
! ---- (I.E.  1/2*SIGMA_MECA - SIGMA_THERMIQUES)
!      ------------------------------------
    call simtep('RIGI', nno, ndim, nbsig, npg1,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                instan, repere, zi(imate), nharm, sigma)
!
! ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA)
!      ------------------------------------------------
    call bsigmc(nno, ndim, nbsig, npg1, ipoids,&
                ivf, idfde, zr(igeom), nharm, sigma,&
                bsigma)
!
! ---- CALCUL DU TERME EPSTH_T*D*EPSTH
!      -------------------------------
    call ethdst('RIGI', nno, ndim, nbsig, npg1,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                instan, repere, zi(imate), option, enthth)
!
! ---- CALCUL DE L'ENERGIE POTENTIELLE :
! ----        1/2*UT*K*U - UT*FTH + 1/2*EPSTHT*D*EPSTH :
!             ----------------------------------------
    epot = zero
!
    do 30 i = 1, ndim*nno
        epot = epot + bsigma(i)*zr(idepl+i-1)
30  end do
!
    epot = epot + undemi*enthth
!
! ---- RECUPERATION ET AFFECTATION DU REEL EN SORTIE
! ---- AVEC L'ENERGIE DE DEFORMATION
!      -----------------------------
    call jevech('PENERDR', 'E', iener)
!
    zr(iener) = epot
!
end subroutine
