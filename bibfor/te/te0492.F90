subroutine te0492(option, nomte)
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
#include "asterfort/bsigmc.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/sigtmc.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_MECA_HYDR_R'
!                                   'CHAR_MECA_SECH_R'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=4) :: fami
    real(kind=8) :: bsigma(81), sigth(162), repere(7), instan
    real(kind=8) :: nharm, xyz(3), zero
    integer :: idim, i, idfde, igeom, imate, ipoids, iret
    integer :: itemps, ivectu, ivf, jgano, nbsig, ndim, nno, nnos, npg
!
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    instan = zero
    nharm = zero
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
!
    do 10 i = 1, nbsig*npg
        sigth(i) = zero
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
    xyz(1) = 0.d0
    xyz(2) = 0.d0
    xyz(3) = 0.d0
    do 150 i = 1, nno
        do 140 idim = 1, ndim
            xyz(idim) = xyz(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
140      continue
150  end do
    call ortrep(zi(imate), ndim, xyz, repere)
!
! ---- RECUPERATION DE L'INSTANT
!      -------------------------
    call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemps)
    if (itemps .ne. 0) instan = zr(itemps)
!
! ---- CALCUL DES CONTRAINTES AUX POINTS D'INTEGRATION DE L'ELEMENT
!      ------------------------------------------------------------
    call sigtmc(fami, nno, ndim, nbsig, npg,&
                zr(ivf), zr(igeom), instan, zi(imate), repere,&
                option, sigth)
!
! ---- CALCUL DU VECTEUR DES FORCES (BT*SIGTH)
!      ---------------------------------------
    call bsigmc(nno, ndim, nbsig, npg, ipoids,&
                ivf, idfde, zr(igeom), nharm, sigth,&
                bsigma)
!
! ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE AVEC LE
! ---- VECTEUR DES FORCES D'ORIGINE THERMIQUE
!      -------------------------------------
    call jevech('PVECTUR', 'E', ivectu)
!
    do 30 i = 1, ndim*nno
        zr(ivectu+i-1) = bsigma(i)
30  end do
!
! FIN ------------------------------------------------------------------
end subroutine
