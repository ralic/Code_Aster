subroutine te0083(option, nomte)
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
#include "asterfort/bsigmc.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/metau1.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/sigtmc.h"
#include "asterfort/tecach.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!        ELEMENTS 2D
!                          OPTION : 'CHAR_MECA_TEMP_R'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=4) :: fami
    real(kind=8) :: bsigma(81), sigth(162), repere(7), instan, nharm, bary(3)
    integer :: idim
!
!
!-----------------------------------------------------------------------
    integer :: i, idfde, igeom, imate, ipoids, iret, itemps
    integer :: ivectu, ivf, jgano, nbsig, ndim, nno, nnos
    integer :: npg
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    call metau1(option, nomte, iret)
!
!     PRESENCE DE METALLURGIE ?
    if (iret .eq. 1) goto 40
!
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    instan = zero
    nharm = zero
    ndim = 2
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
! ---- RECUPERATION DE L'INSTANT
!      -------------------------
    call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemps)
    if (itemps .ne. 0) instan = zr(itemps)
!
! ---- CALCUL DES CONTRAINTES THERMIQUES
! ---- AUX POINTS D'INTEGRATION DE L'ELEMENT :
!      --------------------------------------------------------
    call sigtmc(fami, nno, ndim, nbsig, npg,&
                zr(ivf), zr(igeom), instan, zi(imate), repere,&
                option, sigth)
!
! ---- CALCUL DU VECTEUR DES FORCES D'ORIGINE THERMIQUE/HYDRIQUE
! ---- OU DE SECHAGE (BT*SIGTH)
!      ----------------------------------------------------------
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
40  continue
end subroutine
