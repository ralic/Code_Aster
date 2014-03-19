subroutine te0081(option, nomte)
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
#include "asterfort/bmatmc.h"
#include "asterfort/btdbmc.h"
#include "asterfort/dmatmc.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/rccoma.h"
    character(len=16) :: option, nomte, phenom
! ......................................................................
!    - FONCTION REALISEE:
!            OPTION : 'RIGI_MECA      '
!                            CALCUL DES MATRICES ELEMENTAIRES  2D
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: i, igau, igeom, imate, imatuu, j, k
    integer :: nbinco, nbres, nbsig
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    parameter (nbres=7)
    integer :: icodre(nbres)
    real(kind=8) :: b(486), btdb(81, 81), d(36), jacgau
    real(kind=8) :: repere(7), xyzgau(3), instan, nharm
    real(kind=8) :: bary(3)
    integer :: ndim, nno, nnos, npg1, ipoids, ivf, idfde, jgano
    integer :: idim
!
    character(len=4) :: fami
!
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! --- INITIALISATIONS :
!     -----------------
    ndim = 2
    zero = 0.0d0
    instan = zero
    nbinco = ndim*nno
    nharm = zero
!
    do 20 i = 1, nbinco
        do 10 j = 1, nbinco
            btdb(i,j) = zero
10      continue
20  end do
!
    xyzgau(1) = zero
    xyzgau(2) = zero
    xyzgau(3) = zero
!
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
!
! ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
!      ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! ---- RECUPERATION DU MATERIAU
!      ------------------------
    call jevech('PMATERC', 'L', imate)
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
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
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
    do 50 igau = 1, npg1
!
!  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU
!  --      PREMIER ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION
!  --      COURANT : (EPS_1) = (B)*(UN)
!          ----------------------------
        call bmatmc(igau, nbsig, zr(igeom), ipoids, ivf,&
                    idfde, nno, nharm, jacgau, b)
!
!  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
!  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
!          -------------------------------------------------
        call dmatmc(fami, '  ', zi(imate), instan, '+',&
                    igau, 1, repere, xyzgau, nbsig,&
                    d)
!
!  --      MATRICE DE RIGIDITE ELEMENTAIRE BT*D*B
!          ---------------------------------------
        call btdbmc(b, d, jacgau, ndim, nno,&
                    nbsig, phenom, btdb)
!
50  end do
!
! ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
!      ------------------------------------------------
    call jevech('PMATUUR', 'E', imatuu)
!
    k = 0
    do 70 i = 1, nbinco
        do 60 j = 1, i
            k = k + 1
            zr(imatuu+k-1) = btdb(i,j)
60      continue
70  end do
!
!
end subroutine
