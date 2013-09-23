subroutine te0284(option, nomte)
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
#include "asterfort/epsimc.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/sigimc.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES EN 2D
!                      OPTION : 'CHAR_MECA_EPSI_R  ','CHAR_MECA_EPSI_F '
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=4) :: fami
    real(kind=8) :: sigi(162), epsi(162), bsigma(81), repere(7)
    real(kind=8) :: instan, nharm, xyz(81), bary(3)
    integer :: dimcoo, idim
!
!
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!-----------------------------------------------------------------------
    integer :: i, idfde, igeom, iharmo, imate, ipoids, iret
    integer :: itemps, ivectu, ivf, jgano, nbsig, ndim, nno
    integer :: nnos, npg
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    dimcoo = ndim
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    instan = zero
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
    if (nbsig .eq. 6) ndim=3
!
    do 10 i = 1, nbsig*npg
        epsi(i) = zero
        sigi(i) = zero
10  end do
!
    do 20 i = 1, ndim*nno
        bsigma(i) = zero
20  end do
!
! ---- RECUPERATION DE L'HARMONIQUE DE FOURIER
!      ---------------------------------------
    call tecach('NNN', 'PHARMON', 'L', iret, iad=iharmo)
    if (iharmo .eq. 0) then
        nharm = zero
    else
        nharm = dble( zi(iharmo) )
    endif
!
! ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
!      ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
    if (ndim .eq. dimcoo) then
        do 100 i = 1, ndim*nno
            xyz(i) = zr(igeom+i-1)
100      continue
    else
        do 110 i = 1, nno
            do 120 idim = 1, ndim
                if (idim .le. dimcoo) then
                    xyz(idim+ndim*(i-1)) = zr(igeom-1+idim+dimcoo*(i- 1))
                else
                    xyz(idim+ndim*(i-1)) = 0.d0
                endif
120          continue
110      continue
    endif
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
        do 140 idim = 1, dimcoo
            bary(idim) = bary(idim)+zr(igeom+idim+dimcoo*(i-1)-1)/nno
140      continue
150  end do
    call ortrep(zi(imate), dimcoo, bary, repere)
!
!
! ---- RECUPERATION DE L'INSTANT
!      -------------------------
    call tecach('NNN', 'PTEMPSR', 'L', iret, iad=itemps)
    if (itemps .ne. 0) instan = zr(itemps)
!
! ---- CONSTRUCTION DU VECTEUR DES DEFORMATIONS INITIALES DEFINIES AUX
! ---- POINTS D'INTEGRATION A PARTIR DES DONNEES UTILISATEUR
!      -----------------------------------------------------
    call epsimc(option, zr(igeom), nno, npg, ndim,&
                nbsig, zr(ivf), epsi)
!
! ---- CALCUL DU VECTEUR DES CONTRAINTES INITIALES AUX POINTS
! ---- D'INTEGRATION
!      -------------
    call sigimc(fami, nno, ndim, nbsig, npg,&
                zr(ivf), xyz, instan, zi(imate), repere,&
                epsi, sigi)
!
! ---- CALCUL DU VECTEUR DES FORCES DUES AUX CONTRAINTES INITIALES
! ---- (I.E. BT*SIG_INITIALES)
!      ----------------------
    call bsigmc(nno, ndim, nbsig, npg, ipoids,&
                ivf, idfde, zr(igeom), nharm, sigi,&
                bsigma)
!
! ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE AVEC LE
! ---- VECTEUR DES FORCES DUES AUX CONTRAINTES INITIALES
!      -------------------------------------------------
    call jevech('PVECTUR', 'E', ivectu)
!
    do 30 i = 1, ndim*nno
        zr(ivectu+i-1) = bsigma(i)
30  end do
!
! FIN ------------------------------------------------------------------
end subroutine
