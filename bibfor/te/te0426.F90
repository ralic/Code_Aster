subroutine te0426(option, nomte)
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
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/rcvarc.h"
#include "asterfort/sigimc.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: option, nomte
!.......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_EPSA_R  '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    character(len=4) :: fami
!
    real(kind=8) :: sigi(162), epsi(162), bsigma(81), repere(7)
    real(kind=8) :: instan, nharm, xyz(3)
    integer :: idim
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!-----------------------------------------------------------------------
    integer :: i, idfde, igau, igeom, imate, ipoids, iret
    integer :: itemps, ivectu, ivf, jgano, nbsig, ndim, nno
    integer :: nnos, npg1
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
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
        epsi(i) = zero
        sigi(i) = zero
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
!
! ---- CONSTRUCTION DU VECTEUR DES DEFORMATIONS ANELASTIQUES DEFINIES
! ---- AUX POINTS D'INTEGRATION A PARTIR DES DONNEES UTILISATEUR
!      --------------------------------------------------------------
!
    do 30 igau = 1, npg1
        call rcvarc(' ', 'EPSAXX', '+', 'RIGI', igau,&
                    1, epsi(nbsig*(igau- 1)+1), iret)
        if (iret .eq. 1) epsi(nbsig*(igau-1)+1)=0.d0
        call rcvarc(' ', 'EPSAYY', '+', 'RIGI', igau,&
                    1, epsi(nbsig*(igau- 1)+2), iret)
        if (iret .eq. 1) epsi(nbsig*(igau-1)+2)=0.d0
        call rcvarc(' ', 'EPSAZZ', '+', 'RIGI', igau,&
                    1, epsi(nbsig*(igau- 1)+3), iret)
        if (iret .eq. 1) epsi(nbsig*(igau-1)+3)=0.d0
        call rcvarc(' ', 'EPSAXY', '+', 'RIGI', igau,&
                    1, epsi(nbsig*(igau- 1)+4), iret)
        if (iret .eq. 1) epsi(nbsig*(igau-1)+4)=0.d0
        call rcvarc(' ', 'EPSAXZ', '+', 'RIGI', igau,&
                    1, epsi(nbsig*(igau- 1)+5), iret)
        if (iret .eq. 1) epsi(nbsig*(igau-1)+5)=0.d0
        call rcvarc(' ', 'EPSAYZ', '+', 'RIGI', igau,&
                    1, epsi(nbsig*(igau- 1)+6), iret)
        if (iret .eq. 1) epsi(nbsig*(igau-1)+6)=0.d0
30  end do
!
! ---- CALCUL DU VECTEUR DES CONTRAINTES ANELASTIQUES AUX POINTS
! ---- D'INTEGRATION
!      -------------
    call sigimc(fami, nno, ndim, nbsig, npg1,&
                zr(ivf), zr(igeom), instan, zi(imate), repere,&
                epsi, sigi)
!
! ---- CALCUL DU VECTEUR DES FORCES DUES AUX CONTRAINTES ANELASTIQUES
! ---- (I.E. BT*SIG_ANELASTIQUES)
!      ----------------------
    call bsigmc(nno, ndim, nbsig, npg1, ipoids,&
                ivf, idfde, zr(igeom), nharm, sigi,&
                bsigma)
!
! ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE AVEC LE
! ---- VECTEUR DES FORCES DUES AUX CONTRAINTES ANELASTIQUES
!      -------------------------------------------------
    call jevech('PVECTUR', 'E', ivectu)
!
    do 40 i = 1, ndim*nno
        zr(ivectu+i-1) = bsigma(i)
40  end do
!
! FIN ------------------------------------------------------------------
end subroutine
