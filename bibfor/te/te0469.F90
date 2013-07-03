subroutine te0469(option, nomte)
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
    implicit none
!
!     BUT: CONSTRUCTION DU VECTEUR DES FORCES CORRESPONDANT A UN
!          CHARGEMENT FORCE_ARETE POUR LES ELEMENTS ISOPARAMETRIQUES 3D.
!          (I.E. CALCUL DU VECTEUR DES FORCES NODALES EQUIVALENTES
!                A UNE FORCE LINEIQUE LE LONG D'UNE ARETE POUR
!                LES ELEMENTS 3D).
!
!          OPTIONS : 'CHAR_MECA_FR1D3D'
!                    'CHAR_MECA_FF1D3D'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
!-----------------------------------------------------------------------
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/vff3d.h"
    integer :: i, idecno, idecpg, idfdk, idflin, ier, igau
    integer :: igeom, ino, ipoids, itemps, ivectu, ivf, jgano
    integer :: nbnomx, ndim, nno, nnos, npg
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    parameter (nbnomx=27)
    character(len=8) :: nompar(4)
    character(len=16) :: nomte, option
    real(kind=8) :: fx(nbnomx), fy(nbnomx), fz(nbnomx), valpar(4)
    real(kind=8) :: xyzgau(3), forlin(3), jacob
    real(kind=8) :: fxlin(5), fylin(5), fzlin(5)
!
!
!
! --- CARACTERISTIQUES DU TYPE D'ELEMENT :
! --- INTEGRATION ET INTERPOLATION
!      ----------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
!
    do 10 i = 1, nbnomx
        fx(i) = zero
        fy(i) = zero
        fz(i) = zero
10  end do
!
    do 20 i = 1, npg
        fxlin(i) = zero
        fylin(i) = zero
        fzlin(i) = zero
20  end do
!
! --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!     ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! --- OPTION 'CHAR_MECA_FR1D3D'
! --- CAS OU LES DONNEES DES FORCES LINEIQUES SONT DES REELS :
!     ------------------------------------------------------
    if (option .eq. 'CHAR_MECA_FR1D3D') then
!
! ---  RECUPERATION DES VALEURS DE LA FORCE LINEIQUE A APPLIQUER
! ---  SUR L'ELEMENT D'ARETE :
!      ---------------------
        call jevech('PFR1D3D', 'L', idflin)
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
        do 40 igau = 1, npg
!
            idecpg = nno* (igau-1)
!
!
! ---    CALCUL DE LA FORCE LINEIQUE AUX POINTS D'INTEGRATION :
!        -----------------------------------------------------
            do 30 ino = 1, nno
!
                fxlin(igau) = fxlin(igau) + zr(ivf+idecpg+ino-1)* zr(idflin+1-1)
                fylin(igau) = fylin(igau) + zr(ivf+idecpg+ino-1)* zr(idflin+2-1)
                fzlin(igau) = fzlin(igau) + zr(ivf+idecpg+ino-1)* zr(idflin+3-1)
30          continue
40      continue
!
! ---    BOUCLE SUR LES POINTS D'INTEGRATION
!        -----------------------------------
        do 60 igau = 1, npg
!
            idecpg = nno* (igau-1)
!
! ---    CALCUL DU PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
! ---    COURANT :
!        -------
            call vff3d(nno, zr(ipoids+igau-1), zr(idfdk+idecpg), zr( igeom), jacob)
!
! ---    CALCUL DE LA CONTRIBUTION AU VECTEUR DES FORCES NODALES
! ---    DU CHARGEMENT LINEIQUE AU POINT D'INTEGRATION COURANT :
!        -----------------------------------------------------
            do 50 ino = 1, nno
!
                fx(ino) = fx(ino) + zr(ivf+idecpg+ino-1)*fxlin(igau)* jacob
                fy(ino) = fy(ino) + zr(ivf+idecpg+ino-1)*fylin(igau)* jacob
                fz(ino) = fz(ino) + zr(ivf+idecpg+ino-1)*fzlin(igau)* jacob
50          continue
!
60      continue
!
! --- OPTION 'CHAR_MECA_FF1D3D'
! --- CAS OU LES DONNEES DES FORCES LINEIQUES SONT DES FONCTIONS
! --- DES COORDONNEES ET DU TEMPS :
!     ---------------------------
    else if (option.eq.'CHAR_MECA_FF1D3D') then
!
! ---  RECUPERATION DES NOMS DES FONCTIONS REPRESENTANT LA FORCE
! ---  LINEIQUE A APPLIQUER SUR L'ELEMENT D'ARETE :
!      ------------------------------------------
        call jevech('PFF1D3D', 'L', idflin)
!
! ---  RECUPERATION DE L'INSTANT D'INTERPOLATION :
!      -----------------------------------------
        call jevech('PTEMPSR', 'L', itemps)
!
! ---  AFFECTATION DES VARIABLES POUR L'INTERPOLATION :
!      ----------------------------------------------
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
!
        valpar(4) = zr(itemps)
!
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
        do 100 igau = 1, npg
!
            idecpg = nno* (igau-1)
!
            do 70 i = 1, 3
                xyzgau(i) = zero
                forlin(i) = zero
70          continue
!
! ---    CALCUL DES COORDONNEES DU POINT D'INTEGRATION COURANT :
!        -----------------------------------------------------
            do 80 ino = 1, nno
!
                idecno = 3* (ino-1) - 1
!
                xyzgau(1) = xyzgau(1) + zr(ivf+idecpg+ino-1)* zr( igeom+1+idecno)
                xyzgau(2) = xyzgau(2) + zr(ivf+idecpg+ino-1)* zr( igeom+2+idecno)
                xyzgau(3) = xyzgau(3) + zr(ivf+idecpg+ino-1)* zr( igeom+3+idecno)
80          continue
!
! ---    INTERPOLATION DES FORCES LINEIQUES EN FONCTION DES
! ---    COORDONNEES ET DU TEMPS :
!        -----------------------
            valpar(1) = xyzgau(1)
            valpar(2) = xyzgau(2)
            valpar(3) = xyzgau(3)
!
            call fointe('FM', zk8(idflin+1-1), 4, nompar, valpar,&
                        forlin(1), ier)
            call fointe('FM', zk8(idflin+2-1), 4, nompar, valpar,&
                        forlin(2), ier)
            call fointe('FM', zk8(idflin+3-1), 4, nompar, valpar,&
                        forlin(3), ier)
!
! ---    CALCUL DU PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
! ---    COURANT :
!        -------
            call vff3d(nno, zr(ipoids+igau-1), zr(idfdk+idecpg), zr( igeom), jacob)
!
! ---    CALCUL DE LA CONTRIBUTION AU VECTEUR DES FORCES NODALES
! ---    DU CHARGEMENT LINEIQUE AU POINT D'INTEGRATION COURANT :
!        -----------------------------------------------------
            do 90 ino = 1, nno
!
                fx(ino) = fx(ino) + zr(ivf+idecpg+ino-1)*forlin(1)* jacob
                fy(ino) = fy(ino) + zr(ivf+idecpg+ino-1)*forlin(2)* jacob
                fz(ino) = fz(ino) + zr(ivf+idecpg+ino-1)*forlin(3)* jacob
90          continue
!
100      continue
!
    endif
!
! ---- RECUPERATION ET AFFECTATION DU VECTEUR FORCES NODALES EN SORTIE :
!      ---------------------------------------------------------------
    call jevech('PVECTUR', 'E', ivectu)
!
    do 110 ino = 1, nno
        zr(ivectu+3* (ino-1)+1-1) = fx(ino)
        zr(ivectu+3* (ino-1)+2-1) = fy(ino)
        zr(ivectu+3* (ino-1)+3-1) = fz(ino)
110  end do
!
end subroutine
