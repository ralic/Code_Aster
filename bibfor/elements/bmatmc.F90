subroutine bmatmc(igau, nbsig, xyz, ipoids, ivf,&
                  idfde, nno, nharm, jacob, b)
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
!.======================================================================
    implicit none
!
!      BMATMC  -- CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS
!                 DU PREMIER ORDRE AUX DEPLACEMENTS AU POINT
!                 D'INTEGRATION D'INDICE IGAU
!
!   ARGUMENT        E/S  TYPE         ROLE
!    IGAU           IN     I        INDICE DU POINT D'INTEGRATION
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    IVF            IN     I        POINTEUR FONCTIONS DE FORME
!    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
!    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NHARM          IN     R        NUMERO D'HARMONIQUE
!    JACOB          OUT    R        PRODUIT POIDS*JACOBIEN
!    B(NBSIG,1)     OUT    R        MATRICE (B) RELIANT LES
!                                   DEFORMATIONS DU PREMIER ORDRE
!                                   AUX DEPLACEMENTS AU POINT
!                                   D'INTEGRATION IGAU.
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/lteatt.h"
#include "asterfort/matini.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: xyz(1), nharm, jacob, b(nbsig, 1)
! -----  VARIABLES LOCALES
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), b3j(9), nharay
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
!-----------------------------------------------------------------------
    integer :: i, idecno, idfde, igau, ipoids, ivf, j
    integer :: k, nbsig, nno
    real(kind=8) :: rayon, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
    call matini(nbsig, 81, zero, b)
!
!       -------------
! ----  CAS MASSIF 3D
!       -------------
    if (lteatt(' ','DIM_TOPO_MAILLE','3')) then
!
        k = 3*(igau-1)*nno
!
! ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
! ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
!         ----------------------------------------------
        call dfdm3d(nno, igau, ipoids, idfde, xyz,&
                    dfdx, dfdy, dfdz, jacob)
!
! ----    AFFECTATION DE LA MATRICE (B)
!         -----------------------------
        do 20 i = 1, nno
!
            j= 3*(i-1) + 1
!
            b(1,j) = dfdx(i)
            b(2,j+1) = dfdy(i)
            b(3,j+2) = dfdz(i)
            b(4,j) = dfdy(i)
            b(4,j+1) = dfdx(i)
            b(5,j) = dfdz(i)
            b(5,j+2) = dfdx(i)
            b(6,j+1) = dfdz(i)
            b(6,j+2) = dfdy(i)
!
20      continue
!
!       -------------------------------------------------------
! ----  CAS MASSIF 2D CONTRAINTES PLANES ET DEFORMATIONS PLANES
!       -------------------------------------------------------
        elseif (lteatt(' ','C_PLAN','OUI').or. lteatt(' ','D_PLAN','OUI'))&
    then
!
        k = (igau-1)*nno + 1
!
! ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
! ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
!         ----------------------------------------------
        call dfdm2d(nno, igau, ipoids, idfde, xyz,&
                    dfdx, dfdy, jacob)
!
! ----    AFFECTATION DE LA MATRICE (B)
!         -----------------------------
        do 30 i = 1, nno
!
            j= 2*(i-1) + 1
!
            b(1,j) = dfdx(i)
            b(2,j+1) = dfdy(i)
            b(4,j) = dfdy(i)
            b(4,j+1) = dfdx(i)
!
30      continue
!
!       ------------------------
! ----  CAS MASSIF AXISYMETRIQUE
!       ------------------------
        elseif (lteatt(' ','AXIS','OUI').and. (.not.lteatt(' ','FOURIER',&
    'OUI'))) then
!
        k = (igau-1)*nno
        rayon = zero
!
        do 40 i = 1, nno
            idecno = 2*(i-1)
            rayon = rayon + zr(ivf+i+k-1)*xyz(1+idecno)
40      continue
!
! ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
! ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
!         ----------------------------------------------
        call dfdm2d(nno, igau, ipoids, idfde, xyz,&
                    dfdx, dfdy, jacob)
!
        jacob = jacob*rayon
!
        if (rayon .eq. zero) then
            do 50 i = 1, nno
                b3j(i) = dfdx(i)
50          continue
        else
            do 60 i = 1, nno
                b3j(i) = zr(ivf+i+k-1)/rayon
60          continue
        endif
!
! ----    AFFECTATION DE LA MATRICE (B)
!         -----------------------------
        do 70 i = 1, nno
!
            j= 2*(i-1) + 1
!
            b(1,j) = dfdx(i)
            b(2,j+1) = dfdy(i)
            b(3,j) = b3j(i)
            b(4,j) = dfdy(i)
            b(4,j+1) = dfdx(i)
!
70      continue
!
!       ------------------
! ----  CAS MASSIF FOURIER
!       ------------------
    else if (lteatt(' ','FOURIER','OUI')) then
!
        k = (igau-1)*nno
        rayon = zero
!
        do 80 i = 1, nno
            idecno = 2*(i-1)
            rayon = rayon + zr(ivf+i+k-1)*xyz(1+idecno)
80      continue
!
! ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
! ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
!         ----------------------------------------------
        call dfdm2d(nno, igau, ipoids, idfde, xyz,&
                    dfdx, dfdy, jacob)
!
        jacob = jacob*rayon
        nharay = nharm/rayon
!
! ----    AFFECTATION DE LA MATRICE (B)
!         -----------------------------
        do 90 i = 1, nno
!
            j= 3*(i-1) + 1
!
            b(1,j) = dfdx(i)
            b(2,j+1) = dfdy(i)
            b(3,j) = zr(ivf+i+k-1)/rayon
            b(3,j+2) = -zr(ivf+i+k-1)*nharay
            b(4,j) = dfdy(i)
            b(4,j+1) = dfdx(i)
            b(5,j) = zr(ivf+i+k-1)*nharay
            b(5,j+2) = dfdx(i) - zr(ivf+i+k-1)/rayon
            b(6,j+1) = zr(ivf+i+k-1)*nharay
            b(6,j+2) = dfdy(i)
!
90      continue
    else
        call u2mess('F', 'ELEMENTS_11')
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine
