subroutine cgcine(ndim, nno1, vff1, wref, &
                   dffr1, geom, tang, wg, l,&
                   b, nornor)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/dfdm1b.h"
#include "asterfort/r8inir.h"
    integer :: nno1, ndim
    real(kind=8) :: wref, vff1(nno1), geom(ndim, nno1), tang(3, 3)
    real(kind=8) :: dffr1(nno1), wg, dfdx(3), b(4, nno1), l(nno1)
!-----------------------------------------------------------------------
!  MATRICE CINEMATIQUE POUR LES ELEMENTS DE CABLE/GAINE (EN UN PG DONNE)
!     ROUTINE INSPIREE DE EICINE
!-----------------------------------------------------------------------
! IN  NDIM   DIMENSION DE L'ESPACE
! IN  NNO1   NB DE NOEUDS POUR LES DEPLACEMENTS U
! IN  VFF1   VALEUR DES FONCTIONS DE FORME POUR U
! IN  WREF   POIDS DE REFERENCE DU POINT DE GAUSS
! IN  DFFR1  DERIVEE DES FONCTIONS DE FORME DE REFERENCE DE L EN G
! IN  GEOM   COORDONNEES DES NOEUDS (X)
! IN  TANG   TANGENTES (FAMILLE X)
! OUT WG     POIDS REEL DU POINT DE GAUSS (AVEC DISTORSION)
! OUT L      VALEURS DES FONCTIONS DE FORME
! OUT B      MATRICE DE PASSAGE UNODAL -> U GAINE TANGENTIEL ET U CABLE
! OUT NORNOR RAYON DE COURBURE
!-----------------------------------------------------------------------
    integer :: n, i
    real(kind=8) :: tanloc(3), norloc(3)
    real(kind=8) :: norm, nornor
!-----------------------------------------------------------------------
!
!    CALCUL DU JACOBIEN
!
    call dfdm1b(nno1, wref, dffr1, geom, dfdx,&
                wg)
!
!
!    CALCUL DES ANGLES NAUTIQUES AU POINT D'INTEGRATION
!
    call r8inir(3, 0.d0, tanloc, 1)
    call r8inir(3, 0.d0, norloc, 1)
    do n = 1, nno1
        do 11 i = 1, 3
            tanloc(i)=tanloc(i)+vff1(n)*tang(i,n)
            norloc(i)=norloc(i)+dfdx(n)*tang(i,n)
11      continue
    end do
    norm = sqrt((tanloc(1)**2+tanloc(2)**2+tanloc(3)**2))
    nornor = sqrt((norloc(1)**2+norloc(2)**2+norloc(3)**2))
!
!    CONSTRUCTION DE LA MATRICE B
!
    do n = 1, nno1
        do 20 i = 1, ndim
            b(i,n) = tanloc(i)/norm*dfdx(n)
20      continue
        b(ndim+1,n) = dfdx(n)
        l(n) = vff1(n)
    end do
!
end subroutine
