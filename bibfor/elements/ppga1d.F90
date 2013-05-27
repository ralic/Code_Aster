subroutine ppga1d(ndim, nno, npg, poids, vff,&
                  dff, geom, pg)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux.h'
    integer :: ndim, nno, npg
    real(kind=8) :: poids(npg), vff(nno, npg), dff(nno, npg), geom(ndim, nno)
    real(kind=8) :: pg(ndim+1, npg)
! ----------------------------------------------------------------------
!  POSITION ET POIDS DES POINTS DE GAUSS POUR ELEMENTS 1D
!  *           *                   **                  **
! ----------------------------------------------------------------------
! IN  NDIM    DIMENSION DE L'ESPACE
! IN  NNO     NOMBRE DE NOEUDS
! IN  NPG     NOMBRE DE POINTS DE GAUSS
! IN  POIDS   POIDS DES POINTS DE GAUSS DE L'ELEMENT DE REFERENCE
! IN  VFF     VALEUR DES FONCTIONS DE FORME
! IN  GEOM    COORDONNEES DES NOEUDS
! OUT PG      COORDONNEES DES POINTS DE GAUSS + POIDS
! ----------------------------------------------------------------------
!
    integer :: g, i, j
    real(kind=8) :: dxdk, dydk, dzdk
    real(kind=8) :: jac
!
! ----------------------------------------------------------------------
!
!     1. CALCUL DES COORDONNEES DES POINTS DE GAUSS
!     =============================================
!
    do 10 g = 1, npg
        do 20 i = 1, ndim+1
            pg(i,g) = 0.d0
20      continue
10  end do
!
    do 30 g = 1, npg
        do 40 i = 1, ndim
            do 50 j = 1, nno
                pg(i,g) = pg(i,g) + geom(i,j)*vff(j,g)
50          continue
40      continue
30  end do
!
    if (nno .eq. 1) goto 9999
!
!     2. CALCUL DU POIDS
!     ==================
!
    do 100 g = 1, npg
        dxdk = 0.d0
        dydk = 0.d0
        dzdk = 0.d0
!        COMPOSANTES DU VECTEUR TANGENT
        do 90 j = 1, nno
            dxdk = dxdk + geom(1,j)*dff(j,g)
            dydk = dydk + geom(2,j)*dff(j,g)
            if (ndim .eq. 3) dzdk = dzdk + geom(3,j)*dff(j,g)
90      continue
!        JACOBIEN 1D == DERIVEE DE L'ABSCISSE CURVILIGNE
        jac = sqrt(dxdk**2+dydk**2+dzdk**2)
!
        pg(ndim+1,g) = jac*poids(g)
100  end do
!
9999  continue
!
end subroutine
