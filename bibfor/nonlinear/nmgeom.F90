subroutine nmgeom(ndim, nno, axi, grand, geom,&
                  kpg, ipoids, ivf, idfde, depl,&
                  ldfdi, poids, dfdi, f, eps,&
                  r)
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
    implicit none
!
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
    logical :: axi, grand
    integer :: ndim, nno, kpg
    real(kind=8) :: geom(ndim, nno), dfdi(nno, ndim), depl(ndim, nno)
    real(kind=8) :: poids, f(3, 3), eps(6), r
    logical :: ldfdi
!
!.......................................................................
!
!     BUT:  CALCUL DES ELEMENTS CINEMATIQUES (MATRICES F ET E, RAYON R)
!           EN UN POINT DE GAUSS (EVENTUELLEMENT EN GRANDES TRANSFORM.)
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  AXI     : INDICATEUR SI AXISYMETRIQUE
! IN  GRAND   : INDICATEUR SI GRANDES TRANSFORMATIONS
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  KPG     : NUMERO DU POINT DE GAUSS (POUR L'ACCES AUX FCT. FORMES)
! IN  IPOIDS  : POIDS DU POINT DE GAUSS DE L'ELEMENT DE REFERENCE
! IN  IVF     : VALEUR DES FONCTIONS DE FORME (EN AXISYMETRIQUE)
! IN  IDFDE   : DERIVEE DES FONCTIONS DE FORME DE REFERENCE
! IN  DEPL    : DEPLACEMENT A PARTIR DE LA CONF DE REF
! IN  DEPL    : DEPLACEMENT A PARTIR DE LA CONF DE REF
! IN  LDFDI   : VEUT-ON CALCULER DFDI ET POIDS
! OUT POIDS   : "POIDS" DU POINT DE GAUSS
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME
! OUT F       : GRADIENT DE LA TRANSFORMATION
! OUT EPS     : DEFORMATIONS
! OUT R       : DISTANCE DU POINT DE GAUSS A L'AXE (EN AXISYMETRIQUE)
!......................................................................
! REMARQUE CONCERNANT L'ARGUMENT LDFDI :
!  NMGEOM EST PARFOIS APPELE 2 FOIS DE SUITE AVEC U ET DELTA_U (PAR
!  EXEMPLE DANS NMPL3D). COMME LE CALCUL DE DFDM3D EST COUTEUX ET QU'IL
!  EST INDEPENDANT DE U, ON PEUT ECONOMISER LE 2EME CALCUL EN UTILISANT
!  L'ARGUMENT LDFDI : 1ER APPEL .TRUE. ; 2EME APPEL .FALSE.
!
!
    logical :: tridim
    integer :: i, j, k, n
    real(kind=8) :: grad(3, 3), epstab(3, 3), ur, tmp
    real(kind=8) :: rac2, kron(3, 3)
!-----------------------------------------------------------------------
    integer :: idfde, ipoids, ivf
!-----------------------------------------------------------------------
    data kron/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
    rac2 = sqrt(2.d0)
    tridim = (ndim.eq.3)
!
! - CALCUL DES DERIVEES DES FONCTIONS DE FORME ET JACOBIEN
    if (ldfdi) then
        if (tridim) then
            call dfdm3d(nno, kpg, ipoids, idfde, geom,&
                        dfdi(1, 1), dfdi(1, 2), dfdi(1, 3), poids)
        else
            call dfdm2d(nno, kpg, ipoids, idfde, geom,&
                        dfdi(1, 1), dfdi(1, 2), poids)
        endif
    endif
!
!
! - CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE) ET DU DEPL. RADIAL
    if (axi) then
        r = 0.d0
        ur = 0.d0
        do 10 n = 1, nno
            r = r + zr(ivf-1+n+(kpg-1)*nno)*geom(1,n)
            ur = ur + zr(ivf-1+n+(kpg-1)*nno)*depl(1,n)
10      continue
        if (ldfdi) poids = poids*r
    endif
!
! - CALCUL DES GRADIENT : GRAD(U) ET F
!
    do 13 i = 1, 3
        do 16 j = 1, 3
            f(i,j) = kron(i,j)
            grad(i,j) = 0.d0
16      continue
13  end do
!
    if (tridim) then
        do 20 n = 1, nno
            do 22 i = 1, 3
                do 24 j = 1, 3
                    grad(i,j) = grad(i,j) + dfdi(n,j)*depl(i,n)
24              continue
22          continue
20      continue
    else
        do 30 n = 1, nno
            do 32 i = 1, 2
                do 34 j = 1, 2
                    grad(i,j) = grad(i,j) + dfdi(n,j)*depl(i,n)
34              continue
32          continue
30      continue
    endif
!
    if (grand) then
        do 40 i = 1, 3
            do 42 j = 1, 3
                f(i,j) = f(i,j) + grad(i,j)
42          continue
40      continue
        if (axi) f(3,3) = 1.d0 + ur/r
    endif
!
! - CALCUL DES DEFORMATIONS : E
!
    do 90 i = 1, ndim
        do 100 j = 1, i
            tmp = grad(i,j) + grad(j,i)
!
            if (grand) then
                do 110 k = 1, ndim
                    tmp = tmp + grad(k,i)*grad(k,j)
110              continue
            endif
!
            epstab(i,j) = 0.5d0*tmp
!
100      continue
90  end do
!
    eps(1) = epstab(1,1)
    eps(2) = epstab(2,2)
    eps(3) = 0.d0
    eps(4) = epstab(2,1)*rac2
    eps(5) = 0.d0
    eps(6) = 0.d0
!
    if (tridim) then
        eps(3) = epstab(3,3)
        eps(5) = epstab(3,1)*rac2
        eps(6) = epstab(3,2)*rac2
    else if (axi) then
        eps(3) = ur/r
        if (grand) eps(3) = eps(3) + 0.5d0*ur*ur/(r*r)
    else
        eps(3) = 0.d0
    endif
!
end subroutine
