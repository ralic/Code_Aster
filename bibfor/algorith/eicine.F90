subroutine eicine(ndim, axi, nno1, nno2, vff1,&
                  vff2, wref, dffr2, geom, ang,&
                  wg, b)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/dfdm1d.h"
#include "asterfort/matrot.h"
#include "asterfort/r8inir.h"
#include "asterfort/subaco.h"
#include "asterfort/sumetr.h"
#include "blas/ddot.h"
    logical :: axi
    integer :: ndim, nno1, nno2
    real(kind=8) :: wref, vff1(nno1), vff2(nno2), geom(ndim, nno2), ang(*)
    real(kind=8) :: dffr2(ndim-1, nno2), wg, b(3, 3, 2*nno1)
!-----------------------------------------------------------------------
!  MATRICE CINEMATIQUE POUR LES ELEMENTS D'INTERFACE (EN UN PG DONNE)
!-----------------------------------------------------------------------
! IN  NDIM   DIMENSION DE L'ESPACE
! IN  AXI    .TRUE. SI AXISYMETRIQUE
! IN  NNO1   NB DE NOEUDS DE LA FACE POUR LES DEPLACEMENTS
! IN  NNO2   NB DE NOEUDS DE LA FACE POUR LES LAGRANGES L ET LA GEOM X
! IN  VFF1   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR U
! IN  VFF2   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR L ET X
! IN  WREF   POIDS DE REFERENCE DU POINT DE GAUSS
! IN  DFFR2  DERIVEE DES FONCTIONS DE FORME DE REFERENCE DE L ET X EN G
! IN  GEOM   COORDONNEES DES NOEUDS (X)
! IN  ANG    ANGLES NAUTIQUES NODAUX (FAMILLE X)
! OUT WG     POIDS REEL DU POINT DE GAUSS (AVEC DISTORSION)
! OUT B      MATRICE DE PASSAGE UNODAL -> SAUT DE U LOCAL
!-----------------------------------------------------------------------
    integer :: n, i, j, nang
    real(kind=8) :: cova(3, 3), metr(2, 2), dfdx(9), cour, jac, cosa, sina
    real(kind=8) :: angloc(3), rot(3, 3), r, rmax
!-----------------------------------------------------------------------
!
!    CALCUL DU JACOBIEN
!
    if (ndim .eq. 3) then
        call subaco(nno2, dffr2, geom, cova)
        call sumetr(cova, metr, jac)
        wg = wref*jac
    else if (ndim.eq.2) then
        call dfdm1d(nno2, wref, dffr2, geom, dfdx,&
                    cour, wg, cosa, sina)
    endif
!
    if (axi) then
        r = ddot(nno2,geom,2,vff2,1)
! ----------------------------------------------------------------------
! POUR LES ELEMENTS AVEC COUPLAGE HM, DANS LE CAS OU R EGAL 0, ON A UN
! JACOBIEN NUL EN UN PG. ON PRENDS LE MAX DU RAYON MULTIPLIE PAR 1.E-3
! ----------------------------------------------------------------------
        if (r .eq. 0.d0) then
            rmax=geom(1,1)
            do 11 n = 2, nno2
                rmax=max(geom(1,n),rmax)
11          continue
            wg = wg*1.d-03*rmax
        else
            wg = r*wg
        endif
    endif
!
!    CALCUL DES ANGLES NAUTIQUES AU POINT D'INTEGRATION
!
    if (ndim .eq. 2) nang = 1
    if (ndim .eq. 3) nang = 3
    call r8inir(3, 0.d0, angloc, 1)
    do 10 i = 1, nang
        angloc(i) = ddot(nno2,ang(i),nang,vff2,1)
10  end do
!
!    CALCUL DE LA MATRICE DE ROTATION GLOBAL -> LOCAL
!
    call matrot(angloc, rot)
!
!    CONSTRUCTION DE LA MATRICE B
!
    do 20 i = 1, ndim
        do 30 j = 1, ndim
            do 40 n = 1, nno1
                b(i,j,n) = - rot(i,j)*vff1(n)
                b(i,j,n+nno1) = rot(i,j)*vff1(n)
40          continue
30      continue
20  end do
!
end subroutine
