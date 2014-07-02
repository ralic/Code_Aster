subroutine ejcine(ndim, axi, nno1, nno2, vff1,&
                  vff2, wref, dffr2, geom, wg,&
                  kpg, ipg, idf2, rot, b)
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
! person_in_charge: jerome.laverne at edf.fr
! aslint: disable=W1306
    implicit none
#include "asterf_types.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/r8inir.h"
#include "asterfort/subaco.h"
#include "asterfort/sumetr.h"
#include "blas/ddot.h"
    aster_logical :: axi
    integer :: ndim, nno1, nno2, kpg, ipg, idf2
    real(kind=8) :: wref, vff1(nno1), vff2(nno2), geom(ndim, nno2)
    real(kind=8) :: rot(ndim, ndim)
    real(kind=8) :: dffr2(ndim-1, nno2), wg, b(2*ndim-1, ndim+1, 2*nno1+nno2)
!
!-----------------------------------------------------------------------
!  MATRICE CINEMATIQUE POUR LES ELEMENTS DE JOINT HM (EN UN PG DONNE)
!-----------------------------------------------------------------------
! IN  NDIM   DIMENSION DE L'ESPACE
! IN  AXI    .TRUE. SI AXISYMETRIQUE
! IN  NNO1   NB DE NOEUDS DE LA FACE POUR LES DEPLACEMENTS
! IN  NNO2   NB DE NOEUDS DE LA FACE POUR LES PRESSIONS P ET LA GEOM X
! IN  VFF1   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR U
! IN  VFF2   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR P ET X
! IN  WREF   POIDS DE REFERENCE DU POINT DE GAUSS
! IN  DFFR2  DERIVEE DES FONCTIONS DE FORME DE REFERENCE DE P ET X EN G
! IN  GEOM   COORDONNEES DES NOEUDS (X)
! OUT WG     POIDS REEL DU POINT DE GAUSS (AVEC DISTORSION)
! OUT ROT    MATRICE DE ROTATION DU LOCAL AU GLOBAL
! OUT B      MATRICE DE PASSAGE UNODAL -> SAUT DE U LOCAL
!-----------------------------------------------------------------------
    integer :: n, i, j
    real(kind=8) :: cova(3, 3), metr(2, 2), cour, jac, cosa, sina, noa1
    real(kind=8) :: ray
    real(kind=8) :: geoloc(ndim, nno2), geotan(ndim-1, nno2)
    real(kind=8) :: dfdis(nno2, ndim-1), wg2
!-----------------------------------------------------------------------
!
    if (ndim .eq. 3) then
!
        call subaco(nno2, dffr2, geom, cova)
        call sumetr(cova, metr, jac)
        wg = wref*jac
!
!       MATRICE DE ROTATION
        noa1 = sqrt(cova(1,1)**2 + cova(2,1)**2 + cova(3,1)**2)
        rot(1,1) = cova(1,3)
        rot(1,2) = cova(2,3)
        rot(1,3) = cova(3,3)
        rot(2,1) = cova(1,1)/noa1
        rot(2,2) = cova(2,1)/noa1
        rot(2,3) = cova(3,1)/noa1
        rot(3,1) = rot(1,2)*rot(2,3) - rot(1,3)*rot(2,2)
        rot(3,2) = rot(1,3)*rot(2,1) - rot(1,1)*rot(2,3)
        rot(3,3) = rot(1,1)*rot(2,2) - rot(1,2)*rot(2,1)
!
!       CALCUL DE LA GEOMETRIE DANS LE REPERE LOCAL
        call r8inir(ndim*nno2, 0.d0, geoloc, 1)
!
        do 10 n = 1, nno2
            do 11 i = 1, ndim
                do 12 j = 1, ndim
                    geoloc(i,n) = geoloc(i,n) + rot(i,j)*geom(j,n)
 12             continue
 11         continue
 10     continue
!
        do 13 n = 1, nno2
            do 14 i = 2, ndim
                geotan(i-1,n)=geoloc(i,n)
 14         continue
 13     continue
!
!       CALCUL DES DERIVEE DES FF DANS LE PLAN TANGENTIEL
        call dfdm2d(nno2, kpg, ipg, idf2, geotan,&
                    wg2, dfdis(1, 1), dfdis(1, 2))
!
    else if (ndim.eq.2) then
!
!       CALCUL DES DERIVEE DES FF DANS LE PLAN TANGENTIEL
        call dfdm1d(nno2, wref, dffr2, geom, dfdis(1, 1),&
                    cour, wg, cosa, sina)
!
!       CALCUL DE LA DISTANCE A L'AXE EN AXI, R=RAYON DU PG COURANT
        if (axi) then
            ray = ddot(nno2,geom,2,vff2,1)
            wg = ray*wg
        endif
!
!       MATRICE DE ROTATION
        rot(1,1) = -cosa
        rot(1,2) = -sina
        rot(2,1) = sina
        rot(2,2) = -cosa
!
    endif
!
!     CONSTRUCTION DE LA MATRICE B
    call r8inir((2*ndim-1)*(ndim+1)*(2*nno1+nno2), 0.d0, b, 1)
!
    do 20 i = 1, ndim
        do 30 j = 1, ndim
!
            do 40 n = 1, nno1
                b(i,j,n) = - rot(i,j)*vff1(n)
                b(i,j,n+nno1) = rot(i,j)*vff1(n)
 40         continue
!
 30     continue
 20 end do
!
    do 21 i = 1, ndim-1
        do 41 n = 1, nno2
            b(ndim+i,ndim+1,2*nno1+n) = dfdis(n,i)
 41     continue
 21 end do
!
end subroutine
