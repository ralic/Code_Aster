subroutine eifono(ndim, axi, nno1, nno2, npg,&
                  wref, vff1, vff2, dffr2, geom,&
                  ang, iu, im, sigp, vect)
!
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
!
    implicit none
!
#include "asterfort/eicine.h"
#include "asterfort/r8inir.h"
    logical(kind=1) :: axi
    integer :: ndim, nno1, nno2, npg, iu(3, 18), im(3, 9)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno2)
    real(kind=8) :: wref(npg)
    real(kind=8) :: vect(2*nno1*ndim+nno2*ndim)
    real(kind=8) :: dffr2(ndim-1, nno2, npg), ang(*), sigp(2*ndim, npg)
! ----------------------------------------------------------------------
!     OPTION FORC_NODA POUR LES ELEMENTS D'INTERFACE
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  AXI     : .TRUE. SI AXISYMETRIE
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
! IN  DFFR2   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE L)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  WREF    : POIDS DES POINTS DE GAUSS DE REFERENCE
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! IN  IM      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE
! OUT SIGP    : CONTRAINTES GENERALISEES (RAPH_MECA   ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! ----------------------------------------------------------------------
    integer :: g, n, i, k, kk
    real(kind=8) :: wg, b(3, 3, 18), t1
! ----------------------------------------------------------------------
!
    call r8inir(nno1*2*ndim+nno2*ndim, 0.d0, vect, 1)
!
    do 1000 g = 1, npg
!
        call eicine(ndim, axi, nno1, nno2, vff1(1, g),&
                    vff2(1, g), wref(g), dffr2(1, 1, g), geom, ang,&
                    wg, b)
!
!      VECTEUR FINT:U
        do 300 n = 1, 2*nno1
            do 301 i = 1, ndim
                kk = iu(i,n)
                t1 = 0
                do 320 k = 1, ndim
                    t1 = t1 + b(k,i,n)*sigp(k,g)
320              continue
                vect(kk) = vect(kk) + wg*t1
301          continue
300      continue
!
!      VECTEUR FINT:M
        do 350 n = 1, nno2
            do 351 i = 1, ndim
                kk = im(i,n)
                t1 = vff2(n,g)*sigp(ndim+i,g)
                vect(kk) = vect(kk) + wg*t1
351          continue
350      continue
!
1000  end do
!
end subroutine
