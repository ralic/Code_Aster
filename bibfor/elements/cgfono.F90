subroutine cgfono(ndim, nno1, nno2, npg, wref,&
                  vff1, vff2, dffr1, geom, tang,&
                  iu, iuc, im, sigp, vect)
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
!
#include "asterfort/cgcine.h"
#include "asterfort/r8inir.h"
    integer :: ndim, nno1, nno2, npg, iu(3, 3), iuc(3), im(3)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno1), wref(npg)
    real(kind=8) :: vect(2*nno1*ndim+nno2*ndim)
    real(kind=8) :: dffr1(nno1, npg), tang(*), sigp(3, npg)
! ----------------------------------------------------------------------
!
!     RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR CABLE_GAINE
!
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
! IN  DFFR2   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE L)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  WREF    : POIDS DES POINTS DE GAUSS DE REFERENCE
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT GA
! IN  IUC     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPL RELATIF
! IN  IM      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE
! OUT SIGP    : CONTRAINTES GENERALISEES (RAPH_MECA   ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! ----------------------------------------------------------------------
    integer :: g, n, i, kk
    real(kind=8) :: wg, b(4, 3), t1, l(3), courb
! ----------------------------------------------------------------------
!
    call r8inir(nno1*(ndim+1)+nno2, 0.d0, vect, 1)
!
    do g = 1, npg
!
        call cgcine(ndim, nno1, vff1(1, g), &
                    wref(g), dffr1(1, g), geom, tang, wg,&
                    l, b, courb)
!
!        CONVENTION DE RANGEMENT SIGP(1,2,3) EXPLICITE CI-DESSOUS
!          SIGP(     1,G) = SIGCAB*A
!          SIGP(     2,G) = MU + R*(UPROJ-UCAB-DE)
!          SIGP(     3,G) = UPROJ-UCAB-DE
!        VECTEUR FINT:U
        do 300 n = 1, nno1
            do 301 i = 1, ndim
                kk = iu(i,n)
                t1 = b(i,n)*sigp(1,g)
                vect(kk) = vect(kk) + wg*t1
301          continue
            kk=iuc(n)
            t1=b(4,n)*sigp(1,g)+l(n)*sigp(2,g)
            vect(kk)=vect(kk)+wg*t1
300      continue
!
!        VECTEUR FINT:M
        do 350 n = 1, nno2
            kk = im(n)
            t1 = vff2(n,g)*sigp(3,g)
            vect(kk) = vect(kk) + wg*t1
350      continue
!
!
!
    end do
!
end subroutine
