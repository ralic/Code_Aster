subroutine cgforc(ndim, nno1, nno2, npg, wref,&
                  vff1, dffr1, geom, mat,&
                  pesa, iu, a, tang,&
                  vect)
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
#include "asterfort/cgcine.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
!
    integer :: ndim, nno1, nno2, npg, mat, iu(3, 3)
    real(kind=8) :: vff1(nno1, npg), geom(ndim, nno1), wref(npg)
    real(kind=8) :: vect(nno1*(ndim+1) + nno2)
    real(kind=8) :: dffr1(nno1, npg)
    real(kind=8) :: a, tang(3, 3), pesa(4)
! ----------------------------------------------------------------------
!
!   CHAR_MECA_PESA_R POUR ELEMENT CABLE/GAINE
!
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE L)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  WREF    : POIDS DES POINTS DE GAUSS DE REFERENCE
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  DFFR1   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  TANG    : TANGENTE AUX NOEUDS
! IN  MAT     : MATERIAU CODE
! IN  IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT GA
! IN  A       : SECTION DE LA BARRE
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! ----------------------------------------------------------------------
    integer :: nddl, g, n, i, kk, codres
    real(kind=8) :: wg, b(4, 3), t1
    real(kind=8) :: rho, courb, l(3)
! ----------------------------------------------------------------------
!
!
    nddl = nno1*(ndim+1) + nno2
    call r8inir(nddl, 0.d0, vect, 1)
!
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do g = 1, npg
!
!      CALCUL DES ELEMENTS GEOM DE L'EF AU POINT DE GAUSS CONSIDERE
!
        call rcvalb('RIGI', g, 1, '+', mat,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, 'RHO', rho, codres, 1)
!
        call cgcine(ndim, nno1, vff1(1, g), &
                    wref(g), dffr1(1, g), geom, tang, wg,&
                    l, b, courb)
!
!        VECTEUR FINT:U ET UC
        do 300 n = 1, nno1
            do 301 i = 1, ndim
                kk = iu(i,n)
                t1 = vff1(n,g)*a*rho*pesa(1)*pesa(i+1)
                vect(kk) = vect(kk) + wg*t1
301          continue
300      continue
!
!
!
    end do
!
end subroutine
