subroutine ejfono(ndim, nddl, axi, nno1, nno2,&
                  npg, ipg, wref, vff1, vff2,&
                  idf2, dffr2, geom, iu, ip,&
                  sigm, vect)
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
#include "asterfort/ejcine.h"
#include "asterfort/r8inir.h"
    logical :: axi
    integer :: ndim, idf2, ipg, nddl, nno1, nno2, npg, iu(3, 16), ip(4)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno2)
    real(kind=8) :: wref(npg)
    real(kind=8) :: dffr2(ndim-1, nno2, npg)
    real(kind=8) :: vect(nddl), sigm(2*ndim-1, npg)
! ----------------------------------------------------------------------
!      OPTION FORC_NODA LES JOINTS QUADRA ET HYME
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  NDDL    : NOMBRE DE DDL
! IN  AXI     : .TRUE. SI AXISYMETRIE
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE P)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE P)
! IN  DFFR2   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE P)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  WREF    : POIDS DES POINTS DE GAUSS DE REFERENCE
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! IN  IP      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION
! OUT VECT    : FORCES INTERIEURES DE REFERENCE
! ----------------------------------------------------------------------
    integer :: kpg, n, i, j, kk
    real(kind=8) :: wg, b(2*ndim-1, ndim+1, 2*nno1+nno2), temp, rot(ndim*ndim)
! ----------------------------------------------------------------------
!
    call r8inir(nddl, 0.d0, vect, 1)
!
    do 1000 kpg = 1, npg
!
        call ejcine(ndim, axi, nno1, nno2, vff1(1, kpg),&
                    vff2(1, kpg), wref(kpg), dffr2(1, 1, kpg), geom, wg,&
                    kpg, ipg, idf2, rot, b)
!
!
!       VECTEUR FINT : U
        do 300 n = 1, 2*nno1
            do 301 i = 1, ndim
!
                kk = iu(i,n)
                temp = 0.d0
                do 320 j = 1, ndim
                    temp = temp + b(j,i,n)*sigm(j,kpg)
320              continue
!
                vect(kk) = vect(kk) + wg*temp
!
301          continue
300      continue
!
!       VECTEUR FINT : P
        do 302 n = 1, nno2
!
            kk = ip(n)
            temp = 0.d0
            do 321 i = ndim+1, 2*ndim-1
                temp = temp + b(i,ndim+1,2*nno1+n)*sigm(i,kpg)
321          continue
!
            vect(kk) = vect(kk) + wg*temp
!
302      continue
!
1000  end do
end subroutine
