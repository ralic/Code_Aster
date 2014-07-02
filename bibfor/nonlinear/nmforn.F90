subroutine nmforn(ndim, nno1, nno2, npg, iw,&
                  vff1, vff2, idfde1, geom, vect)
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
!
#include "asterf_types.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nmgvdn.h"
#include "asterfort/nmmabu.h"
#include "asterfort/r8inir.h"
#include "asterfort/terefe.h"
    integer :: ndim, nno1, nno2, npg, idfde1, iw
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg)
    real(kind=8) :: geom(ndim, nno1)
    real(kind=8) :: vect(*)
! ---------------------------------------------------------------------
!
!     FORC_NODA POUR GRAD_VARI (2D ET 3D)
!
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  IDFDE1  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS DE REFERENCE (INDICE)
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! ---------------------------------------------------------------------
!
    aster_logical :: grand, axi
    integer :: nddl, ndimsi, g, n, i, kl, kk
    integer :: iu(3*27), ia(8)
    real(kind=8) :: dfdi1(27, 3)
    real(kind=8) :: r, wg, b(6, 3, 27)
    real(kind=8) :: t1, sigref, varref
! ---------------------------------------------------------------------
!
! - INITIALISATION
!
    grand = .false.
    axi = .false.
    nddl = nno1*ndim + nno2
    ndimsi = 2*ndim
!
!
! --- VALEURS DE REFERENCE POUR REFE_FORC_NODA
!
    call terefe('SIGM_REFE', 'MECA_GRADVARI', sigref)
    call terefe('VARI_REFE', 'MECA_GRADVARI', varref)
!
    call r8inir(nddl, 0.d0, vect, 1)
!
    call nmgvdn(ndim, nno1, nno2, iu, ia)
!
    do 1000 g = 1, npg
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U
!
        call dfdmip(ndim, nno1, axi, geom, g,&
                    iw, vff1(1, g), idfde1, r, wg,&
                    dfdi1)
        call nmmabu(ndim, nno1, axi, grand, dfdi1,&
                    b)
        do 300 n = 1, nno1
            do 310 i = 1, ndim
                kk = iu(nno1*(i-1)+n)
                t1 = 0
                do 320 kl = 1, ndimsi
                    t1 = t1 + abs(b(kl,i,n))
320             continue
                vect(kk) = vect(kk) + wg*t1*sigref
310         continue
300     continue
!
        do 400 n = 1, nno2
            kk = ia(n)
            vect(kk) = vect(kk) + wg*vff2(n,g)*varref
400     continue
!
!
1000 end do
!
end subroutine
