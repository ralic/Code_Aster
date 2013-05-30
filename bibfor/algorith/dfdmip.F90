subroutine dfdmip(ndim, nno, axi, geom, g,&
                  iw, vff, idfde, r, w,&
                  dfdi)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'blas/ddot.h'
    logical :: axi
    integer :: ndim, nno, g, iw, idfde
    real(kind=8) :: geom(ndim, nno), vff(nno), r, w, dfdi(nno, ndim)
!
! ----------------------------------------------------------------------
!     CALCUL DES DERIVEES DES FONCTIONS DE FORME ET DU JACOBIEN
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO     : NOMBRE DE NOEUDS (FAMILLE E-BARRE)
! IN  AXI     : .TRUE. SI AXISYMETRIQUE
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  G       : NUMERO DU POINT DE GAUSS COURANT
! IN  IW      : ACCES AUX POIDS DES POINTS DE GAUSS DE REFERENCE
! IN  VFF     : VALEURS DES FONCTIONS DE FORME (POINT DE GAUSS COURANT)
! IN  IDFDE   : DERIVEES DES FONCTIONS DE FORME DE REFERENCE
! OUT R       : RAYON DU POINT DE GAUSS COURANT (EN AXI)
! OUT W       : POIDS DU POINT DE GAUSS COURANT (Y COMPRIS R EN AXI)
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME (POINT DE GAUSS COURANT)
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
!
!
!
! - CALCUL DES DERIVEES DES FONCTIONS DE FORME ET JACOBIEN
    if (ndim .eq. 3) then
        call dfdm3d(nno, g, iw, idfde, geom,&
                    dfdi(1, 1), dfdi(1, 2), dfdi(1, 3), w)
    else
        call dfdm2d(nno, g, iw, idfde, geom,&
                    dfdi(1, 1), dfdi(1, 2), w)
    endif
!
!
! - CALCUL DE LA DISTANCE A L'AXE EN AXI
    if (axi) then
        r = ddot(nno,vff,1,geom,2)
        w = w*r
    endif
!
end subroutine
