subroutine calpf(ndim, nno, axi, npg, geomm,&
                 g, iw, vff, idff, depld,&
                 grand, alpha, r, dff, fd,&
                 deplda, fda)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nmepsi.h"
#include "asterfort/r8inir.h"
    aster_logical :: axi, grand
    integer :: ndim, nno, g, iw, idff, i, npg
    real(kind=8) :: geomm(ndim, nno), vff(nno, npg)
    real(kind=8) :: r, dff(nno, ndim), depld(3*27)
    real(kind=8) :: alpha
!
! ----------------------------------------------------------------------
!   NDIM    : DIMENSION DE L'ESPACE
!   NNO     : NOMBRE DE NOEUDS (FAMILLE E-BARRE)
!   AXI     : .TRUE. SI AXISYMETRIQUE
!   GRAND   : .TRUE.  --> CALCUL DE F(3,3)
!             .FALSE. --> CALCUL DE EPS(6)
!   GEOMI   : COORDONNEES DES NOEUDS A L'INSTANT T-
!   G       : NUMERO DU POINT DE GAUSS COURANT
!   IW      : ACCES AUX POIDS DES POINTS DE GAUSS DE REFERENCE
!   VFF     : VALEURS DES FONCTIONS DE FORME (POINT DE GAUSS COURANT)
!   IDFF    : DERIVEES DES FONCTIONS DE FORME DE REFERENCE
!   R       : RAYON DU POINT DE GAUSS COURANT (EN AXI)
!   W       : POIDS DU POINT DE GAUSS COURANT (Y COMPRIS R EN AXI)
!   DFF     : DERIVEE DES FONCTIONS DE FORME (POINT DE GAUSS COURANT)
!   DEPLM   : DEPLACEMENT EN T-
!   DEPLT   : INCREMENT DE DEPLACEMENT ENTRE T- ET T+
!   ALPHA   : PARAMETRE D INTEGRATION COMPRIS ENTRE 0 ET 1
!
!
!   SORTIE:
!   FD      : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET T+
!   DEPLDA  : INCREMENT DE DEPLACEMENT ENTRE CONFIGURATION T- ET
!             CONFIGURATION INTERMEDIAIRE
!   FDA    :  TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET
!             CONFIGURATION INTERMEDIAIRE
!
! ----------------------------------------------------------------------
!
!     CALCUL DES TENSEURS DE DEFORMATIONS ENTRE CONFIGURATION T- ET T+
!     CALCUL DES TENSEURS DE DEFORMATIONS ENTRE CONFIGURATION T- ET
!     CONFIGURATION INTERMEDIAIRE
!
!----------------------------------------------------------------------
!
    real(kind=8) :: fd(3, 3), fda(3, 3), deplda(3*27), tbid(6)
    real(kind=8) :: rbid
!
!----------------------INTIALISATION DES MATRICES ------------------
    call r8inir(9, 0.d0, fd, 1)
    call r8inir(9, 0.d0, fda, 1)
    call r8inir(3*27, 0.d0, deplda, 1)
    call r8inir(6, 0.d0, tbid, 1)
    call r8inir(nno*ndim, 0.d0, dff, 1)
    r=0.d0
    rbid=0.d0
!
!----------------CALCUL DE f_(n+1) ET f_(n+alpha) PAR RAPPORT A GEOM T-
!      CALCUL DE f_(n+1) = FD
    call dfdmip(ndim, nno, axi, geomm, g,&
                iw, vff(1, g), idff, r, rbid,&
                dff)
    call nmepsi(ndim, nno, axi, grand, vff(1, g),&
                r, dff, depld, fd, tbid)
!
!      CALCUL DU DEPLACEMENT MULTIPLIE PAR ALPHA
    do 10 i = 1, nno*ndim
        deplda(i)=alpha*depld(i)
 10 continue
!
!      CALCUL DE  FDA = f_(n+alpha)
    call dfdmip(ndim, nno, axi, geomm, g,&
                iw, vff(1, g), idff, r, rbid,&
                dff)
    call nmepsi(ndim, nno, axi, grand, vff(1, g),&
                r, dff, deplda, fda, tbid)
end subroutine
