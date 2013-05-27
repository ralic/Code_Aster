subroutine calgf(ndim, nno, axi, npg, geomi,&
                 g, iw, vff, idff, deplm,&
                 deplt, grand, alpha, r, w,&
                 dff, fm, fmp, fma)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    include 'asterfort/dfdmip.h'
    include 'asterfort/nmepsi.h'
    include 'asterfort/r8inir.h'
    logical :: axi, grand
    integer :: ndim, nno, g, iw, idff, i, j, npg
    real(kind=8) :: geomi(ndim, nno), vff(nno, npg)
    real(kind=8) :: deplm(nno*ndim), deplt(nno*ndim)
    real(kind=8) :: alpha
!
! ---------------------------------
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
!   FM      : TENSEUR DE DEFORMATION ENTRE CONFIGURATION INITIALE ET
!             CONFIGURATION A T-
!   FMP     : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET
!             CONFIGURATION A T+
!   FMA     : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET
!             CONFIGURATION A T_(N+ALPHA)
! ----------------------------
!
!  CALCUL DES TENSEURS DE DEFORMATIONS ENTRE CONFIGURATION DE REFERENCE
!  ET LES CONFIGURATIONS A L INSTANTS T-, T+ ET INTERMEDIAIRE
!
!----------------------------------------------------------------------
!
!
    real(kind=8) :: fm(3, 3), fmp(3, 3), fma(3, 3), eps(6)
    real(kind=8) :: r, w, dff(nno, ndim), tbid(6)
!
!----------------------INTIALISATION DES MATRICES ---------------------
    call r8inir(9, 0.d0, fm, 1)
    call r8inir(9, 0.d0, fmp, 1)
    call r8inir(9, 0.d0, fma, 1)
    call r8inir(6, 0.d0, eps, 1)
    call r8inir(6, 0.d0, tbid, 1)
    call r8inir(nno*ndim, 0.d0, dff, 1)
    r=0.d0
    w=0.d0
!
!--CALCUL DE F_N, F_N+1 ET F_(N+ALPHA) PAR RAPPORT A GEOMI GEOM INITIAL
!
!     CALCUL DE FM EN SORTIE FM = F_N
    call dfdmip(ndim, nno, axi, geomi, g,&
                iw, vff(1, g), idff, r, w,&
                dff)
    call nmepsi(ndim, nno, axi, grand, vff(1, g),&
                r, dff, deplm, fm, tbid)
!
!     CALCUL DE FMP EN SORTIE FMP = F_N+1
    call nmepsi(ndim, nno, axi, grand, vff(1, g),&
                r, dff, deplt, fmp, tbid)
!
!     CALCUL DE F_(N+ALPHA)
    do 11 i = 1, 3
        do 20 j = 1, 3
            fma(i,j) = fm(i,j) + alpha*(fmp(i,j)-fm(i,j))
20      continue
11  end do
end subroutine
