subroutine prep2(ndim, npg, g, rpa, etdpn1,&
                 sigm, jm, fda, rp, rpat,&
                 etdm, etdv, sigmam, rpt, epsm,&
                 epsmm)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/lctr2m.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tnsvec.h'
    include 'asterfort/utbtab.h'
    integer :: ndim, npg, g, i, j
    real(kind=8) :: rpa(3, 3), rpat(3, 3), etdpn1(3, 3)
    real(kind=8) :: work(9), etdm(3, 3), rac2, etdv(6)
    real(kind=8) :: sigm(2*ndim, npg), sigmm(3, 3), jm
    real(kind=8) :: tau(3, 3), fda(3, 3), fdat(3, 3)
    real(kind=8) :: rp(3, 3), rpt(3, 3), contm(3, 3)
    real(kind=8) :: sigmam(6), epsm(6), epsmm(6), epsmt(3, 3), epsmp(3, 3)
!
! ----------------------------------------------------------------------
!   G       : NUMERO DU POINT DE GAUSS COURANT
!   NDIM    : DIMENSION DE L'ESPACE
!   NPG     : NOMBRE DE POINTS DE GAUSS
!   JM      : DETERMINANT DE FM
!   RPA     :
!   SIGM    : CONTRAINTES DE CAUCHY EN T-
!   FDA     :  TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET
!             CONFIGURATION INTERMEDIAIRE
!   ETDPN1  :
!   TAU     : CONTRAINTYE DE CAUCHY
!
!
!
!   SORTIE:
!   RPAT    : TRANSPOSEE DE RPA
!   SIGMM   : MATRICE CORRESPONDANT AU VECTEUR SIGM
!   FDAT    : TRANSPOSEE DU TENSEUR DE DEFORMATION ENTRE CONFIGURATION
!             T- ET CONFIGURATION INTERMEDIAIRE
!   CONTM   : TENSEUR DES CONTRAINTES TRANSFORME POUR ENTREE DE NMCOMP
!   SIGMAM  : TENSEUR DE DEFORMATION TRANSFORME POUR ENTREE DE NMCOMP
! ----------------------------------------------------------------------
!
!     CALCUL DES ENTREE DE NMCOMP CONTM ET SIGMAM POUR GDEF_HYPO_ELAS
!
! ----------------------------------------------------------------------
!
!--------------------------INITIALISATION-------------------------------
    call r8inir(9, 0.d0, rpat, 1)
    call r8inir(9, 0.d0, etdm, 1)
    call r8inir(9, 0.d0, sigmm, 1)
    call r8inir(9, 0.d0, tau, 1)
    call r8inir(9, 0.d0, rpt, 1)
    call r8inir(9, 0.d0, contm, 1)
    call r8inir(6, 0.d0, etdv, 1)
    call r8inir(6, 0.d0, sigmam, 1)
!
    rac2=sqrt(2.d0)
!     TRANSPOSITION DE R~_N+ALPHA=RPA EN RPAT
    call lctr2m(3, rpa, rpat)
    call utbtab('ZERO', 3, 3, etdpn1, rpat,&
                work, etdm)
!
!     TRANSFORMATION MATRICE EN VECTEUR DE e~_(n+ALPHA)
    call tnsvec(3, ndim, etdm, etdv, rac2)
!
!     TRANSF MATRICE EN VECTEUR DES CONTRAINTES
!
    call tnsvec(6, ndim, sigmm, sigm(1, g), 1.d0)
!
!     TENSEUR DE KIRCHHOFF
    do 300 i = 1, 3
        do 310 j = 1, 3
            tau(i,j)= jm*sigmm(i,j)
310      continue
300  end do
!
    call lctr2m(3, fda, fdat)
!
!     TRANSPOSITION DE R_N+1=RPT
    call lctr2m(3, rp, rpt)
!
!  CALCUL DE CONTRAINTE ENTREE DE NMCOMP ET TRANSFORMATION EN VECTEUR
    call utbtab('ZERO', 3, 3, tau, rpt,&
                work, contm)
    call tnsvec(3, ndim, contm, sigmam, rac2)
!
! ajout transforùation de EPSM (pour certaines lois comme vendochab)
    call tnsvec(6, ndim, epsmt, epsm, 1.d0)
    call utbtab('ZERO', 3, 3, epsmt, rpt,&
                work, epsmp)
    call tnsvec(3, ndim, epsmp, epsmm, rac2)
!
end subroutine
