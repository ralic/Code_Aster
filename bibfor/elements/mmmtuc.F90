subroutine mmmtuc(phasep, ndim, nnl, nne, nnm,&
                  norm, tau1, tau2, mprojt, wpg,&
                  ffl, ffe, ffm, jacobi, coefff,&
                  coefaf, dlagrf, djeut, rese, nrese,&
                  matrec, matrmc)
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
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit none
    include 'asterfort/mmmtec.h'
    include 'asterfort/mmmtmc.h'
    character(len=9) :: phasep
    integer :: ndim, nne, nnl, nnm
    real(kind=8) :: ffe(9), ffl(9), ffm(9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: dlagrf(2)
    real(kind=8) :: djeut(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: coefff, coefaf
    real(kind=8) :: matrec(27, 9), matrmc(27, 9)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES MATRICES DEPL/LAGR_C
!
! ----------------------------------------------------------------------
!
!
! IN  PHASEP : PHASE DE CALCUL
!              'CONT'      - CONTACT
!              'CONT_PENA' - CONTACT PENALISE
!              'ADHE'      - ADHERENCE
!              'ADHE_PENA' - ADHERENCE PENALISE
!              'GLIS'      - GLISSEMENT
!              'GLIS_PENA' - GLISSEMENT PENALISE
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  NORM   : NORMALE AU POINT DE CONTACT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  COEFAF : COEF_AUGM_FROT
! IN  DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! OUT MATREC : MATRICE ELEMENTAIRE DEPL_E/LAGR_C
! OUT MATRMC : MATRICE ELEMENTAIRE DEPL_M/LAGR_C
!
! ----------------------------------------------------------------------
!
!
!
! --- DEPL_E/LAGR_C
!
    call mmmtec(phasep, ndim, nnl, nne, norm,&
                tau1, tau2, mprojt, wpg, ffl,&
                ffe, jacobi, coefff, coefaf, dlagrf,&
                djeut, rese, nrese, matrec)
!
! --- DEPL_M/LAGR_C
!
    call mmmtmc(phasep, ndim, nnl, nnm, norm,&
                tau1, tau2, mprojt, wpg, ffl,&
                ffm, jacobi, coefff, coefaf, dlagrf,&
                djeut, rese, nrese, matrmc)
!
end subroutine
