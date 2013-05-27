subroutine mmmvuu(phasez, ndim, nne, nnm, norm,&
                  tau1, tau2, mprojt, wpg, ffe,&
                  ffm, jacobi, jeu, coefac, coefaf,&
                  lambda, coefff, dlagrc, dlagrf, dvite,&
                  rese, nrese, vectee, vectmm)
!
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
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_21
!
    implicit none
    include 'asterfort/mmmvee.h'
    include 'asterfort/mmmvmm.h'
    character(len=*) :: phasez
    integer :: ndim, nne, nnm
    real(kind=8) :: wpg, ffe(9), ffm(9), jacobi
    real(kind=8) :: dlagrc, dlagrf(2), dvite(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: norm(3)
    real(kind=8) :: tau1(3), tau2(3), mprojt(3, 3)
    real(kind=8) :: coefac, coefaf, jeu
    real(kind=8) :: lambda, coefff
    real(kind=8) :: vectee(27), vectmm(27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DU VECTEUR DEPL
!
! ----------------------------------------------------------------------
!
!
! IN  PHASEP : PHASE DE CALCUL
!              'CONT'      - CONTACT
!              'CONT_PENA' - CONTACT PENALISE
!              'ADHE'      - ADHERENCE
!              'ADHE_PENA' - ADHERENCE PENALISE
!              'ADHE'      - GLISSEMENT
!              'ADHE_PENA' - GLISSEMENT PENALISE
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS MAITRES
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFE    : FONCTIONS DE FORMES DEPL_ESCL
! IN  FFM    : FONCTIONS DE FORMES DEPL_MAIT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  JEU    : VALEUR DU JEU
! IN  NORM   : NORMALE
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFAF : COEF_AUGM_FROT
! IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! IN  DVITE  : SAUT DE "VITESSE" [[DELTA X]]
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE
! OUT VECTEE : VECTEUR ELEMENTAIRE DEPL_ESCL
! OUT VECTMM : VECTEUR ELEMENTAIRE DEPL_MAIT
!
! ----------------------------------------------------------------------
!
    character(len=9) :: phasep
!
! ----------------------------------------------------------------------
!
    phasep = phasez
!
! --- DEPL_ESCL
!
    call mmmvee(phasep, ndim, nne, norm, tau1,&
                tau2, mprojt, wpg, ffe, jacobi,&
                jeu, coefac, coefaf, lambda, coefff,&
                dlagrc, dlagrf, dvite, rese, nrese,&
                vectee)
!
! --- DEPL_MAIT
!
    call mmmvmm(phasep, ndim, nnm, norm, tau1,&
                tau2, mprojt, wpg, ffm, jacobi,&
                jeu, coefac, coefaf, lambda, coefff,&
                dlagrc, dlagrf, dvite, rese, nrese,&
                vectmm)
!
end subroutine
