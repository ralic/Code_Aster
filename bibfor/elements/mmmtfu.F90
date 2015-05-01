subroutine mmmtfu(phasep, ndim, nnl, nne, nnm,&
                  nbcps, wpg, jacobi, ffl, ffe,&
                  ffm, tau1, tau2, mprojt, rese,&
                  nrese, lambda, coefff, matrfe, matrfm)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/mmmtfe.h"
#include "asterfort/mmmtfm.h"
    character(len=9) :: phasep
    integer :: ndim, nne, nnl, nnm, nbcps
    real(kind=8) :: ffe(9), ffl(9), ffm(9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: lambda
    real(kind=8) :: coefff
    real(kind=8) :: matrfe(18, 27), matrfm(18, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES MATRICES LAGR_F/DEPL
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
! IN  NBCPS  : NB DE DDL DE LAGRANGE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : RACINE DE LA NORME DE RESE
! IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! OUT MATRFE : MATRICE ELEMENTAIRE LAGR_F/DEPL_E
! OUT MATRFM : MATRICE ELEMENTAIRE LAGR_F/DEPL_M
!
! ----------------------------------------------------------------------
!
!
! --- LAGR_F/DEPL_E
!
    call mmmtfe(phasep, ndim, nne, nnl, nbcps,&
                wpg, jacobi, ffe, ffl, tau1,&
                tau2, mprojt, rese, nrese, lambda,&
                coefff, matrfe)
!
! --- LAGR_F/DEPL_M
!
    call mmmtfm(phasep, ndim, nnm, nnl, nbcps,&
                wpg, jacobi, ffm, ffl, tau1,&
                tau2, mprojt, rese, nrese, lambda,&
                coefff, matrfm)
!
end subroutine
