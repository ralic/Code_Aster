subroutine mmmtuu(phasep, lnewtg, ndim, nne, nnm,&
                  mprojn, mprojt, wpg, ffe, ffm,&
                  dffm, jacobi, coefac, coefaf, coefff,&
                  rese, nrese, lambda, jeu, dlagrc,&
                  h11t1n, h12t2n, h21t1n, h22t2n, matree,&
                  matrmm, matrem, matrme)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit     none
    include 'asterfort/mmmtee.h'
    include 'asterfort/mmmtem.h'
    include 'asterfort/mmmtme.h'
    include 'asterfort/mmmtmm.h'
    character(len=9) :: phasep
    logical :: lnewtg
    integer :: ndim, nne, nnm
    real(kind=8) :: mprojn(3, 3), mprojt(3, 3)
    real(kind=8) :: ffe(9), ffm(9), dffm(2, 9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: coefac, coefaf, jeu, dlagrc
    real(kind=8) :: lambda, coefff
    real(kind=8) :: h11t1n(3, 3), h12t2n(3, 3)
    real(kind=8) :: h21t1n(3, 3), h22t2n(3, 3)
    real(kind=8) :: matrem(27, 27), matrme(27, 27)
    real(kind=8) :: matree(27, 27), matrmm(27, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DEPL/DEPL
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
! IN  LNEWTG : .TRUE. SI CALCUL CONTRIBUTION GEOMETRIQUE EN NEWTON GENE.
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  MPROJN : MATRICE DE PROJECTION NORMALE [Pn]
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  DFFM   : DERIVEES PREMIERES DES FONCTIONS DE FORME MAITRES
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFAF : COEF_AUGM_FROT
! IN  LAMBDA : LAGRANGIEN DE CONTACT
! IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! IN  DJEU   : INCREMENT DEPDEL DU JEU
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  H11T1N : MATRICE
! IN  H21T1N : MATRICE
! IN  H12T2N : MATRICE
! IN  H22T2N : MATRICE
! OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
! OUT MATRMM : MATRICE ELEMENTAIRE DEPL_M/DEPL_M
! OUT MATRME : MATRICE ELEMENTAIRE DEPL_M/DEPL_E
! OUT MATREM : MATRICE ELEMENTAIRE DEPL_E/DEPL_M
!
! ----------------------------------------------------------------------
!
!
!
!
! --- DEPL_ESCL/DEPL_ESCL
!
    call mmmtee(phasep, ndim, nne, mprojn, mprojt,&
                wpg, ffe, jacobi, coefac, coefaf,&
                coefff, rese, nrese, lambda, matree)
!
! --- DEPL_MAIT/DEPL_MAIT
!
    call mmmtmm(phasep, lnewtg, ndim, nnm, mprojn,&
                mprojt, wpg, ffm, dffm, jacobi,&
                coefac, coefaf, coefff, rese, nrese,&
                lambda, dlagrc, jeu, h11t1n, h12t2n,&
                h21t1n, h22t2n, matrmm)
!
! --- DEPL_ESCL/DEPL_MAIT
!
    call mmmtem(phasep, lnewtg, ndim, nne, nnm,&
                mprojn, mprojt, wpg, ffe, ffm,&
                dffm, jacobi, coefac, coefaf, coefff,&
                rese, nrese, lambda, dlagrc, jeu,&
                h11t1n, h12t2n, h21t1n, h22t2n, matrem)
!
! --- DEPL_MAIT/DEPL_ESCL
!
    call mmmtme(phasep, lnewtg, ndim, nne, nnm,&
                mprojn, mprojt, wpg, ffe, ffm,&
                dffm, jacobi, coefac, coefaf, coefff,&
                rese, nrese, lambda, dlagrc, jeu,&
                h11t1n, h12t2n, h21t1n, h22t2n, matrme)
!
end subroutine
