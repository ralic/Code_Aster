subroutine mmtfpe(phasep, iresof, ndim, nne, nnm,&
                  nnl, nbcps, wpg, jacobi, ffl,&
                  ffe, ffm, dffm, norm, tau1,&
                  tau2, mprojn, mprojt, rese, nrese,&
                  lambda, jeu, coefff, coefaf, coefac,&
                  dlagrc, dlagrf, djeut, matree, matrmm,&
                  matrem, matrme, matrec, matrmc, matref,&
                  matrmf)
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
    include 'asterfort/matini.h'
    include 'asterfort/mmmtuc.h'
    include 'asterfort/mmmtuf.h'
    include 'asterfort/mmmtuu.h'
    character(len=9) :: phasep
    integer :: ndim, nne, nnm, nnl, nbcps
    integer :: iresof
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: mprojn(3, 3), mprojt(3, 3)
    real(kind=8) :: ffe(9), ffm(9), ffl(9)
    real(kind=8) :: wpg, jacobi, dffm(2, 9)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: lambda, coefff, jeu
    real(kind=8) :: dlagrf(2), dlagrc
    real(kind=8) :: djeut(3)
    real(kind=8) :: matrem(27, 27), matrme(27, 27)
    real(kind=8) :: matree(27, 27), matrmm(27, 27)
    real(kind=8) :: matrec(27, 9), matrmc(27, 9)
    real(kind=8) :: matrmf(27, 18), matref(27, 18)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! CALCUL DES MATRICES - EQUATION EQUILIBRE - CAS POIN_ELEM
!
! ----------------------------------------------------------------------
!
!
! IN  PHASEP : 'SANS' - PAS DE CONTACT
!              'CONT' - CONTACT
!              'ADHE' - CONTACT ADHERENT
!              'GLIS' - CONTACT GLISSANT
!              'SANS_PENA' - PENALISATION - PAS DE CONTACT
!              'CONT_PENA' - PENALISATION - CONTACT
!              'ADHE_PENA' - PENALISATION - CONTACT ADHERENT
!              'GLIS_PENA' - PENALISATION - CONTACT GLISSANT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NBCPS  : NB DE DDL DE LAGRANGE
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  NORM   : NORMALE AU POINT DE CONTACT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJN : MATRICE DE PROJECTION NORMALE [Pn]
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFAF : COEF_AUGM_FROT
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
! IN  LAMBDA : LAGRANGIEN DE CONTACT
! IN  IRESOF : ALGO. DE RESOLUTION POUR LE FROTTEMENT
!              0 - POINT FIXE
!              1 - NEWTON PARTIEL
!              2 - NEWTON COMPLET
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! OUT MATREC : MATRICE ELEMENTAIRE DEPL_E/LAGR_C
! OUT MATRMC : MATRICE ELEMENTAIRE DEPL_M/LAGR_C
! OUT MATREF : MATRICE ELEMENTAIRE DEPL_E/LAGR_F
! OUT MATRMF : MATRICE ELEMENTAIRE DEPL_M/LAGR_F
! OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
! OUT MATRMM : MATRICE ELEMENTAIRE DEPL_M/DEPL_M
! OUT MATRME : MATRICE ELEMENTAIRE DEPL_M/DEPL_E
! OUT MATREM : MATRICE ELEMENTAIRE DEPL_E/DEPL_M
!
! ----------------------------------------------------------------------
!
    logical :: lnewtg
    real(kind=8) :: h11t1n(3, 3), h12t2n(3, 3)
    real(kind=8) :: h21t1n(3, 3), h22t2n(3, 3)
!
! ----------------------------------------------------------------------
!
    lnewtg = .false.
!
    call matini(3, 3, 0.d0, h11t1n)
    call matini(3, 3, 0.d0, h12t2n)
    call matini(3, 3, 0.d0, h21t1n)
    call matini(3, 3, 0.d0, h22t2n)
!
    if (phasep(1:4) .eq. 'CONT') then
        call mmmtuc(phasep, ndim, nnl, nne, nnm,&
                    norm, tau1, tau2, mprojt, wpg,&
                    ffl, ffe, ffm, jacobi, coefff,&
                    coefaf, dlagrf, djeut, rese, nrese,&
                    matrec, matrmc)
        call mmmtuu(phasep, lnewtg, ndim, nne, nnm,&
                    mprojn, mprojt, wpg, ffe, ffm,&
                    dffm, jacobi, coefac, coefaf, coefff,&
                    rese, nrese, lambda, jeu, dlagrc,&
                    h11t1n, h12t2n, h21t1n, h22t2n, matree,&
                    matrmm, matrem, matrme)
    else if (phasep(1:4).eq.'ADHE') then
        call mmmtuf(phasep, ndim, nnl, nne, nnm,&
                    nbcps, wpg, jacobi, ffl, ffe,&
                    ffm, tau1, tau2, mprojt, rese,&
                    nrese, lambda, coefff, matref, matrmf)
        call mmmtuu(phasep, lnewtg, ndim, nne, nnm,&
                    mprojn, mprojt, wpg, ffe, ffm,&
                    dffm, jacobi, coefac, coefaf, coefff,&
                    rese, nrese, lambda, jeu, dlagrc,&
                    h11t1n, h12t2n, h21t1n, h22t2n, matree,&
                    matrmm, matrem, matrme)
        if (iresof .ge. 1) then
            call mmmtuc(phasep, ndim, nnl, nne, nnm,&
                        norm, tau1, tau2, mprojt, wpg,&
                        ffl, ffe, ffm, jacobi, coefff,&
                        coefaf, dlagrf, djeut, rese, nrese,&
                        matrec, matrmc)
        endif
    else if (phasep(1:4).eq.'GLIS') then
        call mmmtuu(phasep, lnewtg, ndim, nne, nnm,&
                    mprojn, mprojt, wpg, ffe, ffm,&
                    dffm, jacobi, coefac, coefaf, coefff,&
                    rese, nrese, lambda, jeu, dlagrc,&
                    h11t1n, h12t2n, h21t1n, h22t2n, matree,&
                    matrmm, matrem, matrme)
        call mmmtuf(phasep, ndim, nnl, nne, nnm,&
                    nbcps, wpg, jacobi, ffl, ffe,&
                    ffm, tau1, tau2, mprojt, rese,&
                    nrese, lambda, coefff, matref, matrmf)
        if (iresof .ge. 1) then
            call mmmtuc(phasep, ndim, nnl, nne, nnm,&
                        norm, tau1, tau2, mprojt, wpg,&
                        ffl, ffe, ffm, jacobi, coefff,&
                        coefaf, dlagrf, djeut, rese, nrese,&
                        matrec, matrmc)
        endif
    endif
!
end subroutine
