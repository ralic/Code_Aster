subroutine mmvape(phasep, leltf, ndim, nnl, nbcps,&
                  coefac, coefaf, coefff, ffl, wpg,&
                  jeu, jacobi, lambda, tau1, tau2,&
                  mprojt, dlagrc, dlagrf, dvite, rese,&
                  vectcc, vectff)
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
    include 'asterfort/assert.h'
    include 'asterfort/mmmvcc.h'
    include 'asterfort/mmmvff.h'
    character(len=9) :: phasep
    logical :: leltf
    integer :: ndim, nnl, nbcps
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: coefff
    real(kind=8) :: ffl(9)
    real(kind=8) :: jeu, wpg
    real(kind=8) :: tau1(3), tau2(3), rese(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: jacobi, lambda
    real(kind=8) :: dlagrc, dlagrf(2), dvite(3)
    real(kind=8) :: vectcc(9), vectff(18)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! CALCUL DES VECTEURS - LOIS DE CONTACT/FROTTEMENT - CAS POIN_ELEM
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
! IN  LELTF  : .TRUE. SI ELEMENT COMPORTANT DES DDL DE FROTTEMENT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNL    : NOMBRE DE NOEUDS LAGRANGE
! IN  NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  COEFAF : COEF_AUGM_FROT
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  JACOBI : JACOBIEN DE LA MAILLE ESCLAVE
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
! IN  JEU    : VALEUR DU JEU
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE
! IN  DVITE  : SAUT DE "VITESSE" [[DELTA X]]
! IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! IN  DLAGRF : INCREMENT DEPDEL DU LAGRANGIEN DE FROTTEMENT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! OUT VECTCC : VECTEUR ELEMENTAIRE LAGR_C
! OUT VECTFF : VECTEUR ELEMENTAIRE LAGR_F
!
! ----------------------------------------------------------------------
!
    if (phasep(1:4) .eq. 'SANS') then
        if (leltf) then
            call mmmvff(phasep, ndim, nnl, nbcps, wpg,&
                        ffl, tau1, tau2, jacobi, coefaf,&
                        dlagrf, rese, lambda, coefff, dvite,&
                        mprojt, vectff)
        else
            call mmmvcc(phasep, nnl, wpg, ffl, jacobi,&
                        jeu, coefac, dlagrc, vectcc)
        endif
    else if (phasep(1:4).eq.'CONT') then
        call mmmvcc(phasep, nnl, wpg, ffl, jacobi,&
                    jeu, coefac, dlagrc, vectcc)
    else if (phasep(1:4).eq.'ADHE') then
        call mmmvff(phasep, ndim, nnl, nbcps, wpg,&
                    ffl, tau1, tau2, jacobi, coefaf,&
                    dlagrf, rese, lambda, coefff, dvite,&
                    mprojt, vectff)
    else if (phasep(1:4).eq.'GLIS') then
        call mmmvff(phasep, ndim, nnl, nbcps, wpg,&
                    ffl, tau1, tau2, jacobi, coefaf,&
                    dlagrf, rese, lambda, coefff, dvite,&
                    mprojt, vectff)
    else
        call assert(.false.)
    endif
!
end subroutine
