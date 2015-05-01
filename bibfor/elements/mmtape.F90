subroutine mmtape(phasep, leltf, ndim, nnl, nne,&
                  nnm, nbcps, wpg, jacobi, ffl,&
                  ffe, ffm, norm, tau1, tau2,&
                  mprojt, rese, nrese, lambda, coefff,&
                  coefaf, coefac, matrcc, matrff, matrce,&
                  matrcm, matrfe, matrfm)
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
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/mmmtcc.h"
#include "asterfort/mmmtcu.h"
#include "asterfort/mmmtff.h"
#include "asterfort/mmmtfu.h"
    character(len=9) :: phasep
    aster_logical :: leltf
    integer :: ndim, nne, nnl, nnm, nbcps
    real(kind=8) :: ffe(9), ffl(9), ffm(9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: lambda
    real(kind=8) :: coefaf, coefff
    real(kind=8) :: coefac
    real(kind=8) :: matrcc(9, 9), matrff(18, 18)
    real(kind=8) :: matrce(9, 27), matrcm(9, 27)
    real(kind=8) :: matrfe(18, 27), matrfm(18, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! CALCUL DES MATRICES - LOIS DE CONTACT/FROTTEMENT - CAS POIN_ELEM
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
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  NBCPS  : NB DE DDL DE LAGRANGE
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  NORM   : VECTEUR NORMAL
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : RACINE DE LA NORME DE RESE
! IN  LAMBDA : LAGRANGIEN DE CONTACT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFAF : COEF_AUGM_FROT
! OUT MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
! OUT MATRFF : MATRICE ELEMENTAIRE LAGR_F/LAGR_F
! OUT MATRCE : MATRICE ELEMENTAIRE LAGR_C/DEPL_E
! OUT MATRCM : MATRICE ELEMENTAIRE LAGR_C/DEPL_M
! OUT MATRFE : MATRICE ELEMENTAIRE LAGR_F/DEPL_E
! OUT MATRFM : MATRICE ELEMENTAIRE LAGR_F/DEPL_M
!
! ----------------------------------------------------------------------
!
    if (phasep(1:4) .eq. 'SANS') then
        if (leltf) then
            call mmmtff(phasep, ndim, nbcps, nnl, wpg,&
                        ffl, jacobi, tau1, tau2, rese,&
                        nrese, lambda, coefaf, coefff, matrff)
        else
            call mmmtcc(phasep, nnl, wpg, ffl, jacobi,&
                        coefac, matrcc)
        endif
    else if (phasep(1:4).eq.'CONT') then
        call mmmtcu(phasep, ndim, nnl, nne, nnm,&
                    norm, wpg, ffl, ffe, ffm,&
                    jacobi, matrce, matrcm)
        if (phasep(6:9) .eq. 'PENA') then
            call mmmtcc(phasep, nnl, wpg, ffl, jacobi,&
                        coefac, matrcc)
        endif
    else if (phasep(1:4).eq.'ADHE') then
        call mmmtfu(phasep, ndim, nnl, nne, nnm,&
                    nbcps, wpg, jacobi, ffl, ffe,&
                    ffm, tau1, tau2, mprojt, rese,&
                    nrese, lambda, coefff, matrfe, matrfm)
        if (phasep(6:9) .eq. 'PENA') then
            call mmmtff(phasep, ndim, nbcps, nnl, wpg,&
                        ffl, jacobi, tau1, tau2, rese,&
                        nrese, lambda, coefaf, coefff, matrff)
        endif
    else if (phasep(1:4).eq.'GLIS') then
        call mmmtff(phasep, ndim, nbcps, nnl, wpg,&
                    ffl, jacobi, tau1, tau2, rese,&
                    nrese, lambda, coefaf, coefff, matrff)
        call mmmtfu(phasep, ndim, nnl, nne, nnm,&
                    nbcps, wpg, jacobi, ffl, ffe,&
                    ffm, tau1, tau2, mprojt, rese,&
                    nrese, lambda, coefff, matrfe, matrfm)
    else
        ASSERT(.false.)
    endif
!
!
end subroutine
