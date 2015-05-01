subroutine mmtgeo(phasep,ndim  ,nne   ,nnm   ,mprt1n, &
              mprt2n,mprojn,mprt11,mprt21,mprt22, &
          wpg   ,ffe   ,ffm   ,dffm  ,jacobi, &
          coefac,jeu   ,dlagrc,kappa ,vech1 , &
          vech2 ,h        ,hah  , &
          matree,matrmm,matrem, matrme)
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
    implicit     none
#include "asterfort/matini.h"
#include "asterfort/mmgnuu.h"

    character(len=9) :: phasep
    
    integer :: ndim, nne, nnm
    
    real(kind=8) :: mprojn(3, 3)
    real(kind=8) :: mprt1n(3, 3), mprt2n(3, 3)
    real(kind=8) :: mprt11(3, 3), mprt21(3, 3), mprt22(3, 3)
    
    real(kind=8) :: ffe(9), ffm(9), dffm(2, 9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: coefac, jeu, dlagrc
    
    real(kind=8) :: kappa(2,2),h(2,2),hah(2,2)
    real(kind=8) :: vech1(3),vech2(3)
    
    real(kind=8) :: matrem(27, 27), matrme(27, 27)
    real(kind=8) :: matree(27, 27), matrmm(27, 27)
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
! IN  MPRT1N : MATRICE DE PROJECTION TANGENTE1/NORMALE
! IN  MPRT2N : MATRICE DE PROJECTION TANGENTE2/NORMALE
! OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
! OUT MATRMM : MATRICE ELEMENTAIRE DEPL_M/DEPL_M
! OUT MATRME : MATRICE ELEMENTAIRE DEPL_M/DEPL_E
! OUT MATREM : MATRICE ELEMENTAIRE DEPL_E/DEPL_M
!--------ON VIENT ENRICHIR LES MATRICES MATREE MATRMM MATREM MATRME
!--------AVEC LA DEUXIEME VARIATION DU GAP NORMAL
!
! ----------------------------------------------------------------------
!
!
! --- CONTRIBUTIONS MATRICE NEWTON GENERALISE
!
    if (phasep(1:4) .eq. 'CONT') then
    
      call mmgnuu(ndim  ,nne   ,nnm   ,mprt1n, &
              mprt2n,mprojn,mprt11,mprt21,mprt22, &
          wpg   ,ffe   ,ffm   ,dffm  ,jacobi, &
          coefac,jeu   ,dlagrc,kappa ,vech1 , &
          vech2 ,h     ,hah   , &
          matree,matrmm,matrem, matrme)
   
    elseif (phasep(1:4) .eq. 'GLIS') then
!Implementation de la deuxième varitation de xi
! ROUTINE DE CALCUL SUPPLEMENTAIRE EN FROTTEMENT : INEXISTANT POUR LE MOMENT 

    endif
!
end subroutine
