subroutine mmgnme(ndim  ,nnm   ,nne,mprt1n,mprt2n, &
                 wpg   , &
          ffe,dffm  ,jacobi,coefac,jeu   , &
          dlagrc,kappa ,vech1 ,vech2 ,h     , &
          matrme)
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

    implicit     none
#include "asterfort/matini.h"

    
    integer :: ndim,  nnm, nne
    
    real(kind=8) :: mprt1n(3,3),mprt2n(3,3)
    
    real(kind=8) :: ffe(9),dffm(2,9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: coefac, jeu, dlagrc
    
    real(kind=8) :: kappa(2,2),h(2,2)
    real(kind=8) :: vech1(3),vech2(3)
    
    real(kind=8) :: matrme(27, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DEPL/DEPL ----- CONTRIBUTIONS STANDARDS 
! SANS NON LINEARITES GEOMETRIQUES LIEES A LA DEUXIEME VARIATION DU GAP NORMAL
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

    integer :: i, j, k,l, ii, jj
    real(kind=8) :: g(3, 3), e(3, 3), d(3, 3), f(3, 3)
    real(kind=8) :: mprt12(3,3),mprnt1(3,3),mprnt2(3,3)
    real(kind=8) :: supkap,supmat,alpha
    alpha = 1.d-5 
!ETUDE HEURISTIQUE

    call matini(3, 3, 0.d0, e)
    call matini(3, 3, 0.d0, d)
    call matini(3, 3, 0.d0, g)
    call matini(3, 3, 0.d0, f)
    call matini(3, 3, 0.d0, mprt12)
    call matini(3, 3, 0.d0, mprnt1)
    call matini(3, 3, 0.d0, mprnt2)

!-------e =vech1/vech1  
   e(1,1) = h(1,1)*vech1(1)*vech1(1)
   e(1,2) = h(1,1)*vech1(1)*vech1(2) 
   e(1,3) = h(1,1)*vech1(1)*vech1(3) 
   e(2,1) = h(1,1)*vech1(2)*vech1(1)
   e(2,2) = h(1,1)*vech1(2)*vech1(2)
   e(2,3) = h(1,1)*vech1(2)*vech1(3)
   e(3,1) = h(1,1)*vech1(3)*vech1(1)
   e(3,2) = h(1,1)*vech1(3)*vech1(2)
   e(3,3) = h(1,1)*vech1(3)*vech1(3)
   
!-------d =vech1/vech2  
   d(1,1) = h(1,2)*vech1(1)*vech2(1)
   d(1,2) = h(1,2)*vech1(1)*vech2(2) 
   d(1,3) = h(1,2)*vech1(1)*vech2(3) 
   d(2,1) = h(1,2)*vech1(2)*vech2(1)
   d(2,2) = h(1,2)*vech1(2)*vech2(2)
   d(2,3) = h(1,2)*vech1(2)*vech2(3)
   d(3,1) = h(1,2)*vech1(3)*vech2(1)
   d(3,2) = h(1,2)*vech1(3)*vech2(2)
   d(3,3) = h(1,2)*vech1(3)*vech2(3)
   
!-------g =vech2/vech1  
   g(1,1) = h(2,1)*vech2(1)*vech2(1)
   g(1,2) = h(2,1)*vech2(1)*vech2(2) 
   g(1,3) = h(2,1)*vech2(1)*vech2(3) 
   g(2,1) = h(2,1)*vech2(2)*vech2(1)
   g(2,2) = h(2,1)*vech2(2)*vech2(2)
   g(2,3) = h(2,1)*vech2(2)*vech2(3)
   g(3,1) = h(2,1)*vech2(3)*vech2(1)
   g(3,2) = h(2,1)*vech2(3)*vech2(2)
   g(3,3) = h(2,1)*vech2(3)*vech2(3)      

!-------f =vech2/vech2  
   f(1,1) = h(2,2)*vech2(1)*vech2(1)
   f(1,2) = h(2,2)*vech2(1)*vech2(2) 
   f(1,3) = h(2,2)*vech2(1)*vech2(3) 
   f(2,1) = h(2,2)*vech2(2)*vech2(1)
   f(2,2) = h(2,2)*vech2(2)*vech2(2)
   f(2,3) = h(2,2)*vech2(2)*vech2(3)
   f(3,1) = h(2,2)*vech2(3)*vech2(1)
   f(3,2) = h(2,2)*vech2(3)*vech2(2)
   f(3,3) = h(2,2)*vech2(3)*vech2(3) 

     

!-----------mprnt1
  mprnt1(1,1) = mprt1n(1,1)
  mprnt1(2,2) = mprt1n(2,2)  
  mprnt1(3,3) = mprt1n(3,3)
  mprnt1(1,2) = mprt1n(2,1)
  mprnt1(1,3) = mprt1n(3,1)  
  mprnt1(2,1) = mprt1n(1,2)
  mprnt1(2,3) = mprt1n(3,2)  
  mprnt1(3,1) = mprt1n(1,3)
  mprnt1(3,2) = mprt1n(2,3)      

!-----------mprnt2
  mprnt2(1,1) = mprt2n(1,1)
  mprnt2(2,2) = mprt2n(2,2)  
  mprnt2(3,3) = mprt2n(3,3)
  mprnt2(1,2) = mprt2n(2,1)
  mprnt2(1,3) = mprt2n(3,1)  
  mprnt2(2,1) = mprt2n(1,2)
  mprnt2(2,3) = mprt2n(3,2)  
  mprnt2(3,1) = mprt2n(1,3)
  mprnt2(3,2) = mprt2n(2,3) 

! LES MATRICES KAPPA INFLUENCENT LA CONVERGENCE DE LA METHODE :
! OUT KAPPA  : MATRICE DE SCALAIRES LIEES A LA CINEMATIQUE DU GLISSEMENT
! OUT KAPPA(i,j) = INVERSE[TAU_i.TAU_j-JEU*(ddFFM*geomm)](matrice 2*2) 
! ON LE DEBRANCHE AUTOMATIQUEMENT SI SA VALEUR EST TROP GRANDE COMPARATIVEMENT A MATRME


supkap = kappa(1,1)
do 1 i = 1,2
  do 2 j= 1,2
    if (kappa(i,j) .ge. supkap) supkap = kappa(i,j)
2 continue
1 continue

supmat = matrme(1,1)
do 11 i = 1,27
  do 21  j= 1,27
    if (matrme(i,j) .ge. supmat) supmat = matrme(i,j)
21 continue
11 continue


  
if (supkap .le. (alpha*supmat)) then 
  
 

!
! CONTRIBUTION 1 :
! -NORM{[d(delta YPR)/delta XI)*DELTA XI]+&
![(D(DELTA YPR)/DELTA XI)*delta XI]}
!    
!

    do 160 i = 1, nnm
    do 150 j = 1, nne
        do 140 k = 1, ndim
        do 130 l = 1, ndim
            ii = ndim*(i-1)+l
            jj = ndim*(j-1)+k
 matrme(ii,jj) = matrme(ii,jj)    + &
 (dlagrc-coefac*jeu)*wpg*jacobi*ffe(i)  *(&
 dffm(1,j)*&
 (mprnt1(l,k)*kappa(1,1)+mprnt2(l,k)*kappa(2,1))+&
 dffm(2,j)*&
 (mprnt2(l,k)*kappa(1,2)+mprnt2(l,k)*kappa(2,2))  )                                               
130         continue
140         continue
150     continue
160  continue


!
!
! CONTRIBUTION 2 :  DELTA XI*H*delta XI MATREM = 0
!


!
!CONTRIBUTION 3 :
! JEU*{[(delta XI*H)+(NORM.d(delta YPR)/delta XI)]A[(delta XI*H)+&
!(NORM.d(delta YPR)/delta XI)] 
!




endif


end subroutine
