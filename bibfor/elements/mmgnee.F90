subroutine mmgnee(ndim  ,nne   ,wpg   ,ffe   , &
          jacobi,coefac,jeu   ,dlagrc,vech1 , &
          vech2 ,hah   ,kappa ,mprt11,mprt21, &
          mprt22,matree)
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

    implicit     none
#include "asterfort/matini.h"
#include "asterfort/mmmmpb.h"
#include "asterfort/pmat.h"
#include "asterfort/pmavec.h"
#include "asterfort/vecini.h"



    
    integer :: ndim, nne
    

    real(kind=8) :: mprt11(3, 3), mprt21(3, 3), mprt22(3, 3)
    
    real(kind=8) :: ffe(9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: coefac, jeu, dlagrc
    
    real(kind=8) :: kappa(2,2),h(2,2),hah(2,2)
    real(kind=8) :: vech1(3),vech2(3)
    
    real(kind=8) :: matree(27, 27)
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
! OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
!
! ----------------------------------------------------------------------


    integer :: i, j,  ii, jj
    real(kind=8) :: g(3, 3), e(3, 3), d(3, 3), f(3, 3),mprt12(3,3)
    real(kind=8) :: supkap,supmat,alpha
    integer :: inoe1, inoe2, idim1, idim2
    alpha = 1.d-5
! ETUDE HEURISTIQUE

    call matini(3, 3, 0.d0, e)
    call matini(3, 3, 0.d0, d)
    call matini(3, 3, 0.d0, g)
    call matini(3, 3, 0.d0, f)
    call matini(3, 3, 0.d0, mprt12)

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

!-----------mprt12
  mprt12(1,1) = mprt21(1,1)
  mprt12(2,2) = mprt21(2,2)  
  mprt12(3,3) = mprt21(3,3)
  mprt12(1,2) = mprt21(2,1)
  mprt12(1,3) = mprt21(3,1)  
  mprt12(2,1) = mprt21(1,2)
  mprt12(2,3) = mprt21(3,2)  
  mprt12(3,1) = mprt21(1,3)
  mprt12(3,2) = mprt21(2,3)  
  
      
! LES MATRICES KAPPA INFLUENCENT LA CONVERGENCE DE LA METHODE :
! OUT KAPPA  : MATRICE DE SCALAIRES LIEES A LA CINEMATIQUE DU GLISSEMENT
! OUT KAPPA(i,j) = INVERSE[TAU_i.TAU_j-JEU*(ddFFM*geomm)](matrice 2*2) 
! ON LE DEBRANCHE AUTOMATIQUEMENT SI SA VALEUR EST TROP GRANDE COMPARATIVEMENT A MATREE


supkap = kappa(1,1)
do 1 i = 1,2
  do 2 j= 1,2
    if (kappa(i,j) .ge. supkap) supkap = kappa(i,j)
2 continue
1 continue

supmat = matree(1,1)
do 11 i = 1,27
  do 21  j= 1,27
    if (matree(i,j) .ge. supmat) supmat = matree(i,j)
21 continue
11 continue


!
! CONTRIBUTION 1 :
! -NORM{[d(delta YPR)/delta XI)*DELTA XI]+[(D(DELTA YPR)/DELTA XI)*delta XI]}
!    MATREE = 0
!


!
! CONTRIBUTION 2 :  DELTA XI*H*delta XI MATREM = 0
!

if (supkap .le. (alpha*supmat)) then

     do 161 inoe1 = 1, nne
       do 151 inoe2 = 1, nne
       do 141 idim2 = 1, ndim
           do 131 idim1 = 1, ndim
           ii = ndim*(inoe1-1)+idim1
           jj = ndim*(inoe2-1)+idim2
           matree(ii,jj) = matree(ii,jj)           - &
             (dlagrc-coefac*jeu)* wpg*jacobi*ffe(inoe1)* &
                   e(idim1,idim2) *ffe(inoe2)              - &
             (dlagrc-coefac*jeu)* wpg*jacobi*ffe(inoe1)* &
            d(idim1,idim2) *ffe(inoe2)             - &
             (dlagrc-coefac*jeu)* wpg*jacobi*ffe(inoe1)* &
            g(idim1,idim2) *ffe(inoe2)             - &
             (dlagrc-coefac*jeu)* wpg*jacobi*ffe(inoe1)* &
            f(idim1,idim2) *ffe(inoe2)
131        continue
141        continue
151    continue
161   continue

!
!CONTRIBUTION 3 :
!JEU*{[(delta XI*H)+(NORM.d(delta YPR)/delta XI)]A[(delta XI*H)+(NORM.d(delta YPR)/delta XI)] 
!
! 


if (ndim .eq.3) then 

     do 261 inoe1 = 1, nne
    do 251 inoe2 = 1, nne
        do 241 idim2 = 1, ndim
        do 231 idim1 = 1, ndim
    ii = ndim*(inoe1-1)+idim1
    jj = ndim*(inoe2-1)+idim2
    matree(ii,jj) = matree(ii,jj)                             + &
     (dlagrc-coefac*jeu)* wpg*jacobi*jeu*ffe(inoe1)                  * &
  mprt11(idim1,idim2)*(kappa(1,1)**2*hah(1,1)+2.d0*kappa(1,1)*kappa(1,2)*hah(1,2)     + &
     kappa(1,2)**2*hah(2,2))*ffe(inoe2)                         + &
    (dlagrc-coefac*jeu)* wpg*jacobi*jeu*ffe(inoe1)                   * &
  mprt12(idim1,idim2)*(kappa(2,1)*kappa(1,1)*hah(1,1)+2.d0*kappa(1,1)*kappa(2,2)*hah(1,2) + &
    kappa(1,2)**2*hah(2,2))*ffe(inoe2)                         + &
    (dlagrc-coefac*jeu)* wpg*jacobi*jeu*ffe(inoe1)                   * &
  mprt21(idim1,idim2)*(kappa(2,1)*kappa(1,1)*hah(1,1)+2.d0*kappa(1,2)*kappa(2,1)*hah(1,2) + &
    kappa(1,2)*kappa(2,2)*hah(2,2))*ffe(inoe2)                     + &
    (dlagrc-coefac*jeu)* wpg*jacobi*jeu*ffe(inoe1)                   * &
  mprt22(idim1,idim2)*(kappa(2,1)**2*hah(1,1)+2.d0*kappa(1,2)*kappa(2,2)*hah(1,2)     + &
    kappa(2,2)**2*hah(2,2))*ffe(inoe2)  
      
231         continue
241         continue
251     continue
261   continue
            
else if (ndim .eq.2) then 

     do 361 inoe1 = 1, nne
    do 351 inoe2 = 1, nne
        do 341 idim2 = 1, ndim
        do 331 idim1 = 1, ndim
    ii = ndim*(inoe1-1)+idim1
    jj = ndim*(inoe2-1)+idim2
    matree(ii,jj) = matree(ii,jj)                             + &
     (dlagrc-coefac*jeu)* wpg*jacobi*jeu*ffe(inoe1)                  * &
  mprt11(idim1,idim2)*(kappa(1,1)**2*hah(1,1)+2.d0*kappa(1,1)*kappa(1,2)*hah(1,2)     + &
     kappa(1,2)**2*hah(2,2))*ffe(inoe2)  


331         continue
341         continue
351     continue
361   continue

endif

endif

end subroutine
