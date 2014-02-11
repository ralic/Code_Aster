subroutine coef_infl_a1(ix1,ix2,iy1,iy2,i,&
                        v11,v21,v12,v22)
    implicit none
    integer :: ix1,ix2,iy1,iy2
    integer :: i
    real(kind=8) :: v11,v21,v12,v22
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ======================================================================
! --- BUT : CALCUL DES COEFFICIENTS D INFLUENCE AU POINT A POUR Er/E=1.0
! ======================================================================
! IN  : IX1    : PARAMETRE a/c : INDICE INFERIEUR ----------------------
! --- : IX2    : PARAMETRE a/c : INDICE SUPERIEUR ----------------------
! --- : IY1    : PARAMETRE a/r : INDICE INFERIEUR ----------------------
! --- : IY2    : PARAMETRE a/r : INDICE SUPERIEUR ----------------------
! --- : I      : NUMERO DU COEFFICIENT D INFLUENCE TRAITE --------------
! OUT : v11    : COEFFICIENT INFLUENCE EN IX1,IY1 ----------------------
! --- : v21    : COEFFICIENT INFLUENCE EN IX2,IY1 ----------------------
! --- : v12    : COEFFICIENT INFLUENCE EN IX1,IY2 ----------------------
! --- : v22    : COEFFICIENT INFLUENCE EN IX2,IY2 ----------------------
! ======================================================================
! ======================================================================
!-----------------------------------------------------------------------
!     operateur POST_K_BETA
!     coefficients d'influence au point A pour Er/E=1
!     attention le point A (notation EDF) correspond au point B du RSE-M
!-----------------------------------------------------------------------
!
! Les valeurs des coefficients d'influence tabulees dans cette routine
! sont extraites de la note :
!
!              RSE-M - Edition 2010
!                  ANNEXE 5.4
!      METHODES ANALYTIQUES DE CALCUL DES FACTEURS
!      D'INTENSITE DE CONTRAINTE ET DE L'INTEGRALE J
!
! Les coefficients d'influence tabulees sont extraites du  
! Tableau VII.5.3.2b : PLA-DSR - coefficients d'influence pour Er/E = 1
!                      point B (note A pour EDF)
!
!-----------------------------------------------------------------------
!
!    defaut semi-elliptique (notation EDF)
!
!       ---------2c----------
!       $$$$$$$$$$$$$$$$$$$$$   <- revetement 
!    ^  ----------A--------C-
!         \               /
!    a     \             /      <- metal de base
!           \           /
!    v        ----B----
!
!  ou Er : module d'young du revetement
!     E  : module d'young du metal de base 
!     a  : hauteur de la fissure
!    2c  : largeur de la fissure
! ======================================================================
    real(kind=8) :: coef(6,9,5)
    integer :: k
!
! a/c = 1 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(1,1,k),k=1,5) / 0.580d0, 0.580d0, 0.580d0, 0.580d0, 0.580d0 / 
    data (coef(1,2,k),k=1,5) / 0.582d0, 0.530d0, 0.484d0, 0.442d0, 0.404d0 / 
    data (coef(1,3,k),k=1,5) / 0.582d0, 0.490d0, 0.414d0, 0.350d0, 0.297d0 / 
    data (coef(1,4,k),k=1,5) / 0.584d0, 0.430d0, 0.320d0, 0.240d0, 0.182d0 / 
    data (coef(1,5,k),k=1,5) / 0.593d0, 0.359d0, 0.225d0, 0.145d0, 9.74d-2 /
    data (coef(1,6,k),k=1,5) / 0.602d0, 0.318d0, 0.178d0, 0.106d0, 6.76d-2 /
    data (coef(1,7,k),k=1,5) / 0.611d0, 0.292d0, 0.152d0, 8.70d-2, 5.43d-2 /
    data (coef(1,8,k),k=1,5) / 0.633d0, 0.263d0, 0.126d0, 6.98d-2, 4.33d-2 /
    data (coef(1,9,k),k=1,5) / 0.655d0, 0.249d0, 0.115d0, 6.29d-2, 3.92d-2 /
!
! a/c = 1/2 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(2,1,k),k=1,5) / 0.635d0, 0.635d0, 0.635d0, 0.635d0, 0.635d0 / 
    data (coef(2,2,k),k=1,5) / 0.639d0, 0.584d0, 0.535d0, 0.490d0, 0.450d0 / 
    data (coef(2,3,k),k=1,5) / 0.640d0, 0.542d0, 0.461d0, 0.393d0, 0.336d0 /
    data (coef(2,4,k),k=1,5) / 0.644d0, 0.480d0, 0.361d0, 0.276d0, 0.213d0 /
    data (coef(2,5,k),k=1,5) / 0.658d0, 0.408d0, 0.262d0, 0.174d0, 0.121d0 /
    data (coef(2,6,k),k=1,5) / 0.673d0, 0.367d0, 0.214d0, 0.133d0, 8.84d-2 /
    data (coef(2,7,k),k=1,5) / 0.688d0, 0.342d0, 0.187d0, 0.113d0, 7.39d-2 /
    data (coef(2,8,k),k=1,5) / 0.720d0, 0.316d0, 0.162d0, 0.095d0, 6.20d-2 /
    data (coef(2,9,k),k=1,5) / 0.751d0, 0.305d0, 0.151d0, 0.088d0, 5.77d-2 /
!
! a/c = 1/4 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(3,1,k),k=1,5) / 0.657d0, 0.657d0, 0.657d0, 0.657d0, 0.657d0 /
    data (coef(3,2,k),k=1,5) / 0.661d0, 0.606d0, 0.556d0, 0.510d0, 0.469d0 /
    data (coef(3,3,k),k=1,5) / 0.664d0, 0.564d0, 0.481d0, 0.411d0, 0.353d0 /
    data (coef(3,4,k),k=1,5) / 0.670d0, 0.502d0, 0.381d0, 0.293d0, 0.228d0 /
    data (coef(3,5,k),k=1,5) / 0.689d0, 0.432d0, 0.281d0, 0.190d0, 0.134d0 /
    data (coef(3,6,k),k=1,5) / 0.707d0, 0.392d0, 0.233d0, 0.148d0, 0.101d0 /
    data (coef(3,7,k),k=1,5) / 0.726d0, 0.369d0, 0.207d0, 0.128d0, 8.60d-2 /
    data (coef(3,8,k),k=1,5) / 0.763d0, 0.345d0, 0.182d0, 0.110d0, 7.41d-2 /
    data (coef(3,9,k),k=1,5) / 0.799d0, 0.335d0, 0.172d0, 0.104d0, 7.01d-2 /
!
! a/c = 1/8 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(4,1,k),k=1,5) / 0.665d0, 0.665d0, 0.665d0, 0.665d0, 0.665d0 /
    data (coef(4,2,k),k=1,5) / 0.670d0, 0.614d0, 0.564d0, 0.518d0, 0.476d0 /
    data (coef(4,3,k),k=1,5) / 0.674d0, 0.573d0, 0.489d0, 0.419d0, 0.360d0 /
    data (coef(4,4,k),k=1,5) / 0.681d0, 0.512d0, 0.389d0, 0.300d0, 0.234d0 /
    data (coef(4,5,k),k=1,5) / 0.702d0, 0.443d0, 0.290d0, 0.197d0, 0.140d0 /
    data (coef(4,6,k),k=1,5) / 0.722d0, 0.404d0, 0.242d0, 0.155d0, 0.107d0 /
    data (coef(4,7,k),k=1,5) / 0.742d0, 0.381d0, 0.216d0, 0.135d0, 9.19d-2 /
    data (coef(4,8,k),k=1,5) / 0.782d0, 0.358d0, 0.192d0, 0.118d0, 8.02d-2 /
    data (coef(4,9,k),k=1,5) / 0.822d0, 0.350d0, 0.183d0, 0.112d0, 7.66d-2 /

!
! a/c = 1/16 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(5,1,k),k=1,5) / 0.667d0, 0.667d0, 0.667d0, 0.667d0, 0.667d0 /
    data (coef(5,2,k),k=1,5) / 0.672d0, 0.616d0, 0.566d0, 0.520d0, 0.478d0 /
    data (coef(5,3,k),k=1,5) / 0.676d0, 0.576d0, 0.492d0, 0.421d0, 0.362d0 /
    data (coef(5,4,k),k=1,5) / 0.684d0, 0.515d0, 0.392d0, 0.302d0, 0.236d0 /
    data (coef(5,5,k),k=1,5) / 0.706d0, 0.446d0, 0.292d0, 0.200d0, 0.142d0 /
    data (coef(5,6,k),k=1,5) / 0.726d0, 0.407d0, 0.245d0, 0.158d0, 0.109d0 /
    data (coef(5,7,k),k=1,5) / 0.747d0, 0.385d0, 0.219d0, 0.138d0, 9.43d-2 /
    data (coef(5,8,k),k=1,5) / 0.789d0, 0.363d0, 0.196d0, 0.121d0, 8.28d-2 /
    data (coef(5,9,k),k=1,5) / 0.831d0, 0.356d0, 0.187d0, 0.116d0, 7.96d-2 /
!
! a/c = 0. ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(6,1,k),k=1,5) / 0.680d0, 0.680d0, 0.680d0, 0.680d0, 0.680d0 /
    data (coef(6,2,k),k=1,5) / 0.681d0, 0.625d0, 0.574d0, 0.528d0, 0.486d0 /
    data (coef(6,3,k),k=1,5) / 0.683d0, 0.582d0, 0.498d0, 0.427d0, 0.368d0 /
    data (coef(6,4,k),k=1,5) / 0.689d0, 0.520d0, 0.397d0, 0.307d0, 0.241d0 /
    data (coef(6,5,k),k=1,5) / 0.707d0, 0.448d0, 0.295d0, 0.203d0, 0.145d0 /
    data (coef(6,6,k),k=1,5) / 0.726d0, 0.410d0, 0.248d0, 0.161d0, 0.112d0 /
    data (coef(6,7,k),k=1,5) / 0.748d0, 0.388d0, 0.223d0, 0.141d0, 9.73d-2 /
    data (coef(6,8,k),k=1,5) / 0.793d0, 0.370d0, 0.202d0, 0.126d0, 8.70d-2 /
    data (coef(6,9,k),k=1,5) / 0.840d0, 0.366d0, 0.195d0, 0.122d0, 8.49d-2 /
!
    v11 = coef(ix1,iy1,i)
    v21 = coef(ix2,iy1,i)
    v12 = coef(ix1,iy2,i)
    v22 = coef(ix2,iy2,i)


end subroutine
