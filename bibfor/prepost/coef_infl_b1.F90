subroutine coef_infl_b1(ix1,ix2,iy1,iy2,i, &
                        v11,v21,v12,v22)
    implicit none
    integer :: ix1,ix2,iy1,iy2
    integer :: i
    real(kind=8) :: v11,v21,v12,v22
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! IN  : IX1    : PARAMETRE a/c : INDICE INFERIEUR ----------------------
! --- : IX2    : PARAMETRE a/c : INDICE SUPERIEUR ----------------------
! --- : IY1    : PARAMETRE a/r : INDICE INFERIEUR ----------------------
! --- : IY2    : PARAMETRE a/r : INDICE SUPERIEUR ----------------------
! --- : I      : NUMERO DU COEFFICIENT D'INFLUENCE TRAITE --------------
! OUT : v11    : COEFFICIENT INFLUENCE EN IX1,IY1 ----------------------
! --- : v21    : COEFFICIENT INFLUENCE EN IX2,IY1 ----------------------
! --- : v12    : COEFFICIENT INFLUENCE EN IX1,IY2 ----------------------
! --- : v22    : COEFFICIENT INFLUENCE EN IX2,IY2 ----------------------
! ======================================================================
! ======================================================================
!-----------------------------------------------------------------------
!     operateur POST_K_BETA
!     coefficients d'influence au point B pour Er/E=1
!     attention le point B (notation EDF) correspond au point A du RSE-M
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
! Tableau VII.5.3.2a : PLA-DSR - coefficients d'influence pour Er/E = 1
!                      points A (B pour EDF) et C
!
! Elles correspondent a celles du point A (B pour EDF) pour Er/E = 1
!
!-----------------------------------------------------------------------
!
!    defaut semi-elliptique  (notation EDF)
!
!       ----------2c---------
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
!
    real(kind=8) :: coef(6,9,5)
    integer :: k
!
! a/c = 1 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(1,1,k),k=1,5) / 0.550d0, 0.550d0, 0.550d0, 0.550d0, 0.550d0 /
    data (coef(1,2,k),k=1,5) / 0.550d0, 0.537d0, 0.524d0, 0.512d0, 0.500d0 /
    data (coef(1,3,k),k=1,5) / 0.551d0, 0.526d0, 0.504d0, 0.484d0, 0.466d0 /
    data (coef(1,4,k),k=1,5) / 0.551d0, 0.511d0, 0.477d0, 0.448d0, 0.423d0 /
    data (coef(1,5,k),k=1,5) / 0.554d0, 0.493d0, 0.445d0, 0.409d0, 0.379d0 /
    data (coef(1,6,k),k=1,5) / 0.557d0, 0.482d0, 0.429d0, 0.389d0, 0.357d0 /
    data (coef(1,7,k),k=1,5) / 0.560d0, 0.476d0, 0.418d0, 0.376d0, 0.345d0 /
    data (coef(1,8,k),k=1,5) / 0.565d0, 0.468d0, 0.406d0, 0.363d0, 0.331d0 /
    data (coef(1,9,k),k=1,5) / 0.570d0, 0.464d0, 0.399d0, 0.355d0, 0.323d0 /
!
! a/c = 1/2 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(2,1,k),k=1,5) / 0.642d0, 0.642d0, 0.642d0, 0.642d0, 0.642d0 /
    data (coef(2,2,k),k=1,5) / 0.643d0, 0.626d0, 0.610d0, 0.594d0, 0.579d0 /
    data (coef(2,3,k),k=1,5) / 0.645d0, 0.613d0, 0.585d0, 0.559d0, 0.536d0 /
    data (coef(2,4,k),k=1,5) / 0.647d0, 0.594d0, 0.550d0, 0.513d0, 0.481d0 /
    data (coef(2,5,k),k=1,5) / 0.653d0, 0.572d0, 0.511d0, 0.464d0, 0.426d0 /
    data (coef(2,6,k),k=1,5) / 0.660d0, 0.560d0, 0.490d0, 0.439d0, 0.400d0 /
    data (coef(2,7,k),k=1,5) / 0.666d0, 0.553d0, 0.478d0, 0.425d0, 0.385d0 /
    data (coef(2,8,k),k=1,5) / 0.677d0, 0.545d0, 0.463d0, 0.408d0, 0.368d0 /
    data (coef(2,9,k),k=1,5) / 0.686d0, 0.541d0, 0.456d0, 0.399d0, 0.359d0 /
!
! a/c = 1/4 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(3,1,k),k=1,5) / 0.677d0, 0.677d0, 0.677d0, 0.677d0, 0.677d0 /
    data (coef(3,2,k),k=1,5) / 0.680d0, 0.661d0, 0.643d0, 0.626d0, 0.610d0 /
    data (coef(3,3,k),k=1,5) / 0.682d0, 0.648d0, 0.617d0, 0.590d0, 0.564d0 /
    data (coef(3,4,k),k=1,5) / 0.687d0, 0.629d0, 0.581d0, 0.540d0, 0.505d0 /
    data (coef(3,5,k),k=1,5) / 0.696d0, 0.607d0, 0.540d0, 0.488d0, 0.448d0 /
    data (coef(3,6,k),k=1,5) / 0.706d0, 0.595d0, 0.518d0, 0.462d0, 0.420d0 /
    data (coef(3,7,k),k=1,5) / 0.714d0, 0.588d0, 0.505d0, 0.447d0, 0.404d0 /
    data (coef(3,8,k),k=1,5) / 0.729d0, 0.581d0, 0.491d0, 0.430d0, 0.386d0 /
    data (coef(3,9,k),k=1,5) / 0.741d0, 0.578d0, 0.483d0, 0.421d0, 0.377d0 /
!
! a/c = 1/8 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(4,1,k),k=1,5) / 0.689d0, 0.689d0, 0.689d0, 0.689d0, 0.689d0 /
    data (coef(4,2,k),k=1,5) / 0.692d0, 0.673d0, 0.654d0, 0.637d0, 0.621d0 /
    data (coef(4,3,k),k=1,5) / 0.696d0, 0.660d0, 0.629d0, 0.600d0, 0.574d0 /
    data (coef(4,4,k),k=1,5) / 0.701d0, 0.642d0, 0.592d0, 0.550d0, 0.515d0 /
    data (coef(4,5,k),k=1,5) / 0.713d0, 0.620d0, 0.551d0, 0.498d0, 0.456d0 /
    data (coef(4,6,k),k=1,5) / 0.723d0, 0.609d0, 0.529d0, 0.471d0, 0.427d0 /
    data (coef(4,7,k),k=1,5) / 0.732d0, 0.602d0, 0.516d0, 0.456d0, 0.411d0 /
    data (coef(4,8,k),k=1,5) / 0.749d0, 0.596d0, 0.502d0, 0.439d0, 0.393d0 /
    data (coef(4,9,k),k=1,5) / 0.764d0, 0.594d0, 0.495d0, 0.430d0, 0.384d0 /
!
! a/c = 1/16 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(5,1,k),k=1,5) / 0.693d0, 0.693d0, 0.693d0, 0.693d0, 0.693d0 /
    data (coef(5,2,k),k=1,5) / 0.696d0, 0.677d0, 0.658d0, 0.641d0, 0.624d0 /
    data (coef(5,3,k),k=1,5) / 0.700d0, 0.665d0, 0.633d0, 0.604d0, 0.578d0 /
    data (coef(5,4,k),k=1,5) / 0.706d0, 0.646d0, 0.596d0, 0.554d0, 0.518d0 /
    data (coef(5,5,k),k=1,5) / 0.718d0, 0.625d0, 0.555d0, 0.501d0, 0.459d0 /
    data (coef(5,6,k),k=1,5) / 0.729d0, 0.613d0, 0.533d0, 0.475d0, 0.430d0 /
    data (coef(5,7,k),k=1,5) / 0.739d0, 0.607d0, 0.520d0, 0.459d0, 0.414d0 /
    data (coef(5,8,k),k=1,5) / 0.757d0, 0.601d0, 0.506d0, 0.442d0, 0.396d0 /
    data (coef(5,9,k),k=1,5) / 0.774d0, 0.600d0, 0.499d0, 0.434d0, 0.387d0 /
!
! a/c = 0. ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(6,1,k),k=1,5) / 0.680d0, 0.680d0, 0.680d0, 0.680d0, 0.680d0 /
    data (coef(6,2,k),k=1,5) / 0.681d0, 0.661d0, 0.643d0, 0.625d0, 0.608d0 /
    data (coef(6,3,k),k=1,5) / 0.683d0, 0.647d0, 0.615d0, 0.586d0, 0.559d0 /
    data (coef(6,4,k),k=1,5) / 0.687d0, 0.627d0, 0.576d0, 0.534d0, 0.498d0 /
    data (coef(6,5,k),k=1,5) / 0.699d0, 0.606d0, 0.536d0, 0.482d0, 0.439d0 /
    data (coef(6,6,k),k=1,5) / 0.710d0, 0.594d0, 0.513d0, 0.455d0, 0.410d0 /
    data (coef(6,7,k),k=1,5) / 0.720d0, 0.588d0, 0.501d0, 0.439d0, 0.394d0 /
    data (coef(6,8,k),k=1,5) / 0.741d0, 0.583d0, 0.487d0, 0.423d0, 0.377d0 /
    data (coef(6,9,k),k=1,5) / 0.760d0, 0.584d0, 0.482d0, 0.415d0, 0.369d0 /
!
    v11 = coef(ix1,iy1,i)
    v21 = coef(ix2,iy1,i)
    v12 = coef(ix1,iy2,i)
    v22 = coef(ix2,iy2,i)
!
end subroutine
