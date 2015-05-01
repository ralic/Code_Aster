subroutine coef_infl_b2(ix1,ix2,iy1,iy2,i,&
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
!
! ======================================================================
! --- BUT : CALCUL DES COEFFICIENTS D INFLUENCE AU POINT A POUR Er/E=0.7
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
!     coefficients d'influence au point B pour Er/E=0.7
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
! Tableau VII.5.3.2c : PLA-DSR - coefficients d'influence pour Er/E = 0.7
!                      points A (B pour EDF) et C
!
! Elles correspondent a celles du point A (B pour EDF) pour Er/E =0.7
!
!-----------------------------------------------------------------------
!
!    defaut semi-elliptique (notation EDF)
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
    integer :: k
    real(kind=8) :: coef(6,9,5)
!                   x1 <=> xi ; x2 <=> xi+1
!                   y1 <=> yi ; y2 <=> yi+1
!                   z1 <=> zi ; z2 <=> zi+1
!
! a/c = 1 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(1,1,k),k=1,5) / 0.558d0, 0.558d0, 0.558d0, 0.558d0, 0.558d0 /
    data (coef(1,2,k),k=1,5) / 0.558d0, 0.544d0, 0.531d0, 0.518d0, 0.506d0 /
    data (coef(1,3,k),k=1,5) / 0.559d0, 0.533d0, 0.511d0, 0.490d0, 0.471d0 /
    data (coef(1,4,k),k=1,5) / 0.560d0, 0.517d0, 0.482d0, 0.452d0, 0.426d0 /
    data (coef(1,5,k),k=1,5) / 0.562d0, 0.498d0, 0.449d0, 0.411d0, 0.381d0 /
    data (coef(1,6,k),k=1,5) / 0.565d0, 0.487d0, 0.432d0, 0.391d0, 0.359d0 /
    data (coef(1,7,k),k=1,5) / 0.568d0, 0.480d0, 0.421d0, 0.378d0, 0.346d0 /
    data (coef(1,8,k),k=1,5) / 0.573d0, 0.472d0, 0.408d0, 0.364d0, 0.332d0 /
    data (coef(1,9,k),k=1,5) / 0.578d0, 0.468d0, 0.401d0, 0.357d0, 0.324d0 /
!
! a/c = 1/2 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(2,1,k),k=1,5) / 0.657d0, 0.657d0, 0.657d0, 0.657d0, 0.657d0 /
    data (coef(2,2,k),k=1,5) / 0.658d0, 0.640d0, 0.622d0, 0.606d0, 0.590d0 / 
    data (coef(2,3,k),k=1,5) / 0.660d0, 0.626d0, 0.596d0, 0.569d0, 0.545d0 / 
    data (coef(2,4,k),k=1,5) / 0.662d0, 0.606d0, 0.560d0, 0.521d0, 0.487d0 /
    data (coef(2,5,k),k=1,5) / 0.669d0, 0.583d0, 0.519d0, 0.470d0, 0.431d0 / 
    data (coef(2,6,k),k=1,5) / 0.675d0, 0.570d0, 0.497d0, 0.444d0, 0.403d0 /
    data (coef(2,7,k),k=1,5) / 0.681d0, 0.562d0, 0.484d0, 0.428d0, 0.388d0 / 
    data (coef(2,8,k),k=1,5) / 0.692d0, 0.553d0, 0.468d0, 0.411d0, 0.370d0 / 
    data (coef(2,9,k),k=1,5) / 0.701d0, 0.549d0, 0.460d0, 0.402d0, 0.361d0 / 
!
! a/c = 1/4 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(3,1,k),k=1,5) / 0.695d0, 0.695d0, 0.695d0, 0.695d0, 0.695d0 / 
    data (coef(3,2,k),k=1,5) / 0.698d0, 0.678d0, 0.659d0, 0.642d0, 0.625d0 /
    data (coef(3,3,k),k=1,5) / 0.701d0, 0.665d0, 0.632d0, 0.603d0, 0.576d0 / 
    data (coef(3,4,k),k=1,5) / 0.706d0, 0.645d0, 0.593d0, 0.550d0, 0.514d0 / 
    data (coef(3,5,k),k=1,5) / 0.716d0, 0.621d0, 0.550d0, 0.496d0, 0.453d0 / 
    data (coef(3,6,k),k=1,5) / 0.726d0, 0.608d0, 0.527d0, 0.469d0, 0.424d0 / 
    data (coef(3,7,k),k=1,5) / 0.734d0, 0.601d0, 0.513d0, 0.452d0, 0.408d0 /
    data (coef(3,8,k),k=1,5) / 0.749d0, 0.592d0, 0.497d0, 0.434d0, 0.389d0 / 
    data (coef(3,9,k),k=1,5) / 0.761d0, 0.589d0, 0.489d0, 0.425d0, 0.380d0 / 
!
! a/c = 1/8 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(4,1,k),k=1,5) / 0.709d0, 0.709d0, 0.709d0, 0.709d0, 0.709d0 / 
    data (coef(4,2,k),k=1,5) / 0.712d0, 0.691d0, 0.672d0, 0.654d0, 0.636d0 / 
    data (coef(4,3,k),k=1,5) / 0.716d0, 0.679d0, 0.645d0, 0.615d0, 0.587d0 / 
    data (coef(4,4,k),k=1,5) / 0.723d0, 0.659d0, 0.606d0, 0.562d0, 0.524d0 / 
    data (coef(4,5,k),k=1,5) / 0.734d0, 0.636d0, 0.562d0, 0.506d0, 0.462d0 / 
    data (coef(4,6,k),k=1,5) / 0.745d0, 0.623d0, 0.539d0, 0.478d0, 0.433d0 / 
    data (coef(4,7,k),k=1,5) / 0.755d0, 0.616d0, 0.525d0, 0.462d0, 0.416d0 / 
    data (coef(4,8,k),k=1,5) / 0.772d0, 0.608d0, 0.510d0, 0.444d0, 0.397d0 / 
    data (coef(4,9,k),k=1,5) / 0.786d0, 0.605d0, 0.502d0, 0.435d0, 0.388d0 / 
!
! a/c = 1/16 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(5,1,k),k=1,5) / 0.713d0, 0.713d0, 0.713d0, 0.713d0, 0.713d0 / 
    data (coef(5,2,k),k=1,5) / 0.717d0, 0.696d0, 0.676d0, 0.658d0, 0.640d0 / 
    data (coef(5,3,k),k=1,5) / 0.721d0, 0.684d0, 0.650d0, 0.619d0, 0.591d0 / 
    data (coef(5,4,k),k=1,5) / 0.728d0, 0.664d0, 0.611d0, 0.566d0, 0.528d0 / 
    data (coef(5,5,k),k=1,5) / 0.741d0, 0.641d0, 0.567d0, 0.510d0, 0.466d0 / 
    data (coef(5,6,k),k=1,5) / 0.752d0, 0.628d0, 0.543d0, 0.482d0, 0.436d0 / 
    data (coef(5,7,k),k=1,5) / 0.762d0, 0.621d0, 0.529d0, 0.466d0, 0.419d0 / 
    data (coef(5,8,k),k=1,5) / 0.781d0, 0.615d0, 0.514d0, 0.448d0, 0.400d0 / 
    data (coef(5,9,k),k=1,5) / 0.797d0, 0.613d0, 0.507d0, 0.439d0, 0.391d0 / 
!
!---------------------------------------
! a/c =  0. 
! a/r =  0. 1/8 1/4 1/2 1 3/2 2 3 4 
! Valeurs de i0 i1 i2 i3 i4
!--------------------------------------- 
!
    data (coef(6,1,k),k=1,5) / 0.702d0, 0.702d0, 0.702d0, 0.702d0, 0.702d0 /
    data (coef(6,2,k),k=1,5) / 0.703d0, 0.682d0, 0.662d0, 0.643d0, 0.625d0 /
    data (coef(6,3,k),k=1,5) / 0.705d0, 0.666d0, 0.632d0, 0.601d0, 0.573d0 /
    data (coef(6,4,k),k=1,5) / 0.709d0, 0.645d0, 0.591d0, 0.546d0, 0.508d0 /
    data (coef(6,5,k),k=1,5) / 0.722d0, 0.622d0, 0.548d0, 0.491d0, 0.446d0 /
    data (coef(6,6,k),k=1,5) / 0.733d0, 0.609d0, 0.524d0, 0.463d0, 0.416d0 /
    data (coef(6,7,k),k=1,5) / 0.744d0, 0.603d0, 0.510d0, 0.446d0, 0.399d0 /
    data (coef(6,8,k),k=1,5) / 0.766d0, 0.597d0, 0.496d0, 0.429d0, 0.381d0 /
    data (coef(6,9,k),k=1,5) / 0.785d0, 0.597d0, 0.490d0, 0.421d0, 0.373d0 /
!
    v11 = coef(ix1,iy1,i)
    v21 = coef(ix2,iy1,i)
    v12 = coef(ix1,iy2,i)
    v22 = coef(ix2,iy2,i)


end subroutine
