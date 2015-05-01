subroutine coef_infl_c1(ix1,ix2,iy1,iy2,i,&
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
! --- BUT : CALCUL DES COEFFICIENTS D INFLUENCE AU POINT C POUR Er/E=0.7
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
!     coefficients d'influence au point C pour Er/E=1
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
!                      points A et C
!
! Elles correspondent a celles du point C pour Er/E = 1
!
!-----------------------------------------------------------------------
!
!    defaut semi-elliptique
!
!       ----------2c---------
!       $$$$$$$$$$$$$$$$$$$$$   <- revetement 
!    ^  ----------B--------C-
!         \               /
!    a     \             /      <- metal de base
!           \           /
!    v        ----A----
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
    data (coef(1,1,k),k=1,5) / 0.263d0, 0.263d0, 0.263d0, 0.263d0, 0.263d0 /
    data (coef(1,2,k),k=1,5) / 0.264d0, 0.238d0, 0.214d0, 0.193d0, 0.174d0 /
    data (coef(1,3,k),k=1,5) / 0.264d0, 0.217d0, 0.178d0, 0.147d0, 0.121d0 /
    data (coef(1,4,k),k=1,5) / 0.265d0, 0.186d0, 0.131d0, 9.27d-2, 6.61d-2 /
    data (coef(1,5,k),k=1,5) / 0.267d0, 0.148d0, 8.32d-2, 4.80d-2, 2.85d-2 /
    data (coef(1,6,k),k=1,5) / 0.270d0, 0.125d0, 6.05d-2, 3.08d-2, 1.67d-2 /
    data (coef(1,7,k),k=1,5) / 0.272d0, 0.111d0, 4.79d-2, 2.26d-2, 1.19d-2 /
    data (coef(1,8,k),k=1,5) / 0.277d0, 0.093d0, 3.51d-2, 1.56d-2, 8.21d-3 /
    data (coef(1,9,k),k=1,5) / 0.281d0, 0.083d0, 2.90d-2, 1.27d-2, 6.91d-3 /
!
! a/c = 1/2 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(2,1,k),k=1,5) / 0.244d0, 0.244d0, 0.244d0, 0.244d0, 0.244d0 /
    data (coef(2,2,k),k=1,5) / 0.245d0, 0.220d0, 0.197d0, 0.177d0, 0.160d0 /
    data (coef(2,3,k),k=1,5) / 0.245d0, 0.200d0, 0.164d0, 0.134d0, 0.110d0 /
    data (coef(2,4,k),k=1,5) / 0.246d0, 0.171d0, 0.119d0, 8.36d-2, 5.89d-2 /
    data (coef(2,5,k),k=1,5) / 0.248d0, 0.135d0, 7.47d-2, 4.20d-2, 2.42d-2 /
    data (coef(2,6,k),k=1,5) / 0.251d0, 0.114d0, 5.34d-2, 2.61d-2, 1.35d-2 /
    data (coef(2,7,k),k=1,5) / 0.253d0, 0.100d0, 4.16d-2, 1.86d-2, 9.16d-3 /
    data (coef(2,8,k),k=1,5) / 0.257d0, 0.083d0, 2.95d-2, 1.21d-2, 5.91d-3 /
    data (coef(2,9,k),k=1,5) / 0.261d0, 0.073d0, 2.38d-2, 9.58d-3, 4.80d-3 /

!
! a/c = 1/4 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(3,1,k),k=1,5) / 0.215d0, 0.215d0, 0.215d0, 0.215d0, 0.215d0 /
    data (coef(3,2,k),k=1,5) / 0.216d0, 0.193d0, 0.173d0, 0.155d0, 0.139d0 /
    data (coef(3,3,k),k=1,5) / 0.216d0, 0.176d0, 0.143d0, 0.116d0, 9.45d-2 /
    data (coef(3,4,k),k=1,5) / 0.217d0, 0.149d0, 0.103d0, 7.11d-2, 4.92d-2 /
    data (coef(3,5,k),k=1,5) / 0.219d0, 0.117d0, 6.26d-2, 3.40d-2, 1.87d-2 /
    data (coef(3,6,k),k=1,5) / 0.220d0, 9.70d-2, 4.34d-2, 2.00d-2, 9.50d-3 /
    data (coef(3,7,k),k=1,5) / 0.222d0, 8.40d-2, 3.28d-2, 1.34d-2, 5.87d-3 /
    data (coef(3,8,k),k=1,5) / 0.224d0, 6.80d-2, 2.20d-2, 7.86d-3, 3.24d-3 /
    data (coef(3,9,k),k=1,5) / 0.227d0, 5.87d-2, 1.69d-2, 5.71d-3, 2.40d-3 /
!
! a/c = 1/8 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(4,1,k),k=1,5) / 0.185d0, 0.185d0, 0.185d0, 0.185d0, 0.185d0 /
    data (coef(4,2,k),k=1,5) / 0.185d0, 0.165d0, 0.147d0, 0.132d0, 0.118d0 /
    data (coef(4,3,k),k=1,5) / 0.185d0, 0.150d0, 0.121d0, 9.79d-2, 7.92d-2 /
    data (coef(4,4,k),k=1,5) / 0.185d0, 0.126d0, 8.61d-2, 5.88d-2, 4.02d-2 /
    data (coef(4,5,k),k=1,5) / 0.186d0, 9.74d-2, 5.11d-2, 2.70d-2, 1.43d-2 /
    data (coef(4,6,k),k=1,5) / 0.187d0, 8.01d-2, 3.46d-2, 1.51d-2, 6.70d-3 /
    data (coef(4,7,k),k=1,5) / 0.188d0, 6.86d-2, 2.54d-2, 9.63d-3, 3.78d-3 /
    data (coef(4,8,k),k=1,5) / 0.190d0, 5.44d-2, 1.61d-2, 5.08d-3, 1.76d-3 /
    data (coef(4,9,k),k=1,5) / 0.191d0, 4.61d-2, 1.18d-2, 3.37d-3, 1.16d-3 /
!
! a/c = 1/16 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(5,1,k),k=1,5) / 0.156d0, 0.156d0, 0.156d0, 0.156d0, 0.156d0 /
    data (coef(5,2,k),k=1,5) / 0.156d0, 0.139d0, 0.124d0, 0.111d0, 9.91d-2 /
    data (coef(5,3,k),k=1,5) / 0.156d0, 0.126d0, 0.102d0, 8.20d-2, 6.62d-2 /
    data (coef(5,4,k),k=1,5) / 0.156d0, 0.106d0, 7.19d-2, 4.87d-2, 3.31d-2 /
    data (coef(5,5,k),k=1,5) / 0.157d0, 8.11d-2, 4.20d-2, 2.18d-2, 1.13d-2 /
    data (coef(5,6,k),k=1,5) / 0.158d0, 6.62d-2, 2.79d-2, 1.18d-2, 5.05d-3 /
    data (coef(5,7,k),k=1,5) / 0.158d0, 5.63d-2, 2.02d-2, 7.31d-3, 2.70d-3 /
    data (coef(5,8,k),k=1,5) / 0.159d0, 4.40d-2, 1.24d-2, 3.60d-3, 1.11d-3 /
    data (coef(5,9,k),k=1,5) / 0.160d0, 3.68d-2, 8.74d-3, 2.23d-3, 6.65d-4 /
!
! a/c = 0. ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
! ATTENTION comme le tableau de l'annexe 5.4 ne contient aucune valeur, 
! on a mis -9999.D0 comme valeur 

    data (coef(6,1,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,2,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,3,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,4,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,5,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,6,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,7,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,8,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
    data (coef(6,9,k),k=1,5) / -9999.d0, -9999.d0, -9999.d0, -9999.d0, -9999.d0 /
!
    v11 = coef(ix1,iy1,i)
    v21 = coef(ix2,iy1,i)
    v12 = coef(ix1,iy2,i)
    v22 = coef(ix2,iy2,i)
!
end subroutine
