subroutine coef_infl_c2(ix1,ix2,iy1,iy2,i,&
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
! ======================================================================
!-----------------------------------------------------------------------
!     operateur POST_K_BETA
!     coefficients d'influence au point C pour Er/E=0.7
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
!                      points A et C
!
! Elles correspondent a celles du point C pour Er/E =0.7
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
    integer :: k
    real(kind=8) :: coef(6,9,5)
    real(kind=8) :: asc(6)
    real(kind=8) :: asr(9)
!-----------------------------------------------------------------------
!          RSE-M - Edition 2010
!               ANNEXE 5.4
!   METHODES ANALYTIQUES DE CALCUL DES FACTEURS
!   D'INTENSITE DE CONTRAINTE ET DE L?INTEGRALE J
!-----------------------------------------------------------------------
!
! Tableau VII.5.3.2a : PLA-DSR - coefficients d'influence pour Er/E = 0.7 : points A et C
!
    data (asc(k),k=1,6) / 1.d0, 0.5d0,   0.25d0, 0.125d0, 0.0625d0, 0.d0/
    data (asr(k),k=1,9) / 0.d0, 0.125d0, 0.25d0, 0.5d0,  1.d0, 1.5d0, 2.d0, 3.d0, 4.d0 /
!                          ----------------------
!                         | Point A -- Er/E=0.7 |
!                          ----------------------
!
! a/c = 1 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(1,1,k),k=1,5) / 0.284d0, 0.284d0, 0.284d0, 0.284d0, 0.284d0 /
    data (coef(1,2,k),k=1,5) / 0.284d0, 0.256d0, 0.231d0, 0.208d0, 0.187d0 /
    data (coef(1,3,k),k=1,5) / 0.284d0, 0.233d0, 0.192d0, 0.158d0, 0.131d0 /
    data (coef(1,4,k),k=1,5) / 0.285d0, 0.200d0, 0.141d0, 0.101d0, 7.20d-2 /
    data (coef(1,6,k),k=1,5) / 0.290d0, 0.136d0, 6.61d-2, 3.39d-2, 1.86d-2 /
    data (coef(1,7,k),k=1,5) / 0.293d0, 0.120d0, 5.25d-2, 2.51d-2, 1.33d-2 /
    data (coef(1,8,k),k=1,5) / 0.298d0, 0.101d0, 3.87d-2, 1.74d-2, 9.26d-3 /
    data (coef(1,9,k),k=1,5) / 0.302d0, 8.99d-2, 3.20d-2, 1.43d-2, 7.79d-3 /

!
! a/c = 1/2 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(2,1,k),k=1,5) / 0.262d0, 0.262d0, 0.262d0, 0.262d0, 0.262d0 /
    data (coef(2,2,k),k=1,5) / 0.262d0, 0.236d0, 0.212d0, 0.191d0, 0.171d0 /
    data (coef(2,3,k),k=1,5) / 0.263d0, 0.215d0, 0.176d0, 0.144d0, 0.118d0 /
    data (coef(2,4,k),k=1,5) / 0.264d0, 0.184d0, 0.129d0, 9.04d-2, 6.39d-2 /
    data (coef(2,5,k),k=1,5) / 0.266d0, 0.146d0, 8.09d-2, 4.58d-2, 2.66d-2 /
    data (coef(2,6,k),k=1,5) / 0.269d0, 0.123d0, 5.82d-2, 2.87d-2, 1.50d-2 /
    data (coef(2,7,k),k=1,5) / 0.271d0, 0.108d0, 4.55d-2, 2.06d-2, 1.03d-2 /
    data (coef(2,8,k),k=1,5) / 0.276d0, 0.090d0, 3.26d-2, 1.36d-2, 6.73d-3 /
    data (coef(2,9,k),k=1,5) / 0.280d0, 0.079d0, 2.64d-2, 1.08d-2, 5.48d-3 /

!
! a/c = 1/4 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(3,1,k),k=1,5) / 0.229d0, 0.229d0, 0.229d0, 0.229d0, 0.229d0 /
    data (coef(3,2,k),k=1,5) / 0.230d0, 0.206d0, 0.184d0, 0.165d0, 0.148d0 /
    data (coef(3,3,k),k=1,5) / 0.230d0, 0.187d0, 0.152d0, 0.124d0, 0.101d0 /
    data (coef(3,4,k),k=1,5) / 0.231d0, 0.159d0, 0.110d0, 7.61d-2, 5.28d-2 /
    data (coef(3,5,k),k=1,5) / 0.233d0, 0.125d0, 6.72d-2, 3.66d-2, 2.03d-2 /
    data (coef(3,6,k),k=1,5) / 0.235d0, 1.04d-1, 4.68d-2, 2.17d-2, 1.04d-2 /
    data (coef(3,7,k),k=1,5) / 0.236d0, 9.00d-2, 3.55d-2, 1.47d-2, 6.51d-3 /
    data (coef(3,8,k),k=1,5) / 0.239d0, 7.32d-2, 2.40d-2, 8.71d-3, 3.66d-3 /
    data (coef(3,9,k),k=1,5) / 0.242d0, 6.33d-2, 1.85d-2, 6.39d-3, 2.74d-3 /

!
! a/c = 1/8 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(4,1,k),k=1,5) / 0.195d0, 0.195d0, 0.195d0, 0.195d0, 0.195d0 /
    data (coef(4,2,k),k=1,5) / 0.195d0, 0.174d0, 0.155d0, 0.139d0, 0.124d0 /
    data (coef(4,3,k),k=1,5) / 0.195d0, 0.158d0, 0.128d0, 0.103d0, 8.37d-2 /       
    data (coef(4,4,k),k=1,5) / 0.195d0, 0.133d0, 9.10d-2, 6.22d-2, 4.26d-2 /   
    data (coef(4,5,k),k=1,5) / 0.196d0, 0.103d0, 5.41d-2, 2.86d-2, 1.52d-2 /   
    data (coef(4,6,k),k=1,5) / 0.197d0, 8.47d-2, 3.67d-2, 1.61d-2, 7.19d-3 / 
    data (coef(4,7,k),k=1,5) / 0.198d0, 7.26d-2, 2.70d-2, 1.03d-2, 4.09d-3 / 
    data (coef(4,8,k),k=1,5) / 0.200d0, 5.77d-2, 1.73d-2, 5.50d-3, 1.94d-3 / 
    data (coef(4,9,k),k=1,5) / 0.202d0, 4.90d-2, 1.27d-2, 3.69d-3, 1.30d-3 / 

!
! a/c = 1/16 ; a/r = 0 1/8 1/4 1/2 1 3/2 2 3 4 ; i0 i1 i2 i3 i4
! 
    data (coef(5,1,k),k=1,5) / 0.163d0, 0.163d0, 0.163d0, 0.163d0, 0.163d0 / 
    data (coef(5,2,k),k=1,5) / 0.163d0, 0.146d0, 0.130d0, 0.116d0, 0.104d0 / 
    data (coef(5,3,k),k=1,5) / 0.163d0, 0.132d0, 0.106d0, 8.57d-2, 6.92d-2 / 
    data (coef(5,4,k),k=1,5) / 0.163d0, 0.111d0, 7.51d-2, 5.10d-2, 3.46d-2 / 
    data (coef(5,5,k),k=1,5) / 0.164d0, 8.48d-2, 4.39d-2, 2.28d-2, 1.19d-2 / 
    data (coef(5,6,k),k=1,5) / 0.165d0, 6.93d-2, 2.93d-2, 1.24d-2, 5.32d-3 / 
    data (coef(5,7,k),k=1,5) / 0.165d0, 5.89d-2, 2.12d-2, 7.70d-3, 2.85d-3 / 
    data (coef(5,8,k),k=1,5) / 0.166d0, 4.61d-2, 1.30d-2, 3.82d-3, 1.19d-3 / 
    data (coef(5,9,k),k=1,5) / 0.167d0, 3.86d-2, 9.23d-3, 2.39d-3, 7.23d-4 / 

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


end subroutine
