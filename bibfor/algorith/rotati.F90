subroutine rotati(euler, rot)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!    M. CORUS     DATE 03/02/10
!-----------------------------------------------------------------------
!  BUT:      < CALCUL DE MATRICE DE ROTATION >
!
!  CONSTRUIRE LA MATRICE DE ROTATION COMPLETE A PARTIR DES ANGLES
!  NAUTIQUES
!
!   NB : Equations de passage : un vecteur de coordonnees initiales
!       (X,Y,Z) a pour image le vecteur (X1,Y1,Z1), tel que
!    _                  _    _                  _
!   | 1    0      0     |   | cos(B)  0  sin(B) |
!   | 0  cos(G) -sin(G) | x |   0     1   0     | x ...
!   |_0  sin(G)  cos(G)_|   |_-sin(B) 0 cos(B) _|
!
!   _                   _    _  _    _   _
!  | cos(A) -sin(A)  0  |   | X |   | X1 |
!  | sin(A)  cos(A)  0  | x | Y | = | Y1 |
!  |_ 0       0      1 _|   |_Z_|   |_Z1_|
!
!   A (alpha), B(beta), gamma (G) sont les angle nautiques
!
!  ON CONSTRUIT ROT TELLE QUE :
!
!   _    _ _  _    _   _
!  |     || X |   | X1 |
!  | ROT || Y | = | Y1 |
!  |_   _||_Z_|   |_Z1_|
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! EULER    /I/: VECTEUR CONTENANT LES 3 ANGLES NAUTIQUES
! ROT      /I/: MATRICE 3x3 DE LA ROTATION
!
!-- VARIABLES EN ENTREES / SORTIE
    real(kind=8) :: euler(3), rota(3, 3)
!
!-- VARIABLES DE LA ROUTINE
    real(kind=8) :: rotb(3, 3), rotc(3, 3), rotd(3, 3), rot(3, 3)
    integer :: i1, j1, k1
!
    rota(1,1)=cos(euler(1))
    rota(1,2)=-sin(euler(1))
    rota(1,3)=0.d0
    rota(2,1)=-rota(1,2)
    rota(2,2)=rota(1,1)
    rota(2,3)=0.d0
    rota(3,1)=0.d0
    rota(3,2)=0.d0
    rota(3,3)=1.d0
!
    rotb(1,1)=cos(euler(2))
    rotb(1,2)=0.d0
    rotb(1,3)=sin(euler(2))
    rotb(2,1)=0.d0
    rotb(2,2)=1.d0
    rotb(2,3)=0.d0
    rotb(3,1)=-rotb(1,3)
    rotb(3,2)=0.d0
    rotb(3,3)=rotb(1,1)
!
    rotc(1,1)=1.d0
    rotc(1,2)=0.d0
    rotc(1,3)=0.d0
    rotc(2,1)=0.d0
    rotc(2,2)=cos(euler(3))
    rotc(2,3)=-sin(euler(3))
    rotc(3,1)=0.d0
    rotc(3,2)= -rotc(2,3)
    rotc(3,3)=rotc(2,2)
!
    rotd(1,1)=0.d0
    rotd(1,2)=0.d0
    rotd(1,3)=0.d0
    rotd(2,1)=0.d0
    rotd(2,2)=0.d0
    rotd(2,3)=0.d0
    rotd(3,1)=0.d0
    rotd(3,2)=0.d0
    rotd(3,3)=0.d0
!
    rot(1,1)=0.d0
    rot(1,2)=0.d0
    rot(1,3)=0.d0
    rot(2,1)=0.d0
    rot(2,2)=0.d0
    rot(2,3)=0.d0
    rot(3,1)=0.d0
    rot(3,2)=0.d0
    rot(3,3)=0.d0
!
    do 20 j1 = 1, 3
        do 30 i1 = 1, 3
            do 40 k1 = 1, 3
                rotd(i1,j1)=rotd(i1,j1)+rotb(i1,k1)*rota(k1,j1)
40          continue
30      continue
20  end do
!
    do 50 j1 = 1, 3
        do 60 i1 = 1, 3
            do 70 k1 = 1, 3
                rot(i1,j1)=rot(i1,j1)+rotc(i1,k1)*rotd(k1,j1)
70          continue
60      continue
50  end do
!
end subroutine
