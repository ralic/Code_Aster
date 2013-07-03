subroutine matint(kr, mr, direc, vtest, rayon)
!
    implicit none
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
!-----------------------------------------------------------------------
!    M. CORUS     DATE 11/03/10
!-----------------------------------------------------------------------
!  BUT:      < CONSTRUIRE LES MATRICES ELEMENTAIRES D'INTERFACE >
!
!  ON CONSTRUIT LES MATRICES ELEMENTAIRES DE RAIDEUR ET DE MASSE
!   D'UNE POUTRE DROITE D'EULER BERNOULLI
!
!-----------------------------------------------------------------------
!  KR        /O/ : MATRICE DE RAIDEUR
!  MR        /O/ : MATRICE DE MASSE
!  DIREC     /I/ : VECTEUR DIRECTEUR DE LA POUTRE
!  VTEST     /I/ : VECTEUR DEFINISSANT L'ORIENTATION DE LA POUTRE
!  RAYON     /I/ : RAYON DE LA POUTRE
!
!     ------------------------------------------------------------------
!
!-- VARIABLES EN ENTREES / SORTIE
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    real(kind=8) :: kr(12, 12), mr(12, 12), direc(3), vtest(3), rayon
!
!-- VARIABLES DE LA ROUTINE
    integer :: i1, j1, k1, l1
    real(kind=8) :: e, i, s, g, rho, pi, l, t(12, 12), v1(3), v2(3), v3(3), temp
    real(kind=8) :: kini(12, 12), mini(12, 12), fact
    parameter    (pi=3.141592653589793238462643d0)
!
    fact=(20*rayon/5.d-2)
!
    e=2.1d11/(fact**2)
    g=0.8d11/(fact**2)
    rho=7.8d3/(fact**4)
!
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
    do 10 i1 = 1, 12
        do 20 j1 = 1, 12
            t(i1,j1)=0.d0
            kr(i1,j1)=0.d0
            mr(i1,j1)=0.d0
            kini(i1,j1)=0.d0
            mini(i1,j1)=0.d0
20      continue
10  end do
!
    s=pi*(rayon**2)
    i=pi*(rayon**4)/2
    l=sqrt(direc(1)**2+direc(2)**2+direc(3)**2)
!
    v1(1)=direc(1)/l
    v1(2)=direc(2)/l
    v1(3)=direc(3)/l
!
    temp=v1(1)*vtest(1)+v1(2)*vtest(2)+v1(3)*vtest(3)
!
    v2(1)=vtest(1)-v1(1)*temp
    v2(2)=vtest(2)-v1(2)*temp
    v2(3)=vtest(3)-v1(3)*temp
!
    temp=sqrt(v2(1)**2+v2(2)**2+v2(3)**2)
!
    v2(1)=v2(1)/temp
    v2(2)=v2(2)/temp
    v2(3)=v2(3)/temp
!
    v3(1)=v1(2)*v2(3)-v1(3)*v2(2)
    v3(2)=v1(3)*v2(1)-v1(1)*v2(3)
    v3(3)=v1(1)*v2(2)-v1(2)*v2(1)
!
    do 30 i1 = 1, 4
!
        t(1+(i1-1)*3,1+(i1-1)*3)=v1(1)
        t(2+(i1-1)*3,1+(i1-1)*3)=v1(2)
        t(3+(i1-1)*3,1+(i1-1)*3)=v1(3)
!
        t(1+(i1-1)*3,2+(i1-1)*3)=v2(1)
        t(2+(i1-1)*3,2+(i1-1)*3)=v2(2)
        t(3+(i1-1)*3,2+(i1-1)*3)=v2(3)
!
        t(1+(i1-1)*3,3+(i1-1)*3)=v3(1)
        t(2+(i1-1)*3,3+(i1-1)*3)=v3(2)
        t(3+(i1-1)*3,3+(i1-1)*3)=v3(3)
!
30  end do
!
!-- poutre d'axe +X, DDL :
!-- X1 Y1 Z1 RX1 RY1 RZ1 X2 Y2 Z2 RX2 RY2 RZ2
!
!------------------------C
!--                    --C
!-- MATRICE DE RAIDEUR --C
!--                    --C
!------------------------C
!
    kini(1,1)=e*s/l
    kini(1,7)=-e*s/l
!
    kini(2,2)=12.d0*e*i/(l**3)
    kini(2,6)= 6.d0*e*i/(l**2)
    kini(2,8)= -12.d0*e*i/(l**3)
    kini(2,12)= 6.d0*e*i/(l**2)
!
    kini(3,3)=12.d0*e*i/(l**3)
    kini(3,5)=-6.d0*e*i/(l**2)
    kini(3,9)=-12.d0*e*i/(l**3)
    kini(3,11)=-6.d0*e*i/(l**2)
!
    kini(4,4)=g*i/l
    kini(4,10)=-g*i/l
!
    kini(5,3)=-6.d0*e*i/(l**2)
    kini(5,5)=4.d0*e*i/l
    kini(5,9)=6.d0*e*i/(l**2)
    kini(5,11)=2.d0*e*i/l
!
    kini(6,2)=6.d0*e*i/(l**2)
    kini(6,6)= 4.d0*e*i/l
    kini(6,8)= -6.d0*e*i/(l**2)
    kini(6,12)=2.d0*e*i/l
!
    do 40 i1 = 1, 4
        do 50 j1 = 1, 12
            kini(i1+6,j1)=-kini(i1,j1)
50      continue
40  end do
!
    kini(11,3)=-6.d0*e*i/(l**2)
    kini(11,5)= 2.d0*e*i/l
    kini(11,9)= 6.d0*e*i/(l**2)
    kini(11,11)= 4.d0*e*i/l
!
    kini(12,2)=6.d0*e*i/(l**2)
    kini(12,6)= 2.d0*e*i/l
    kini(12,8)= -6.d0*e*i/(l**2)
    kini(12,12)= 4.d0*e*i/l
!
!----------------------C
!--                  --C
!-- MATRICE DE MASSE --C
!--                  --C
!----------------------C
!
    mini(1,1)=1.d0/3
    mini(2,2)= 13.d0/35
    mini(3,3)= 13.d0/35
    mini(4,4)=rayon**2/3
    mini(5,5)=l**2/105
    mini(6,6)=l**2/105
    mini(7,7)=1.d0/3
    mini(8,8)=13.d0/35
    mini(9,9)=13.d0/35
    mini(10,10)=rayon**2/3
    mini(11,11)=l**2/105
    mini(12,12)=l**2/105
!
    mini(1,7)=1.d0/6
    mini(7,1)=1.d0/6
!
    mini(2,6)=11.d0*l/210
    mini(6,2)=11.d0*l/210
    mini(3,5)=-11.d0*l/210
    mini(5,3)=-11.d0*l/210
!
    mini(2,8)=9.d0/70
    mini(8,2)=9.d0/70
    mini(3,9)=9.d0/70
    mini(9,3)=9.d0/70
!
    mini(2,12)=-13.d0*l/420
    mini(12,2)=-13.d0*l/420
    mini(3,11)=13.d0*l/420
    mini(11,3)=13.d0*l/420
!
    mini(4,10)=rayon**2/6
    mini(10,4)=rayon**2/6
!
    mini(5,9)=-13.d0*l/420
    mini(9,5)=-13.d0*l/420
    mini(6,8)=13.d0*l/420
    mini(8,6)=13.d0*l/420
!
    mini(5,11)=-(l**2)/140
    mini(11,5)=-(l**2)/140
    mini(6,12)=-(l**2)/140
    mini(12,6)=-(l**2)/140
!
    mini(8,12)=-11.d0*l/210
    mini(12,8)=-11.d0*l/210
    mini(9,11)=11.d0*l/210
    mini(11,9)=11.d0*l/210
!
    do 110 i1 = 1, 12
        do 120 j1 = 1, 12
            mini(i1,j1)=mini(i1,j1)*rho*s
120      continue
110  end do
!
!-- DANS LA BASE GLOBALE, KR=T*KINI*T^T
    do 130 j1 = 1, 12
        do 140 k1 = 1, 12
            do 150 l1 = 1, 12
                do 160 i1 = 1, 12
                    mr(i1,j1)=mr(i1,j1)+t(i1,k1)*mini(k1,l1)*t(j1,l1)
                    kr(i1,j1)=kr(i1,j1)+t(i1,k1)*kini(k1,l1)*t(j1,l1)
160              continue
150          continue
140      continue
130  end do
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
    call jedema()
end subroutine
