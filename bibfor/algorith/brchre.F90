subroutine brchre(x066, v33, v33t, x166)
!
!     ROUTINE ANCIENNEMENT NOMMEE CHREPX66
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
!
!     CONSTRUCTION DE L OPERATEUR DE ROTATION R PASSAGE DE 0 A 1
!     DE LA BASE DE REFERENCE A LA BASE PRINCIPALE
!     DES ENDOMMAGEMENTS DE RANKINE ET DE SON INVERSE RM1
    implicit none
#include "asterfort/brindz.h"
#include "asterfort/matini.h"
#include "asterfort/utbtab.h"
    real(kind=8) :: x33(3, 3), z33(3, 3), v33(3, 3), v33t(3, 3)
    real(kind=8) :: x066(6, 6), x166(6, 6), a66(6, 6)
    integer :: i, j, k, m, n
    real(kind=8) :: r(6, 6), rm1(6, 6), zz, trav(3, 3)
!
    zz = 0.d0
!
    do 10 i = 1, 6
        call matini(3, 3, zz, x33)
        call brindz(i, m, n)
        x33(m,n)=1.d0
        x33(n,m)=1.d0
        call utbtab('ZERO', 3, 3, x33, v33,&
                    trav, z33)
        do 20 j = 1, 3
            r(j,i)=z33(j,j)
20      end do
        r(4,i)=z33(1,2)
        r(5,i)=z33(1,3)
        r(6,i)=z33(2,3)
        call utbtab('ZERO', 3, 3, x33, v33t,&
                    trav, z33)
        do 30 j = 1, 3
            rm1(j,i)=z33(j,j)
30      end do
        rm1(4,i)=z33(1,2)
        rm1(5,i)=z33(1,3)
        rm1(6,i)=z33(2,3)
10  end do
!
!      PRINT*,'R'
!      CALL AFFICHE66(R)
!      PRINT*,'RM1'
!      CALL AFFICHE66(RM1)
!
!     VERIF R*RM1=I
!      DO I=1,6
!       DO J=1,6
!        A66(I,J)=0.
!        DO K=1,6
!         A66(I,J)=A66(I,J)+R(I,K)*RM1(K,J)
!        END DO
!       END DO
!      END DO
!      PRINT*,'R*RM1'
!      CALL AFFICHE66(A66)
!
!     PASSAGE DE LA MATRICE DES ENDOMMAGEMENTS DANS LA BASE FIXE
    do 40 i = 1, 6
        do 50 j = 1, 6
            a66(i,j)=0.d0
            do 60 k = 1, 6
                a66(i,j)=a66(i,j)+x066(i,k)*rm1(k,j)
60          continue
50      continue
40  end do
    do 70 i = 1, 6
        do 80 j = 1, 6
            x166(i,j)=0.d0
            do 90 k = 1, 6
                x166(i,j)=x166(i,j)+r(i,k)*a66(k,j)
90          continue
80      continue
70  end do
!      PRINT*,'DT66 EN BASE FIXE'
!      CALL AFFICHE66(DT166)
!      READ*
end subroutine
