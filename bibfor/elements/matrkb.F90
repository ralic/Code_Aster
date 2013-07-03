subroutine matrkb(nb1, ndimx, nddlx, nddlet, ktdc,&
                  alpha, rig1, coef)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "asterc/r8prem.h"
    integer :: nb1, nb2
    integer :: ndimx, nddlx, nddlet
!     REAL*8 KTDC(NDDLE,NDDLE),RIG1(NDDLET,NDDLET)
!     REAL*8 ALPHA,COEF
!
    real(kind=8) :: ktdc(ndimx, ndimx), rig1(nddlx, nddlx)
    real(kind=8) :: alpha, coef
!
    real(kind=8) :: rigrl ( 2 , 2 )
    real(kind=8) :: xmin
!
    integer :: i1, i2, i, ib, ir, in, ii
    integer :: j1, j2, j, jb, jj, jr
    integer :: kompti, komptj
!
!
!
!     RECHERCHE DU MINIMUM DE LA MATRICE KTDC = INF DES TERMES DIAGONAUX
!
!
    xmin = 1.d0 / r8prem ( )
!
    nb2 = nb1 + 1
!
    do 402 in = 1, nb2
!
!------- ON CONSTRUIT RIGRL
!
        if (in .le. nb1) then
!
!----------- NOEUDS DE SERENDIP
            do 431 jj = 1, 2
                jr = 5 * ( in - 1 ) + jj + 3
                do 441 ii = 1, 2
                    ir = 5 * ( in - 1 ) + ii + 3
                    rigrl ( ii , jj ) = ktdc ( ir , jr )
441              continue
431          continue
!
        else
!
!----------- SUPERNOEUD
            do 451 jj = 1, 2
                jr = 5 * nb1 + jj
                do 461 ii = 1, 2
                    ir = 5 * nb1 + ii
                    rigrl ( ii , jj ) = ktdc ( ir , jr )
461              continue
451          continue
!
        endif
!
!-------    ON COMPARE LES DEUX PREMIERS TERMES DIAGONAUX DE RIGRL
!
        if (rigrl ( 1 , 1 ) .lt. xmin) xmin = rigrl ( 1 , 1 )
        if (rigrl ( 2 , 2 ) .lt. xmin) xmin = rigrl ( 2 , 2 )
!
402  end do
!
!
    coef=alpha * xmin
!
!     RAIDEUR ASSOCIEE A LA ROTATION FICTIVE = COEF = ALPHA * INF
!
    do 20 i = 1, nddlet
        do 30 j = 1, nddlet
            rig1(i,j)=0.d0
30      end do
20  end do
!
!     CONSTRUCTION DE KBARRE = KTILD EXTENDU  :  (NDDLET,NDDLET)
!
    kompti=-1
    komptj=-1
!
    nb2=nb1+1
!
    do 40 ib = 1, nb2
        kompti=kompti+1
        do 50 jb = 1, nb2
            komptj=komptj+1
            if ((ib.le.nb1) .and. (jb.le.nb1)) then
                do 60 i = 1, 5
                    i1=5*(ib-1)+i
                    i2=i1+kompti
                    do 70 j = 1, 5
                        j1=5*(jb-1)+j
                        j2=j1+komptj
                        rig1(i2,j2)=ktdc(i1,j1)
70                  end do
60              end do
!
            else if ((ib.le.nb1).and.(jb.eq.nb2)) then
                do 95 i = 1, 5
                    i1=5*(ib-1)+i
                    i2=i1+kompti
                    do 100 j = 1, 2
                        j1=5*nb1+j
                        j2=j1+komptj
                        rig1(i2,j2)=ktdc(i1,j1)
100                  end do
95              end do
!
            else if ((ib.eq.nb2).and.(jb.le.nb1)) then
                do 85 i = 1, 2
                    i1=5*nb1+i
                    i2=i1+kompti
                    do 90 j = 1, 5
                        j1=5*(jb-1)+j
                        j2=j1+komptj
                        rig1(i2,j2)=ktdc(i1,j1)
90                  end do
85              end do
!
            else if ((ib.eq.nb2).and.(jb.eq.nb2)) then
                do 105 i = 1, 2
                    i1=5*nb1+i
                    i2=i1+kompti
                    do 110 j = 1, 2
                        j1=5*nb1+j
                        j2=j1+komptj
                        rig1(i2,j2)=ktdc(i1,j1)
110                  end do
105              end do
!
            endif
!
50      end do
!
        if (ib .le. nb1) then
            rig1(6*ib,6*ib)=coef
        else
            rig1(nddlet,nddlet)=coef
        endif
!
        komptj=-1
40  end do
!
end subroutine
