subroutine edgani(dim, y, pm, dvsitr, eqsitr,&
                  mu, ani, gamma, m, n,&
                  g, maxg, dgdy)
    implicit none
!
!
#include "asterfort/edgequ.h"
    integer :: dim
    real(kind=8) :: y(dim), pm, dvsitr(dim-1), eqsitr
    real(kind=8) :: mu, ani(6, 6), gamma(3), m(3), n(3)
    real(kind=8) :: g(dim), maxg, dgdy(dim, dim)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ----------------------------------------------------------------------
!     MODELE VISCOPLASTIQUE SANS SEUIL DE EDGAR
!     CALCUL DE LA FONCTION G DE SA DERIVEE ET DU CRITERE D ARRET
!  IN  DIM   : DIMENSION DU SYSTEME A RESOUDRE
!              DIM=NDIM+1 AVEC NDIM=6 EN 3D ET 4 EN 2D
!  IN  Y    :  Y(1)=DVEPSEL(1)
!              Y(2)=DVEPSEL(2)
!              Y(3)=DVEPSEL(3)
!              Y(4)=DVEPSEL(4)
!              Y(5)=DVEPSEL(5) (EN 3D)
!              Y(6)=DVEPSEL(6) (EN 3D)
!              Y(DIM) = DP
!  IN  PM      : DEFORMATION PLASTIQUE CUMULEE A L INSTANT MOINS
!  IN  DVSITR : CONTRAINTE DEVIATORIQUE ESSAI CONNUE
!  IN  MU      : COEFFICIENT DE MATERIAU ELASTIQUE
!  IN  ANI     : MATRICE DE HILL
!  IN  GAMMA   : COEFFICIENT VISQUEUX
!  IN  M       : COEFFICIENT VISQUEUX
!  IN  N       : COEFFICIENT VISQUEUX
!
!  OUT G       : POUR I ET J = 1 A NDIM
!       G(I)=Y(I)+Y(DIM)*ANI(I,J)*Y(J)/EQEPEL-DVEPSTR(I)
!       G(DIM)=EQEPEL-GAMMA(K)*((PM+Y(DIM))**M(K))*(Y(DIM)**N(K))
!  OUT MAXG    : MAXIMUM DE G
!  OUT DGDY    : DERIVEE DE G (TROP LONG VOIR DOC R POUR EXPRESSION)
!
    integer :: i, j, k, ndim
    real(kind=8) :: dp, eqepel, vect(dim-1)
    real(kind=8) :: pdtsca(6)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data       pdtsca/1.d0,1.d0,1.d0,2.d0,2.d0,2.d0/
!
! 1 - CALCUL DE CERTAINS PARAMETRES
! 1.1 - DIMENSION
!
    ndim=dim-1
    dp=y(dim)
!
! 1.2 - DEFORMATION ELASTIQUE EQUIVALENTE
!
    eqepel = edgequ (ndim,y,ani)
!
! 1.3 - VECTEUR ANI(I,J)*Y(J)
!
    do 5 i = 1, ndim
        vect(i)=0.d0
        do 10 j = 1, ndim
            vect(i)=vect(i)+ani(i,j)*pdtsca(j)*y(j)
10      continue
 5  end do
!
! 2 - VECTEUR G
!
    do 15 i = 1, ndim
        g(i)=y(i)-dvsitr(i)/(2.d0*mu)+dp*vect(i)/eqepel
        g(i)=-g(i)
15  end do
!
    g(dim)= eqepel
    do 20 k = 1, 3
        g(dim)=g(dim)-gamma(k)*((pm+dp)**m(k))*(dp**n(k))
20  end do
    g(dim)=-g(dim)
!
! 3 - MAX DE G POUR CRITERE REMIS EN CONTRAINTE
!
    maxg=abs(g(1))
    do 25 i = 2, dim
        if (abs(g(i)) .gt. maxg) maxg=abs(g(i))
25  end do
    maxg=2.d0*mu*maxg
!
! 2 - DERIVEE DU VECTEUR G
!     = [DG(1)/DVEPSEL(1)... .DG(1)/DVEPSEL(NDIM)    - DG(1)/DP   ]
!       [.......................................    .- ...... ....]
!       [DG(NDIM)/DVEPSEL(1)..DG(NDIM)/DVEPSEL(NDIM) - DG(NDIM)/DP]
!       [---------------------------------------------------------]
!       [DG(DIM)/DVEPSEL(1)..DG(DIM)/DVEPSEL(NDIM)   - DG(DIM)/DP]
!
    do 30 i = 1, ndim
        do 35 j = 1, ndim
            dgdy(i,j)=dp*ani(i,j)/eqepel -dp*vect(i)*vect(j)/(eqepel**&
            3.d0)
            if (i .eq. j) then
                if (i .le. 3) dgdy(i,j)=dgdy(i,j)+1.d0
                if (i .ge. 4) dgdy(i,j)=dgdy(i,j)+0.5d0
            endif
            if (j .ge. 4) dgdy(i,j)=2.d0*dgdy(i,j)
35      continue
30  end do
!
    do 40 i = 1, ndim
        dgdy(i,dim)=vect(i)/eqepel
40  end do
!
    do 45 j = 1, ndim
        dgdy(dim,j)=vect(j)/eqepel
        if (j .ge. 4) dgdy(dim,j)=2.d0*dgdy(dim,j)
45  end do
!
    dgdy(dim,dim)=0.d0
    do 50 k = 1, 3
        dgdy(dim,dim)=dgdy(dim,dim) -gamma(k)*m(k)*((pm+dp)**(m(k)-&
        1.d0))*(dp**n(k)) -gamma(k)*n(k)*((pm+dp)**m(k))*(dp**(n(k)-&
        1.d0))
50  end do
!
end subroutine
