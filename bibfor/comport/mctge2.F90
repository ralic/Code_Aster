subroutine mctge2(deigy, dydx, direig, eigx, eigy,&
                  edge, outofp)
    implicit none
! Declaration of real type variables

! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    real(kind=8) :: deigy(3,3)
    real(kind=8) :: dydx(6,6)
    real(kind=8) :: direig(3,3)
    real(kind=8) :: eigx(3)
    real(kind=8) :: eigy(3)
    real(kind=8) :: edge
    logical :: outofp
!
#include "asterfort/matini.h"
#include "asterfort/vecini.h"
!
! Declaration of integer type variables
    integer :: i, j, mcomp, mdim, ndim
!
    parameter (   mcomp=4    ,mdim=3     ,ndim=2     )
    real(kind=8) :: r1, r2, r3, r4, rp5, eigpr3(mcomp), foid(mcomp,mcomp)
    real(kind=8) :: sopid(mcomp), a1, sqr, r0,eigprj(mcomp,ndim)
    data&
     &    r0    ,r1    ,r2    ,r3    ,r4    ,rp5   ,sqr/&
     &    0.0d0 ,1.0d0 ,2.0d0 ,3.0d0 ,4.0d0 ,0.5d0 ,&
     &    1.4142135623730951d0/
    logical :: epflag
! Declaration of Common space variables
    common / debug / epflag
!***********************************************************************
! COMPUTE THE DERIVATIVE OF A GENERAL ISOTROPIC 2D TENSOR
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DE MOHR-COULOMB
! IN  DEIGY   : D Y_PRIN / D X_PRIN (3,3)-MATRIX
! IN  DIREIG  : DIRECTIONS PRINCIPALES DE Y_PRIN:
!                  DIREIG_1 = DIREIG(I=1-3,1)
!                  DIREIG_2 = DIREIG(I=1-3,2)
!                  DIREIG_3 = DIREIG(I=1-3,3)
! IN  EIGX    : VALEURS PRINCIPALES DE X (3)
! IN  EIGY    : VALEURS PRINCIPALES DE Y (3)
! IN  EDGE    : Y-A-T-IL DEUX  MECANISMES ACTIFS
!
! OUT DYDX    : MATRICE TANGENTE COHERENTE REACTUALISEE
! ----------------------------------------------------------------------
    call matini(6, 6, r0, dydx)
    call matini(mcomp, mcomp, r0, foid)
    call matini(mcomp, ndim, r0, eigprj)
    call vecini(mcomp, r0, sopid)
    call vecini(mcomp, r0, eigpr3)
!
    do i = 1, ndim
        foid(i,i)=r1
        sopid(i) =r1
    end do
    foid(mcomp,mcomp)=rp5
    eigpr3(mdim)     =r1
!
! Calculation of the eigenvectors EIGPRJ_1(6) EIGPRJ_2(6) EIGPRJ_3(6)
! from the eigendirections DIREIG_1(3) DIREIG_2(3) DIREIG_3(3)
! EIGPRJ_A_ij = DIREIG_A_i x DIREIG_A_j etc.
    do i = 1, mdim
        eigprj(i,1)=direig(i,1)*direig(i,1)
        eigprj(i,2)=direig(i,2)*direig(i,2)
    end do
    eigprj(mcomp,1)=direig(1,1)*direig(2,1)
    eigprj(mcomp,2)=direig(1,2)*direig(2,2)
!
    if (edge .eq. r1) then
! Derivative dY/dX for repeated in-plane eigenvalues of X
! -------------------------------------------------------
! In-plane component
        do 10 i = 1, mcomp
            do 10 j = 1, mcomp
                dydx(i,j)=(deigy(1,1)-deigy(1,2))*foid(i,j)+ deigy(1,&
                2)*sopid(i)*sopid(j)
10          continue
! out-of-plane components required
        if (outofp) then
            do 30 i = 1, mcomp
                do 30 j = 1, mcomp
                    if (i .eq. mdim .or. j .eq. mdim) dydx(i,j)= deigy(1,3)* sopid(i)*eigpr3(j)+ &
                                                      &deigy(3,1)*eigpr3(i)*sopid(j)+ deigy(3,3)*&
                                                      &eigpr3(i)*eigpr3(j)
30              continue
        endif
    else
! Derivative dY/dX for distinct in-plane eigenvalues of X
! -------------------------------------------------------
! Assemble in-plane DYDX
        a1=(eigy(1)-eigy(2))/(eigx(1)-eigx(2))
        do 60 i = 1, mcomp
            do 60 j = 1, mcomp
                dydx(i,j)=a1*(foid(i,j)-eigprj(i,1)*eigprj(j,1)-&
                eigprj(i,2)*eigprj(j,2))+ deigy(1,1)*eigprj(i,1)*&
                eigprj(j,1)+ deigy(1,2)*eigprj(i,1)*eigprj(j,2)+&
                deigy(2,1)*eigprj(i,2)*eigprj(j,1)+ deigy(2,2)*eigprj(&
                i,2)*eigprj(j,2)
60          continue
! out-of-plane components required
        if (outofp) then
            do 80 i = 1, mcomp
                do 80 j = 1, mcomp
                    if (i .eq. mdim .or. j .eq. mdim) dydx(i,j)= deigy(1,3)* eigprj(i,1)*eigpr3(j&
                                                      &)+ deigy(2,3)*eigprj(i,2)* eigpr3(j)+ deig&
                                                      &y(3,1)*eigpr3(i)*eigprj(j,1)+ deigy(3,2)*e&
                                                      &igpr3(i)*eigprj(j,2)+ deigy(3,3)* eigpr3(i&
                                                      &)*eigpr3(j)
80              continue
        endif
    endif
!
! Projection to the so-called "tensor base"
    do i = 1, mdim
        dydx(mcomp,i)=sqr*dydx(mcomp,i)
        dydx(i,mcomp)=sqr*dydx(i,mcomp)
    end do
    dydx(mcomp,mcomp)=r2*dydx(mcomp,mcomp)
!
end subroutine
