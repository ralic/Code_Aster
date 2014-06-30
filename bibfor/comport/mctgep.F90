subroutine mctgep(deigy, dydx, eigx, eigy, vecx,&
                  direig, edge, apex)
!
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
    real(kind=8) :: eigx(3)
    real(kind=8) :: eigy(3)
    real(kind=8) :: vecx(6)
    real(kind=8) :: direig(3,3)
    real(kind=8) :: edge
    real(kind=8) :: apex
!
#include "asterfort/matini.h"
#include "asterfort/vecini.h"
!
! Declaration of integer type variables
    integer :: i, j, ia, ib, ic, mcomp, mdim
!
    logical(kind=1) ::  epflag
!
    parameter ( mcomp=6    ,mdim=3 )
!
    real(kind=8) :: dx2dx(mcomp,mcomp), eigprj(mcomp,mdim), foid(mcomp,mcomp), sopid(mcomp), s1, s2
    real(kind=8) :: s3, s4, s5, s6, xi, xj, a1
    real(kind=8) :: r0, r1, r2, r3, dr5, small, tol, sqr
    data&
     &    r0    ,r1    ,r2    ,r3    ,dr5   ,small ,tol   ,sqr/&
     &    0.0d0 ,1.0d0 ,2.0d0 ,3.0d0 ,0.5d0 ,1.d-06,1.d-10,&
     &    1.4142135623730951d0/
!
! Declaration of Common space variables
    common / debug / epflag
!
!***********************************************************************
! COMPUTE THE DERIVATIVE OF A GENERAL ISOTROPIC 3D TENSOR
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT DE MOHR-COULOMB
! IN  DEIGY   : D Y_PRIN / D X_PRIN (3,3)-MATRIX
! IN  DIREIG  : DIRECTIONS PRINCIPALES DE Y_PRIN:
!                  DIREIG_1 = DIREIG(I=1-3,1)
!                  DIREIG_2 = DIREIG(I=1-3,2)
!                  DIREIG_3 = DIREIG(I=1-3,3)
! IN  EIGX    : VALEURS PRINCIPALES DE X (3)
! IN  EIGY    : VALEURS PRINCIPALES DE Y (3)
! IN  VECX    : VECTEUR X DANS LA BASE CARTESIENNE
!                  VECX=(EPXX         EPYY         EPZZ
!                        Sqrt(2)*EPXY Sqrt(2)*EPXZ Sqrt(2)*EPYZ)
! IN  EDGE    : Y-A-T-IL DEUX  MECANISMES ACTIFS
! IN  APEX    : Y-A-T-IL TROIS MECANISMES ACTIFS
!
! OUT DYDX    : MATRICE TANGENTE COHERENTE REACTUALISEE
! ----------------------------------------------------------------------
    call matini(mcomp, mcomp, r0, foid)
    call matini(mcomp, mcomp, r0, dx2dx)
    call matini(mcomp, mcomp, r0, dydx)
    call vecini(mcomp, r0, sopid)
!
! 4th-order symetric unit tensor
    do i = 1, 3
        foid(i,i) =r1
        foid(i+3,i+3)=dr5
        sopid(i) =r1
    end do
!
! Calculation of the eigenvectors EIGPRJ_1(6) EIGPRJ_2(6) EIGPRJ_3(6)
! from the eigendirections DIREIG_1(3) DIREIG_2(3) DIREIG_3(3)
! EIGPRJ_A_ij = DIREIG_A_i x DIREIG_A_j etc.
    do i = 1, 3
        eigprj(i,1)=direig(i,1)*direig(i,1)
        eigprj(i,2)=direig(i,2)*direig(i,2)
        eigprj(i,3)=direig(i,3)*direig(i,3)
        ia=1
        ib=2
        if (i .eq. 2) then
            ia=1
            ib=3
        else if (i.eq.3) then
            ia=2
            ib=3
        endif
        eigprj(3+i,1)=direig(ia,1)*direig(ib,1)
        eigprj(3+i,2)=direig(ia,2)*direig(ib,2)
        eigprj(3+i,3)=direig(ia,3)*direig(ib,3)
    end do
!
! 4th-order D(X.X)/DX tensor
! Be careful that we have the following form of input strain:
! VECX = (EPXX EPYY EPZZ EPXY EPXZ EPYZ)
    dx2dx(1,1) =r2*vecx(1)
    dx2dx(2,2) =r2*vecx(2)
    dx2dx(3,3) =r2*vecx(3)
    dx2dx(1,4) =vecx(4)
    dx2dx(1,5) =vecx(5)
    dx2dx(2,4) =vecx(4)
    dx2dx(2,6) =vecx(6)
    dx2dx(3,5) =vecx(5)
    dx2dx(3,6) =vecx(6)
!
    dx2dx(4,4) =dr5*(vecx(1)+vecx(2))
    dx2dx(5,5) =dr5*(vecx(1)+vecx(3))
    dx2dx(6,6) =dr5*(vecx(3)+vecx(2))
    dx2dx(4,5) =dr5*vecx(6)
    dx2dx(4,6) =dr5*vecx(5)
    dx2dx(5,6) =dr5*vecx(4)
! Symetrization
    do i = 2, mcomp
        do j = 1, i-1
            dx2dx(i,j) =dx2dx(j,i)
        end do
    end do
!
    if (apex .eq. r1) then
!
        if (epflag) write(6,'(A)')'> IN MCTGEP :: APEX'
!
! Derivative dY/dX for 3 repeated in-plane eigenvalues of X
! ---------------------------------------------------------
        do i = 1, mcomp
            do j = 1, mcomp
                dydx(i,j)=(deigy(1,1)-deigy(1,2))*foid(i,j)+ deigy(1,&
                2)*sopid(i)*sopid(j)
            end do
        end do
!
    else if (edge.eq.r1) then
!
        if (epflag) write(6,'(A)')'> IN MCTGEP :: EDGE'
!
! Derivative dY/dX for 2 repeated in-plane eigenvalues of X
! ---------------------------------------------------------
        s1 =(eigy(1)-eigy(3))/(eigx(1)-eigx(3))**r2 + (deigy(3,2)-&
        deigy(3,3))/(eigx(1)-eigx(3))
!
        s2 =r2*eigx(3)* (eigy(1)-eigy(3))/(eigx(1)-eigx(3))**r2 +&
        (eigx(1)+eigx(3))/(eigx(1)-eigx(3))* (deigy(3,2)-deigy(3,3))
!
        s3 =r2*(eigy(1)-eigy(3))/(eigx(1)-eigx(3))**r3 + (deigy(1,3)+&
        deigy(3,1)-deigy(1,1)-deigy(3,3))/ (eigx(1)-eigx(3))**r2
!
        s4 =r2*eigx(3)* (eigy(1)-eigy(3))/(eigx(1)-eigx(3))**r3 +&
        (deigy(1,3)-deigy(3,2))/(eigx(1)-eigx(3)) + (deigy(1,3)+deigy(&
        3,1)-deigy(1,1)-deigy(3,3))* eigx(3)/(eigx(1)-eigx(3))**r2
!
        s5 =r2*eigx(3)* (eigy(1)-eigy(3))/(eigx(1)-eigx(3))**r3 +&
        (deigy(3,1)-deigy(3,2))/(eigx(1)-eigx(3)) + (deigy(1,3)+deigy(&
        3,1)-deigy(1,1)-deigy(3,3))* eigx(3)/(eigx(1)-eigx(3))**r2
!
        s6 =r2*eigx(3)**r2*(eigy(1)-eigy(3))/ (eigx(1)-eigx(3))**r3+(&
        deigy(1,3)+deigy(3,1))* eigx(1)*eigx(3)/(eigx(1)-eigx(3))**r2-&
        eigx(3)**r2/(eigx(1)-eigx(3))**r2* (deigy(1,1)+deigy(3,3))-(&
        eigx(1)+eigx(3))/ (eigx(1)-eigx(3))*deigy(3,2)
!
        do i = 1, mcomp
            xi =vecx(i)
            do j = 1, mcomp
                xj =vecx(j)
                dydx(i,j)=s1*dx2dx(i,j)-s2*foid(i,j)-s3*xi*xj+&
                s4*xi*sopid(j)+s5*sopid(i)*xj- s6*sopid(i)*sopid(j)
            end do
        end do
!
    else
        if (epflag) write(6,'(A)')'> IN MCTGEP :: PLANE'
!
        do ia = 1, mdim
!
            ib=2
            ic=3
            if (ia .eq. 2) then
                ib=3
                ic=1
            else if (ia.eq.3) then
                ib=1
                ic=2
            endif
            a1=eigy(ia)/(eigx(ia)-eigx(ib))/(eigx(ia)-eigx(ic))
!
            do i = 1, mcomp
                do j = 1, mcomp
                    dydx(i,j)=dydx(i,j)+a1*( dx2dx(i,j)-(eigx(ib)+&
                    eigx(ic))*foid(i,j)- (r2*eigx(ia)-eigx(ib)-eigx(&
                    ic))*eigprj(i,ia)* eigprj(j,ia)-(eigx(ib)-eigx(ic)&
                    )*(eigprj(i,ib)* eigprj(j,ib)-eigprj(i,ic)*eigprj(&
                    j,ic)) )
                end do
            end do
        end do
!
        do ia = 1, mdim
            do ib = 1, mdim
                do i = 1, mcomp
                    do j = 1, mcomp
                        dydx(i,j)=dydx(i,j)+ deigy(ia,ib)*eigprj(i,ia)&
                        *eigprj(j,ib)
                    end do
                end do
            end do
        end do
    endif
!
! Projection to the so-called "tensor base"
    do i = 1, mdim
        do j = mdim+1, mcomp
            dydx(i,j)=sqr*dydx(i,j)
            dydx(j,i)=sqr*dydx(j,i)
        end do
    end do
!
    do i = mdim+1, mcomp
        do j = mdim+1, mcomp
            dydx(i,j)=r2*dydx(i,j)
        end do
    end do
!
end subroutine
