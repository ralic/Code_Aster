subroutine matcox(ndim, pp, ddt1, ddt2, ddt3,&
                  ddt4, p, nno, ddlh, ddls,&
                  jac, ffp, singu, rr, mmat)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'asterfort/matini.h'
    real(kind=8) :: ddt1(3, 3), ddt2(3, 3), ddt3(3, 3), ddt4(3, 3), pp(3, 3)
    real(kind=8) :: p(3, 3), mmat(216, 216)
    real(kind=8) :: jac, ffp(27), rr
    integer :: ndim, ddlh, ddls, nno, singu
!.......................................................................
!
!         CALCUL DES MATRICES DE CONTACT FROTTEMENT
!                 LOI COHESIVE - POUR X-FEM
!                     (METHODE CONTINUE)
!
!
!
!  ENTREES  --->  PP,P,JAC,FFP,RR,NDIM,DDLH,DDLS,NNO
!           --->  SINGU,DDT1,DDT2,DDT3,DDT4
!  SORTIES  --->  MMAT
!
!.......................................................................
    real(kind=8) :: ddt11(3, 3), ddt21(3, 3), ddt31(3, 3), ddt41(3, 3)
    real(kind=8) :: ddt111(3, 3), ddt211(3, 3), ddt311(3, 3), ddt411(3, 3)
    integer :: i, j, k, l
!
!.......................................................................
!
    call matini(3, 3, 0.d0, ddt11)
    call matini(3, 3, 0.d0, ddt21)
    call matini(3, 3, 0.d0, ddt31)
    call matini(3, 3, 0.d0, ddt41)
!
    call matini(3, 3, 0.d0, ddt111)
    call matini(3, 3, 0.d0, ddt211)
    call matini(3, 3, 0.d0, ddt311)
    call matini(3, 3, 0.d0, ddt411)
!
    do 1 i = 1, ndim
        do 2 j = 1, ndim
            do 3 l = 1, ndim
                ddt11(i,j)=ddt11(i,j) + pp(i,l)*ddt1(l,j)
                ddt21(i,j)=ddt21(i,j) + pp(i,l)*ddt2(l,j)
                ddt31(i,j)=ddt31(i,j) + p(i,l)*ddt3(l,j)
                ddt41(i,j)=ddt41(i,j) + p(i,l)*ddt4(l,j)
 3          continue
 2      continue
 1  end do
!
    do 4 i = 1, ndim
        do 5 j = 1, ndim
            do 6 l = 1, ndim
                ddt111(i,j)=ddt111(i,j) + ddt11(i,l)*pp(l,j)
                ddt211(i,j)=ddt211(i,j) + ddt21(i,l)*p(l,j)
                ddt311(i,j)=ddt311(i,j) + ddt31(i,l)*pp(l,j)
                ddt411(i,j)=ddt411(i,j) + ddt41(i,l)*p(l,j)
 6          continue
 5      continue
 4  end do
!
    do 7 i = 1, nno
        do 8 j = 1, nno
            do 9 k = 1, ddlh
                do 10 l = 1, ddlh
!
                    mmat(ddls*(i-1)+ndim+k,ddls*(j-1)+ndim+l) =&
                    mmat(ddls*(i-1)+ndim+k,ddls*(j-1)+ndim+l)+&
                    4.d0*ffp(i)*ddt111(k,l)*ffp(j)*jac +4.d0*ffp(i)*&
                    ddt211(k,l)*ffp(j)*jac +4.d0*ffp(i)*ddt311(k,l)*&
                    ffp(j)*jac +4.d0*ffp(i)*ddt411(k,l)*ffp(j)*jac
!
10              continue
!
                do 11 l = 1, singu*ndim
                    mmat(ddls*(i-1)+ndim+k,ddls*(j-1)+ndim+ddlh+l) =&
                    mmat(ddls*(i-1)+ndim+k,ddls*(j-1)+ndim+ddlh+l)+&
                    4.d0*ffp(i)*ddt111(k,l)*ffp(j)*jac*rr +4.d0*ffp(i)&
                    *ddt211(k,l)*ffp(j)*jac*rr +4.d0*ffp(i)*ddt311(k,&
                    l)*ffp(j)*jac*rr +4.d0*ffp(i)*ddt411(k,l)*ffp(j)*&
                    jac*rr
11              continue
!
 9          continue
            do 12 k = 1, singu*ndim
                do 13 l = 1, ddlh
                    mmat(ddls*(i-1)+ndim+ddlh+k,ddls*(j-1)+ndim+l) =&
                    mmat(ddls*(i-1)+ndim+ddlh+k,ddls*(j-1)+ndim+l)+&
                    4.d0*ffp(i)*ddt111(k,l)*ffp(j)*jac*rr +4.d0*ffp(i)&
                    *ddt211(k,l)*ffp(j)*jac*rr +4.d0*ffp(i)*ddt311(k,&
                    l)*ffp(j)*jac*rr +4.d0*ffp(i)*ddt411(k,l)*ffp(j)*&
                    jac*rr
13              continue
!
                do 14 l = 1, singu*ndim
!
                    mmat(ddls*(i-1)+ndim+ddlh+k,ddls*(j-1)+ndim+ddlh+&
                    l) = mmat(ddls*(i-1)+ndim+ddlh+k,ddls*(j-1)+ndim+&
                    ddlh+l)+ 4.d0*ffp(i)*ddt111(k,l)*ffp(j)*jac*rr*rr&
                    +4.d0*ffp(i)*ddt211(k,l)*ffp(j)*jac*rr*rr +4.d0*&
                    ffp(i)*ddt311(k,l)*ffp(j)*jac*rr*rr +4.d0*ffp(i)*&
                    ddt411(k,l)*ffp(j)*jac*rr*rr
!
14              continue
12          continue
!
!
 8      continue
 7  end do
!
end subroutine
