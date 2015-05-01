subroutine mctgel(dydx, rprops)
!
    implicit none
! Declaration of real type variables

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    real(kind=8) :: dydx(6,6)
    real(kind=8) :: rprops(*)
!
#include "asterfort/matini.h"
! Declaration of integer type variables
    integer :: i, j, ndim, mdim
!
    parameter&
     &(   ndim=6    ,mdim=3     )
!
    real(kind=8) :: foid(mdim,mdim), young, poiss, gmodu, bulk, factor
    real(kind=8) :: r0, r1, r2, r3, r4, r2g, r1d3
!
    data&
     &    r0    ,r1    ,r2    ,r3    ,r4    /&
     &    0.0d0 ,1.0d0 ,2.0d0 ,3.0d0 ,4.0d0 /
!***********************************************************************
! COMPUTATION OF THE TANGENT MODULUS (ELASTICITY MATRIX) FOR THE LINEAR
! ELASTIC MATERIAL MODEL
!
!***********************************************************************
    call matini(mdim, mdim, r0, foid)
    call matini(ndim, ndim, r0, dydx)
    foid(1,1)=r1
    foid(2,2)=r1
    foid(3,3)=r1
! Set shear and bulk modulus
! --------------------------
    young=rprops(2)
    poiss=rprops(3)
    gmodu=young/(r2*(r1+poiss))
    bulk =young/(r3*(r1-r2*poiss))
    r1d3=r1/r3
    r2g=r2*gmodu
    factor=bulk-r2g*r1d3
!
! Assemble matrix
!
    do 10 i = 1, mdim
        dydx(mdim+i,mdim+i)=r2g
        do 10 j = 1, mdim
            dydx(i,j)=r2g*foid(i,j)+factor
10      continue
!
end subroutine
