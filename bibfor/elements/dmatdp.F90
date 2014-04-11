subroutine dmatdp(fami, mater, time, poum, ipg,&
                  ispg, repere, d)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dpao2d.h"
#include "asterfort/get_elas_para.h"
#include "asterfort/utbtab.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: fami
    integer, intent(in) :: mater
    real(kind=8), intent(in) :: time
    character(len=*), intent(in) :: poum
    integer, intent(in) :: ipg
    integer, intent(in) :: ispg
    real(kind=8), intent(in) :: repere(7)
    real(kind=8), intent(out) :: d(4, 4)
!
! --------------------------------------------------------------------------------------------------
!
! Hooke matrix for iso-parametric elements
!
! Plane strain and axisymmetric
!
! --------------------------------------------------------------------------------------------------
!
! In  fami   : Gauss family for integration point rule
! In  mater  : material parameters
! In  time   : current time
! In  poum   : '-' or '+' for parameters evaluation (previous or current temperature)
! In  ipg    : current point gauss
! In  ispg   : current "sous-point" gauss
! In  repere : local basis for orthotropic elasticity
! Out d      : Hooke matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: irep, i, j
    integer :: elas_type
    real(kind=8) :: passag(4, 4), dorth(4, 4), work(4, 4)
    real(kind=8) :: zero, undemi, un, deux
    real(kind=8) :: nu, nu12, nu21, nu13, nu23, nu31, nu32
    real(kind=8) :: e1, e2, e3, e
    real(kind=8) :: g1, g2, g3, g
    real(kind=8) :: c1, delta
!
! --------------------------------------------------------------------------------------------------
!
    zero       = 0.0d0
    undemi     = 0.5d0
    un         = 1.0d0
    deux       = 2.0d0
    d(:,:)     = 0.d0
    dorth(:,:) = 0.d0
    work(:,:)  = 0.d0
!
! - Get elastic parameters
!
    call get_elas_para(fami, mater    , time, poum, ipg,&
                       ispg, elas_type,&
                       e   , nu , g,&
                       e1  , e2 , e3, nu12, nu13, nu23, g1, g2, g3)
!
! - Compute Hooke matrix
!
    if (elas_type.eq.1) then
!
! ----- Isotropic matrix
!
        d(1,1) = e*(un-nu)*g
        d(1,2) = e*nu*g
        d(1,3) = e*nu*g
        d(2,1) = e*nu*g
        d(2,2) = e*(un-nu)*g
        d(2,3) = e*nu*g
        d(3,1) = e*nu*g
        d(3,2) = e*nu*g
        d(3,3) = e*(un-nu)*g
        d(4,4) = undemi*e/(un+nu)
!
    else if (elas_type.eq.2) then
!
! ----- Orthotropic matrix
!
        nu21 = e2*nu12/e1
        nu31 = e3*nu13/e1
        nu32 = e3*nu23/e2
        delta = un-nu23*nu32-nu31*nu13-nu21*nu12-deux*nu23*nu31*nu12
        dorth(1,1) = (un - nu23*nu32)*e1/delta
        dorth(1,2) = (nu21 + nu31*nu23)*e1/delta
        dorth(1,3) = (nu31 + nu21*nu32)*e1/delta
        dorth(2,2) = (un - nu13*nu31)*e2/delta
        dorth(2,3) = (nu32 + nu31*nu12)*e2/delta
        dorth(3,3) = (un - nu21*nu12)*e3/delta
        dorth(2,1) = dorth(1,2)
        dorth(3,1) = dorth(1,3)
        dorth(3,2) = dorth(2,3)
        dorth(4,4) = g1
!
! ----- Matrix from orthotropic basis to global 3D basis
!
        call dpao2d(repere, irep, passag)
!
! ----- Hooke matrix in global 3D basis
!
        ASSERT((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 4, 4, dorth, passag,&
                        work, d)
        else if (irep.eq.0) then
            do i = 1, 4
                do j = 1, 4
                    d(i,j) = dorth(i,j)
                end do
            end do
        endif
!
    else if (elas_type.eq.3) then
!
! ----- Transverse isotropic matrix
!
        c1 = e1/ (un+nu12)
        delta = un - nu12 - deux*nu13*nu13*e3/e1
        dorth(1,1) = c1* (un-nu13*nu13*e3/e1)/delta
        dorth(1,2) = c1* ((un-nu13*nu13*e3/e1)/delta-un)
        dorth(1,3) = e3*nu13/delta
        dorth(2,1) = dorth(1,2)
        dorth(2,2) = dorth(1,1)
        dorth(2,3) = dorth(1,3)
        dorth(3,1) = dorth(1,3)
        dorth(3,2) = dorth(2,3)
        dorth(3,3) = e3* (un-nu12)/delta
        dorth(4,4) = undemi*c1
!
! ----- Matrix from transverse isotropic basis to global 3D basis
!
        call dpao2d(repere, irep, passag)
!
! ----- Hooke matrix in global 3D basis
!
        ASSERT((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 4, 4, dorth, passag,&
                        work, d)
        else if (irep.eq.0) then
            do i = 1, 4
                do j = 1, 4
                    d(i,j) = dorth(i,j)
                end do
            end do
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
