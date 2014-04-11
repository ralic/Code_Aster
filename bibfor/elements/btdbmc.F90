subroutine btdbmc(b, d, jacob, ndim, nno,&
                  nbsig, elas_type, btdb)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/btdbpr.h"
#include "asterfort/lteatt.h"
#include "asterfort/utmess.h"
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
    integer, intent(in) :: nbsig
    integer, intent(in) :: ndim
    integer, intent(in) :: nno
    integer, intent(in) :: elas_type
    real(kind=8), intent(in) :: b(nbsig, *)
    real(kind=8), intent(in) :: d(nbsig, *)
    real(kind=8), intent(in) :: jacob
    real(kind=8), intent(out) :: btdb(81, 81)
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elastic rigidity matrix [K] = [B]Tx[D]x[B]
!
! --------------------------------------------------------------------------------------------------
!
! In  nbsig        : number of stress components
! In  ndim         : dimension of element
! In  nno          : number of nodes
! In  elas_type    : Type of elasticity
!                    1 - Isotropic
!                    2 - Orthotropic
!                    3 - Transverse isotropic
! In  jacob        : product local jacobian by integration weight
! In  b            : displacement to strain  (first order) matrix [B]
! In  d            : Hooke matrix [D]
! Out btdb         : elastic rigidity matrix [K] = [B]Tx[D]x[B]
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i1, i2, l1, l2, nbinco
    real(kind=8) :: d1, d2, d3, r11, r12, r13, r21
    real(kind=8) :: r22, r23, r31, r32, r33, rs13, rs23
    real(kind=8) :: rs33, s33, sr31, sr32, sr33
!
! --------------------------------------------------------------------------------------------------
!
    nbinco = nno*ndim
!
    if (elas_type.eq.1) then
!
! ----- Isotropic elasticity
!
        if (lteatt('C_PLAN','OUI') .or. lteatt('D_PLAN','OUI')) then
!
! --------- Plane stress/plane strain
!
            l1 = nbinco - 1
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
            do i1 = 1, l1, 2
                do i2 = 1, l1, 2
                    r11 = b(1,i1) *b(1,i2)
                    r21 = b(2,i1+1)*b(1,i2)
                    r12 = b(1,i1) *b(2,i2+1)
                    r22 = b(2,i1+1)*b(2,i2+1)
                    btdb(i1,i2) = btdb(i1,i2) + d1*r11 + d3*r22
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*r21 + d3*r12
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*r12 + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3* r11
                end do
            end do
!
        elseif (lteatt('AXIS','OUI').and. (.not.lteatt('FOURIER','OUI'))) then
!
! --------- Axi-symmetric
!
            l1 = nbinco - 1
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
            do i1 = 1, l1, 2
                do i2 = 1, l1, 2
                    r11 = b(1,i1) *b(1,i2)
                    r21 = b(2,i1+1)*b(1,i2)
                    r31 = b(3,i1) *b(1,i2)
                    r12 = b(1,i1) *b(2,i2+1)
                    r22 = b(2,i1+1)*b(2,i2+1)
                    r32 = b(3,i1) *b(2,i2+1)
                    r13 = b(1,i1) *b(3,i2)
                    r23 = b(2,i1+1)*b(3,i2)
                    r33 = b(3,i1) *b(3,i2)
                    btdb(i1,i2) = btdb(i1,i2) + d1*(r11+r33) + d2*( r31+r13) + d3*r22
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*(r21+r23) + d3*r12
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*(r12+r32) + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3* r11
                end do
            end do
!
        else if (lteatt('DIM_TOPO_MAILLE','3')) then
!
! --------- 3D
!
            l2 = nbinco - 2
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
            do i1 = 1, l2, 3
                do i2 = 1, l2, 3
                    r11 = b(1,i1)*b(1,i2)
                    r12 = b(1,i1)*b(2,i2+1)
                    r13 = b(1,i1)*b(3,i2+2)
                    r21 = b(2,i1+1)*b(1,i2)
                    r22 = b(2,i1+1)*b(2,i2+1)
                    r23 = b(2,i1+1)*b(3,i2+2)
                    r31 = b(3,i1+2)*b(1,i2)
                    r32 = b(3,i1+2)*b(2,i2+1)
                    r33 = b(3,i1+2)*b(3,i2+2)
                    btdb(i1,i2) = btdb(i1,i2) + d1*r11 + d3*(r22+r33)
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*r21 + d3*r12
                    btdb(i1+2,i2) = btdb(i1+2,i2) + d2*r31 + d3*r13
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*r12 + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3*( r11+r33)
                    btdb(i1+2,i2+1) = btdb(i1+2,i2+1) + d2*r32 + d3* r23
                    btdb(i1, i2+2) = btdb(i1, i2+2) + d2*r13 + d3*r31
                    btdb(i1+1,i2+2) = btdb(i1+1,i2+2) + d2*r23 + d3* r32
                    btdb(i1+2,i2+2) = btdb(i1+2,i2+2) + d1*r33 + d3*( r11+r22)
                end do
            end do
!
        else if (lteatt('FOURIER','OUI')) then
!
! --------- Fourier
!
            l2 = nbinco - 2
            d1 = d(1,1)*jacob
            d2 = d(1,2)*jacob
            d3 = d(4,4)*jacob
            do i1 = 1, l2, 3
                do i2 = 1, l2, 3
                    r11 = b(1,i1)*b(1,i2)
                    r12 = b(1,i1)*b(2,i2+1)
                    r13 = b(1,i1)*b(3,i2+2)
                    r21 = b(2,i1+1)*b(1,i2)
                    r22 = b(2,i1+1)*b(2,i2+1)
                    r23 = b(2,i1+1)*b(3,i2+2)
                    r31 = b(3,i1+2)*b(1,i2)
                    r32 = b(3,i1+2)*b(2,i2+1)
                    r33 = b(3,i1+2)*b(3,i2+2)
                    rs13 = b(1,i1) *b(3,i2)
                    rs23 = b(2,i1+1)*b(3,i2)
                    sr31 = b(3,i1) *b(1,i2)
                    sr32 = b(3,i1) *b(2,i2+1)
                    sr33 = b(3,i1) *b(3,i2+2)
                    rs33 = b(3,i1+2)*b(3,i2)
                    s33 = b(3,i1) *b(3,i2)
                    btdb(i1,i2) = btdb(i1,i2) + d1*(r11+s33) + d2*( rs13+sr31) + d3*(r22+r33)
                    btdb(i1+1,i2) = btdb(i1+1,i2) + d2*(r21+rs23) + d3*r12
                    btdb(i1+2,i2) = btdb(i1+2,i2) + d1*rs33+ d2*r31 + d3*(-r13+sr33)
                    btdb(i1, i2+1) = btdb(i1, i2+1) + d2*(r12+sr32) + d3*r21
                    btdb(i1+1,i2+1) = btdb(i1+1,i2+1) + d1*r22 + d3*( r11+r33)
                    btdb(i1+2,i2+1) = btdb(i1+2,i2+1) + d2*r32 - d3* r23
                    btdb(i1, i2+2) = btdb(i1, i2+2) + d1*sr33 + d2* r13 + d3*(-r31+rs33)
                    btdb(i1+1,i2+2) = btdb(i1+1,i2+2) + d2*r23 - d3* r32
                    btdb(i1+2,i2+2) = btdb(i1+2,i2+2) + d1*r33 + d3*(r11+r22+s33-sr31-rs13)
                end do
            end do
        else
            ASSERT(.false.)
        endif
    elseif ((elas_type.eq.2).or.(elas_type.eq.3)) then
!
! ----- Orthotropic/Transverse isotropic elasticity
!
        call btdbpr(b, d, jacob, nbsig, nbinco,&
                    btdb)
    else
        ASSERT(.false.)
    endif
!
end subroutine
