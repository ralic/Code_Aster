subroutine hgksca(para, xe, gam, xk, kstab)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
!        ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003         !
!-----------------------------------------------------------------------
    implicit none
!
! HourGlass K Stabilization matrix from Combescure and Abed-Meraim
! Evaluation of K stabilization matrix to prevent hourglass in SHB8 element
!
!
! Evaluation of K stabilization matrix to prevent hourglass in SHB8 element
! in corotational frame.
!
!
! IN  para     material parameters E and nu
! IN  xe       element node coordinates in corotational frame
! IN  gam      gamma vectors from assumed strain method
! OUT xk       K stabilization matrix material-dependent components
! OUT kstab    K stabilization matrix
!
#include "asterfort/r8inir.h"
!
    real(kind=8), intent(in) :: para(2)
    real(kind=8), intent(in) :: xe(24)
    real(kind=8), intent(in) :: gam(4,8)
    real(kind=8), intent(out) :: xk(3,2)
    real(kind=8), optional, intent(out) :: kstab(24, 24)
!
    real(kind=8) :: xxvb(3), hij(6)
    real(kind=8) :: youngt, nu
    real(kind=8) :: lambda, mu
    real(kind=8) :: uns3
    real(kind=8) :: k11(8, 8), k22(8, 8), k33(8, 8), k12(8, 8)
    real(kind=8) :: k21(8, 8), k13(8, 8), k23(8, 8), k31(8, 8), k32(8, 8)
    integer :: i, j
!
! ......................................................................
!
    uns3 = 1.d0/3.d0
!
!   K stabilization matrix assembly from (8,8) K11, K22 & K33 matrices
!
!
!   Evaluate xxvb = x * vb
!
    xxvb(1) = -xe(1) + xe(4) + xe(7) - xe(10) - xe(13) + xe(16) + xe(19) - xe(22)
    xxvb(2) = -xe(2) - xe(5) + xe(8) + xe(11) - xe(14) - xe(17) + xe(20) + xe(23)
    xxvb(3) = -xe(3) - xe(6) - xe(9) - xe(12) + xe(15) + xe(18) + xe(21) + xe(24)
!
!   Evaluate stress-strain relations
!
    hij(1) = uns3*xxvb(2)*xxvb(3)/xxvb(1)
    hij(2) = uns3*xxvb(1)*xxvb(3)/xxvb(2)
    hij(3) = uns3*xxvb(2)*xxvb(1)/xxvb(3)
    hij(4) = uns3*xxvb(3)
    hij(5) = uns3*xxvb(1)
    hij(6) = uns3*xxvb(2)
!
!   Retrieve material parameters
!
    youngt = para(1)
    nu = para(2)
    lambda = youngt*nu/(1-nu*nu)
    mu = 0.5d0*youngt/(1+nu)
!
    xk(1,1) = (lambda+2.d0*mu)*hij(1)
    xk(1,2) = uns3*(lambda+2.d0*mu)*hij(1)
    xk(2,1) = (lambda+2.d0*mu)*hij(2)
    xk(2,2) = uns3*(lambda+2.d0*mu)*hij(2)
    xk(3,1) = 0.d0
    xk(3,2) = mu*hij(1)*uns3
!
    if (present(kstab)) then
!       Evaluate K11, K22 & K33 (8,8) matrices
!
        call r8inir(64, 0.d0, k12, 1)
        call r8inir(64, 0.d0, k13, 1)
        call r8inir(64, 0.d0, k21, 1)
        call r8inir(64, 0.d0, k23, 1)
        call r8inir(64, 0.d0, k31, 1)
        call r8inir(64, 0.d0, k32, 1)
!
        do 20 j = 1, 8
            do 10 i = 1, 8
                k11(i,j) = xk(1,1)*gam(3,i)*gam(3,j) + xk(1,2)*gam(4,i)*gam(4,j)
                k22(i,j) = xk(2,1)*gam(3,i)*gam(3,j) + xk(2,2)*gam(4,i)*gam(4,j)
                k33(i,j) = xk(3,1)*gam(3,i)*gam(3,j) + xk(3,2)*gam(4,i)*gam(4,j)
10          continue
20      continue
!
!       K stabilization matrix assembly
!
        call r8inir(576, 0.d0, kstab, 1)
        do 40 i = 1, 8
            do 30 j = 1, 8
                kstab((i-1)*3+1, (j-1)*3+1) = k11(i,j)
                kstab((i-1)*3+1, (j-1)*3+2) = k12(i,j)
                kstab((i-1)*3+1, (j-1)*3+3) = k13(i,j)
                kstab((i-1)*3+2, (j-1)*3+1) = k21(i,j)
                kstab((i-1)*3+2, (j-1)*3+2) = k22(i,j)
                kstab((i-1)*3+2, (j-1)*3+3) = k23(i,j)
                kstab((i-1)*3+3, (j-1)*3+1) = k31(i,j)
                kstab((i-1)*3+3, (j-1)*3+2) = k32(i,j)
                kstab((i-1)*3+3, (j-1)*3+3) = k33(i,j)
30          continue
40      continue
    endif
!
end subroutine
