subroutine char_soli_mat1(ab, ac, matr_inve_1, matr_2)
!
    implicit none
!
#include "asterfort/matinv.h"
#include "asterfort/provec.h"
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
    real(kind=8), intent(in) :: ab(3)
    real(kind=8), intent(in) :: ac(3)
    real(kind=8), intent(out) :: matr_inve_1(3, 3)
    real(kind=8), intent(out) :: matr_2(3, 12)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Computation of matrix for solid movement in 3D - With triangle defined - First part
!
! --------------------------------------------------------------------------------------------------
!
! In  ab : ab segment of triangle abc
! In  ac : ac segment of triangle abc
! Out matr_1
! Out matr_inve_1
! Out matr_2
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: det
    real(kind=8) :: zero
    integer :: i, j
    real(kind=8) :: matr_1(3, 3)
    real(kind=8) :: norm_abc(3), bn(3), cn(3)
!
! --------------------------------------------------------------------------------------------------
!
    zero = 0.d0
!
! - Initializations
!
    do i = 1, 3
        do j = 1, 12
            matr_2(i,j) = zero
        end do
        do j = 1, 3
            matr_inve_1(i,j) = zero
        end do
    end do
!
    call provec(ab, ac, norm_abc)
    call provec(ab, norm_abc, bn)
    call provec(ac, norm_abc, cn)
!
! - Matrix [matr_1] and [matr_1]^-1
!
    matr_1(1,1) = bn(1)
    matr_1(1,2) = bn(2)
    matr_1(1,3) = bn(3)
    matr_1(2,1) = cn(1)
    matr_1(2,2) = cn(2)
    matr_1(2,3) = cn(3)
    matr_1(3,1) = norm_abc(1)
    matr_1(3,2) = norm_abc(2)
    matr_1(3,3) = norm_abc(3)
    call matinv('C', 3, matr_1, matr_inve_1, det)
    if (det .eq. 0.d0) then
        call utmess('F', 'CHARGES2_47')
    endif
!
!
! - Matrix [matr_2]
!
    matr_2(1,1) = -norm_abc(1)
    matr_2(1,2) = -norm_abc(2)
    matr_2(1,3) = -norm_abc(3)
    matr_2(1,4) = norm_abc(1)
    matr_2(1,5) = norm_abc(2)
    matr_2(1,6) = norm_abc(3)
    matr_2(2,1) = -norm_abc(1)
    matr_2(2,2) = -norm_abc(2)
    matr_2(2,3) = -norm_abc(3)
    matr_2(2,7) = norm_abc(1)
    matr_2(2,8) = norm_abc(2)
    matr_2(2,9) = norm_abc(3)
    matr_2(3,1) = -ac(1)
    matr_2(3,2) = -ac(2)
    matr_2(3,3) = -ac(3)
    matr_2(3,4) = ac(1)
    matr_2(3,5) = ac(2)
    matr_2(3,6) = ac(3)
!
end subroutine
