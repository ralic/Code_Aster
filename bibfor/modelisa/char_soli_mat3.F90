subroutine char_soli_mat3(am, coef, matr_6, matr_8)
!
    implicit none
!
#include "asterfort/pmppr.h"
#include "asterfort/provec.h"
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
    real(kind=8), intent(in) :: am(3)
    real(kind=8), intent(in) :: coef
    real(kind=8), intent(in) :: matr_6(3, 9)
    real(kind=8), intent(out) :: matr_8(3, 9)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Computation of matrix for solid movement in 3D - With no triangle defined
!
! --------------------------------------------------------------------------------------------------
!
! In  am : ab segment of triangle abc
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: un, zero
    real(kind=8) :: matr_7(3, 3)
    integer :: i, j
!
! --------------------------------------------------------------------------------------------------
!
    un = 1.d0
    zero = 0.d0
    do i = 1, 3
        do j = 1, 3
            matr_7(i,j) = zero
        enddo
        do j = 1, 9
            matr_8(i,j) = zero
        enddo
    end do
!
! - Matrix matr_7
!
    matr_7(1,2) = -am(3)
    matr_7(1,3) = am(2)
    matr_7(2,1) = am(3)
    matr_7(2,3) = -am(1)
    matr_7(3,1) = -am(2)
    matr_7(3,2) = am(1)
!
! - [matr_8] = [matr_7] x [matr_6]
!
    call pmppr(matr_7, 3, 3, 1, matr_6,&
               3, 9, 1, matr_8, 3,&
               9)
!
! - [matr_8] = coef * [matr_7]
!
    do i = 1, 3
        do j = 1, 9
            matr_8(i,j) = coef*matr_8(i,j)
        enddo
    enddo
!
    matr_8(1,1) = matr_8(1,1) - un
    matr_8(2,2) = matr_8(2,2) - un
    matr_8(3,3) = matr_8(3,3) - un
    matr_8(1,7) = matr_8(1,7) + un
    matr_8(2,8) = matr_8(2,8) + un
    matr_8(3,9) = matr_8(3,9) + un
!
end subroutine
