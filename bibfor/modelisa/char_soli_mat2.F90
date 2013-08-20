subroutine char_soli_mat2(am, matr_inve_1, matr_2, matr_4, matr_3,&
                          matr_5)
!
    implicit none
!
#include "asterfort/pmat.h"
#include "asterfort/pmppr.h"
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
    real(kind=8), intent(in) :: matr_inve_1(3, 3)
    real(kind=8), intent(in) :: matr_2(3, 12)
    real(kind=8), intent(in) :: matr_4(3, 12)
    real(kind=8), intent(inout) :: matr_3(3, 3)
    real(kind=8), intent(inout) :: matr_5(3, 12)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Computation of matrix for solid movement in 3D - With triangle defined - Second part
!
! --------------------------------------------------------------------------------------------------
!
! In  am : am segment of triangle abc
! In  ac : ac segment of triangle abc
! In  matr_inve_1
! In  matr_2
! In  matr_4
! I/O matr_3
! I/O matr_5
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: matr_6(3, 3)
!
! --------------------------------------------------------------------------------------------------
!
    do i = 1, 3
        do j = 1, 3
            matr_6(i,j) = 0.d0
            matr_3(i,j) = 0.d0
        end do
    end do
!
! - Matrix [matr_3]
!
    matr_3(1,2) = -am(3)
    matr_3(1,3) = am(2)
    matr_3(2,1) = am(3)
    matr_3(2,3) = -am(1)
    matr_3(3,1) = -am(2)
    matr_3(3,2) = am(1)
!
! - [matr_6] = [matr_3] x [matr_1]^-1
!
    call pmat(3, matr_3, matr_inve_1, matr_6)
!
! - [matr_5] = [matr_3] x [matr_1]^-1 x [matr_2] = [matr_6] x [matr_2]
!
    call pmppr(matr_6, 3, 3, 1, matr_2,&
               3, 12, 1, matr_5, 3,&
               12)
!
! - [matr_5] = [matr_4] + [matr_3] x [matr_1]^-1 x [matr_2]
!
    do i = 1, 3
        do j = 1, 12
            matr_5(i,j) = matr_5(i,j) + matr_4(i,j)
        enddo
    enddo
!
end subroutine
