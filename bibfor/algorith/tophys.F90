subroutine tophys(dplmod, xgene, xphys)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    Convert into the physical basis some data in generalized coordinates
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
!-----------------------------------------------------------------------
    real(kind=8), pointer, intent(in)  :: dplmod(:)
    real(kind=8), pointer, intent(in)  :: xgene(:)
    real(kind=8),          intent(out) :: xphys(:)
!-----------------------------------------------------------------------
    integer :: i, j, nbmode
!-----------------------------------------------------------------------
    nbmode = size(dplmod)/3

    do j = 1, 3
        xphys(j) = 0.d0
    end do
!
    do j = 1, 3
        do i = 1, nbmode
            xphys(j) = xphys(j) + dplmod((i-1)*3+j)*xgene(i)
        end do
    end do
end subroutine
