subroutine togene(dplmod, fphys, fgene, coef)
    implicit none
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    Convert into the modal basis a force defined in physical coordinates
!
!    Attention : cumulated fgene, fgene needs thus to be initialized
!                before calling this routine
!-----------------------------------------------------------------------
!
#include "jeveux.h"
!-----------------------------------------------------------------------
    real(kind=8), pointer , intent(in)  :: dplmod(:)
    real(kind=8),           intent(in)  :: fphys(:)
    real(kind=8),           intent(out) :: fgene(:)
    real(kind=8), optional, intent(in)  :: coef
!-----------------------------------------------------------------------
    integer :: i, j, nbmode
    real(kind=8) :: coef_m
!-----------------------------------------------------------------------
    coef_m = 1.d0
    if (present(coef)) coef_m = coef

    nbmode = size(dplmod)/3

    do j = 1, 3
        do i = 1, nbmode
            fgene(i) = fgene(i) + coef_m*dplmod((i-1)*3+j)*fphys(j)
        end do
    end do

end subroutine
