subroutine mm_cycl_laugf(pres, dist, coef_augm, lagr_norm)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    real(kind=8), intent(in) :: pres(3)
    real(kind=8), intent(in) :: dist(3)
    real(kind=8), intent(in) :: coef_augm
    real(kind=8), intent(out) :: lagr_norm
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Augmented lagrangian (vectorial version)
!
! --------------------------------------------------------------------------------------------------
!
! In  pres      : pressure
! In  dist      : distance
! In  coef_augm : augmented coefficient
! Out lagr_norm : norm of augmented lagrangian
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: lagr_augm(3)
    integer :: idim
!
! --------------------------------------------------------------------------------------------------
!
    lagr_augm(1) = pres(1) + coef_augm *dist(1)
    lagr_augm(2) = pres(2) + coef_augm *dist(2)
    lagr_augm(3) = pres(3) + coef_augm *dist(3)
    lagr_norm = 0.d0
    do idim = 1, 3
        lagr_norm = lagr_augm(idim)*lagr_augm(idim) + lagr_norm
    end do
    lagr_norm = sqrt(lagr_norm)
end subroutine
