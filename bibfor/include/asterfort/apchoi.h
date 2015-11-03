!
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
!
interface
    subroutine apchoi(dist        , dist_mini, elem_indx, elem_indx_mini, tau1     ,&
                      tau1_mini   , tau2     , tau2_mini, ksi1          , ksi1_mini,&
                      ksi2        , ksi2_mini, proj_stat, proj_stat_mini, vect_pm  ,&
                      vect_pm_mini)
        integer, intent(in) :: proj_stat
        integer, intent(inout) :: proj_stat_mini
        real(kind=8), intent(in) :: dist
        real(kind=8), intent(inout) :: dist_mini
        integer, intent(in) :: elem_indx
        integer, intent(inout) :: elem_indx_mini
        real(kind=8), intent(in) :: tau1(3)
        real(kind=8), intent(inout) :: tau1_mini(3)
        real(kind=8), intent(in) :: tau2(3)
        real(kind=8), intent(inout) :: tau2_mini(3)
        real(kind=8), intent(in) :: vect_pm(3)
        real(kind=8), intent(inout) :: vect_pm_mini(3)
        real(kind=8), intent(in) :: ksi1
        real(kind=8), intent(inout) :: ksi1_mini
        real(kind=8), intent(in) :: ksi2
        real(kind=8), intent(inout) :: ksi2_mini
    end subroutine apchoi
end interface
