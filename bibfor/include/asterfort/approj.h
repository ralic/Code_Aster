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
#include "asterf_types.h"
!
interface
    subroutine approj(mesh          , newgeo        , sdcont_defi , node_mast_indx, l_pair_dire,&
                      pair_vect     , iter_maxi     , epsi_maxi   , tole_proj_ext , poin_coor  ,&
                      elem_mast_mini, proj_stat_mini, ksi1_mini   , ksi2_mini     , tau1_mini  ,&
                      tau2_mini     , dist_mini     , vect_pm_mini)
        character(len=8), intent(in) :: mesh
        character(len=19), intent(in) :: newgeo
        character(len=24), intent(in) :: sdcont_defi
        integer, intent(in) :: node_mast_indx
        aster_logical, intent(in) :: l_pair_dire
        real(kind=8), intent(in) :: pair_vect(3)
        integer, intent(in) :: iter_maxi
        real(kind=8), intent(in) :: epsi_maxi
        real(kind=8), intent(in) :: tole_proj_ext 
        real(kind=8), intent(in) :: poin_coor(3)
        real(kind=8), intent(out) :: tau1_mini(3)
        real(kind=8), intent(out) :: tau2_mini(3)
        real(kind=8), intent(out) :: vect_pm_mini(3)
        real(kind=8), intent(out) :: ksi1_mini
        real(kind=8), intent(out) :: ksi2_mini
        real(kind=8), intent(out) :: dist_mini
        integer, intent(out) :: proj_stat_mini
        integer, intent(out) :: elem_mast_mini
    end subroutine approj
end interface
