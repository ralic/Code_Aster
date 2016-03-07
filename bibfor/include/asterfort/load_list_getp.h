!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine load_list_getp(phenom      , l_load_user, v_llresu_info, v_llresu_name, v_list_dble,&
                              l_apply_user, i_load     , nb_load      , i_excit      , load_name  ,&
                              load_type   , ligrch     , load_apply_)
        character(len=4), intent(in) :: phenom
        aster_logical, intent(in) :: l_load_user
        character(len=8), pointer, intent(in) :: v_list_dble(:)
        integer, intent(in), pointer :: v_llresu_info(:)
        character(len=24), intent(in), pointer :: v_llresu_name(:)
        integer, intent(in) :: i_load
        integer, intent(in) :: nb_load
        aster_logical, intent(in) :: l_apply_user
        integer, intent(inout) :: i_excit
        character(len=8), intent(out) :: load_name
        character(len=8), intent(out) :: load_type
        character(len=16), optional, intent(out) :: load_apply_
        character(len=19), intent(out) :: ligrch
    end subroutine load_list_getp
end interface
