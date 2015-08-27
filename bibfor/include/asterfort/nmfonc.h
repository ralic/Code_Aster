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
    subroutine nmfonc(ds_conv    , algo_para     , algo_meth, solver , model ,&
                      sdcont_defi, list_load     , l_cont   , l_unil , sdnume,&
                      sddyna     , sdcriq        , mate     , compor_, result,&
                      comp_para  , list_func_acti)
        use NonLin_Datastructure_type
        type(NL_DS_Conv), intent(in) :: ds_conv
        real(kind=8), intent(in) :: algo_para(*)
        character(len=16), intent(in) :: algo_meth(*)
        character(len=19), intent(in) :: solver
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: sdcont_defi
        character(len=19), intent(in) :: list_load
        aster_logical, intent(in) :: l_cont
        aster_logical, intent(in) :: l_unil
        character(len=19), intent(in) :: sdnume
        character(len=19), intent(in) :: sddyna
        character(len=24), intent(in) :: sdcriq
        character(len=24), intent(in) :: mate
        character(len=*), intent(in) :: compor_
        character(len=8), intent(in) :: result
        character(len=24), intent(in) :: comp_para
        integer, intent(inout) :: list_func_acti(*)
    end subroutine nmfonc
end interface
