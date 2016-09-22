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
    subroutine nmfonc(ds_conv  , ds_algopara    , solver   , model     , ds_contact    ,&
                      list_load, sdnume         , sddyna   , sdcriq    , mate          ,&
                      ds_inout , ds_constitutive, ds_energy, ds_algorom, list_func_acti)
        use NonLin_Datastructure_type
        use Rom_Datastructure_type
        type(NL_DS_Conv), intent(in) :: ds_conv
        type(NL_DS_AlgoPara), intent(in) :: ds_algopara
        character(len=19), intent(in) :: solver
        character(len=24), intent(in) :: model
        type(NL_DS_Contact), intent(in) :: ds_contact
        character(len=19), intent(in) :: list_load
        character(len=19), intent(in) :: sdnume
        character(len=19), intent(in) :: sddyna
        character(len=24), intent(in) :: sdcriq
        character(len=24), intent(in) :: mate
        type(NL_DS_InOut), intent(in) :: ds_inout
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        type(NL_DS_Energy), intent(in) :: ds_energy
        type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
        integer, intent(inout) :: list_func_acti(*)
    end subroutine nmfonc
end interface
