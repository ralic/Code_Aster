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
interface
    subroutine nmdata(model    , mesh      , mate      , cara_elem , ds_constitutive,&
                      list_load, solver    , ds_conv   , sddyna    , sdpost         ,&
                      sderro   , ds_energy , sdcriq    , ds_print  , ds_algopara    ,&
                      ds_inout , ds_contact, ds_measure, ds_algorom)
        use NonLin_Datastructure_type
        use Rom_Datastructure_type
        character(len=*), intent(out) :: model
        character(len=*), intent(out) :: mesh
        character(len=*), intent(out) :: mate
        character(len=*), intent(out) :: cara_elem
        type(NL_DS_Constitutive), intent(inout) :: ds_constitutive
        character(len=*), intent(out) :: list_load
        character(len=*), intent(out) :: solver
        type(NL_DS_Conv), intent(inout) :: ds_conv
        character(len=19) :: sddyna
        character(len=19) :: sdpost
        character(len=24) :: sderro
        type(NL_DS_Energy), intent(inout) :: ds_energy
        character(len=24) :: sdcriq
        type(NL_DS_Print), intent(inout) :: ds_print
        type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
        type(NL_DS_InOut), intent(inout) :: ds_inout
        type(NL_DS_Contact), intent(inout) :: ds_contact
        type(NL_DS_Measure), intent(inout) :: ds_measure
        type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
    end subroutine nmdata
end interface
