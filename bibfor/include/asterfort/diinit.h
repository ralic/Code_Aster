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
    subroutine diinit(mesh_         , model_     , ds_inout, mate       , cara_elem,&
                      list_func_acti, sddyna     , ds_conv , ds_algopara, solver   ,&
                      ds_contact    , sddisc)
        use NonLin_Datastructure_type
        character(len=*), intent(in) :: mesh_
        character(len=*), intent(in) :: model_
        character(len=19), intent(in) :: sddisc
        character(len=19), intent(in) :: sddyna
        character(len=24), intent(in) :: cara_elem
        character(len=24), intent(in) :: mate
        type(NL_DS_Conv), intent(in) :: ds_conv
        type(NL_DS_AlgoPara), intent(in) :: ds_algopara
        type(NL_DS_InOut), intent(in) :: ds_inout
        character(len=19), intent(in) :: solver
        type(NL_DS_Contact), intent(in) :: ds_contact
        integer, intent(in) :: list_func_acti(*)
    end subroutine diinit
end interface
