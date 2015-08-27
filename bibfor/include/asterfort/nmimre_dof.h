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
    subroutine nmimre_dof(nume_dof , ds_conv  , vale_rela, vale_maxi     , vale_refe     ,&
                          vale_comp, vale_frot, vale_geom, ieq_rela      , ieq_maxi      ,&
                          ieq_refe , noddlm   , ieq_comp , name_node_frot, name_node_geom)
        use NonLin_Datastructure_type
        character(len=24), intent(in) :: nume_dof
        type(NL_DS_Conv), intent(inout) :: ds_conv
        integer, intent(in) :: ieq_rela
        integer, intent(in) :: ieq_maxi
        integer, intent(in) :: ieq_refe
        integer, intent(in) :: ieq_comp
        real(kind=8), intent(in) :: vale_rela
        real(kind=8), intent(in) :: vale_maxi
        real(kind=8), intent(in) :: vale_refe
        real(kind=8), intent(in) :: vale_comp
        real(kind=8), intent(in) :: vale_frot
        real(kind=8), intent(in) :: vale_geom
        character(len=8), intent(in) :: noddlm
        character(len=16), intent(in) :: name_node_frot
        character(len=16), intent(in) :: name_node_geom
    end subroutine nmimre_dof
end interface
