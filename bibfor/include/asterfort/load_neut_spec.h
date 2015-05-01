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
    subroutine load_neut_spec(type_ther  , type_calc  , model      , time_curr    , time      ,&
                              load_name  , load_nume  , i_type_neum, nb_type_neumz, nb_in_maxi,&
                              nb_in_prep , lchin      , lpain      , nb_in_add    , lpaout    ,&
                              load_ligrel, load_option,&
                              time_move_)
        character(len=4), intent(in) :: type_ther
        character(len=4), intent(in) :: type_calc
        character(len=24), intent(in) :: model
        real(kind=8), intent(in) :: time_curr
        character(len=24), intent(in) :: time
        character(len=8), intent(in) :: load_name
        integer, intent(in) :: load_nume
        integer, intent(in) :: i_type_neum
        integer, intent(in) :: nb_type_neumz
        integer, intent(in) :: nb_in_maxi
        integer, intent(in) :: nb_in_prep
        character(len=*), intent(inout) :: lpain(nb_in_maxi)
        character(len=*), intent(inout) :: lchin(nb_in_maxi)
        integer, intent(out) :: nb_in_add
        character(len=8), intent(out) :: lpaout
        character(len=19), intent(out) :: load_ligrel
        character(len=16), intent(out) :: load_option
        character(len=24), optional, intent(in) :: time_move_
    end subroutine load_neut_spec
end interface
