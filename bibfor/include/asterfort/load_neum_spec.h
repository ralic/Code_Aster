!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine load_neum_spec(load_name    , load_nume  , load_type  , ligrel_calc, i_type_neum,&
                              nb_type_neumz, nb_in_maxi , nb_in_prep , lchin      , lpain      ,&
                              nb_in_add    , load_ligrel, load_option, matr_type  , iden_direct,&
                              name_inputz)
        character(len=8), intent(in) :: load_name
        integer, intent(in) :: load_nume
        character(len=19), intent(in) :: ligrel_calc
        character(len=4), intent(in) :: load_type
        integer, intent(in) :: i_type_neum
        integer, intent(in) :: nb_type_neumz
        integer, intent(in) :: nb_in_maxi
        integer, intent(in) :: nb_in_prep
        character(len=*), intent(inout) :: lpain(nb_in_maxi)
        character(len=*), intent(inout) :: lchin(nb_in_maxi)
        integer, intent(out) :: nb_in_add
        character(len=19), intent(out) :: load_ligrel
        character(len=16), intent(out) :: load_option
        character(len=8), optional, intent(out) :: matr_type
        character(len=*), optional, intent(in) :: iden_direct
        character(len=*), optional, intent(in) :: name_inputz
    end subroutine load_neum_spec
end interface
