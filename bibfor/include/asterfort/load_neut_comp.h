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
    subroutine load_neut_comp(type_calc, stop_calc , model         , time      , load_name,&
                              load_nume, nb_in_maxi, nb_in_prep    , lpain     , lchin    ,&
                              base     , resu_elem , matr_vect_elem, time_move_, i_load_  )
        character(len=4), intent(in) :: type_calc
        character(len=1), intent(in) :: stop_calc
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: time
        character(len=8), intent(in) :: load_name
        integer, intent(in) :: load_nume
        integer, intent(in) :: nb_in_maxi
        integer, intent(in) :: nb_in_prep
        character(len=*), intent(inout) :: lpain(nb_in_maxi)
        character(len=*), intent(inout) :: lchin(nb_in_maxi)
        character(len=19), intent(inout) :: resu_elem
        character(len=19), intent(in) :: matr_vect_elem
        character(len=1), intent(in) :: base
        character(len=24), optional, intent(in) :: time_move_
        integer, optional, intent(in) :: i_load_
    end subroutine load_neut_comp
end interface
