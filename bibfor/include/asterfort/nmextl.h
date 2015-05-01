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
    subroutine nmextl(mesh      , model    , keyw_fact, i_keyw_fact, field_type,&
                      field_disc, list_node, list_elem, nb_node    , nb_elem   ,&
                      type_extr)
        character(len=*), intent(in) :: mesh
        character(len=*), intent(in) :: model
        character(len=16), intent(in) :: keyw_fact
        integer, intent(in) :: i_keyw_fact
        character(len=4), intent(in) :: field_disc
        character(len=24), intent(in) :: field_type
        integer, intent(out) :: nb_node
        integer, intent(out) :: nb_elem
        character(len=24), intent(in) :: list_node
        character(len=24), intent(in) :: list_elem
        character(len=8), intent(out) :: type_extr
    end subroutine nmextl
end interface
