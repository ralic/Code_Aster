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
    subroutine lireco(keywf         , mesh          , model         , i_zone      , list_elem_slav,&
                      list_elem_mast, list_node_slav, list_node_mast, nb_elem_slav, nb_node_slav  ,&
                      nb_elem_mast  , nb_node_mast)
        character(len=8), intent(in) :: mesh
        character(len=8), intent(in) :: model
        character(len=16), intent(in) :: keywf
        integer, intent(in) :: i_zone
        character(len=24), intent(in) :: list_elem_slav
        character(len=24), intent(in) :: list_elem_mast
        character(len=24), intent(in) :: list_node_slav
        character(len=24), intent(in) :: list_node_mast
        integer, intent(out) :: nb_elem_slav
        integer, intent(out) :: nb_node_slav
        integer, intent(out) :: nb_elem_mast
        integer, intent(out) :: nb_node_mast
    end subroutine lireco
end interface
