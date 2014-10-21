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
    subroutine nmmein(mesh        , model   , crack      , nb_dim     , list_node,&
                      nb_node     , list_cmp, list_node_1, list_node_2, cmp_name ,&
                      nb_node_sele)
        integer, intent(in) :: nb_dim
        character(len=8), intent(in) :: mesh
        character(len=8), intent(in) :: model
        character(len=8), intent(in)  :: crack
        integer, intent(in) :: nb_node
        character(len=24), intent(in) :: list_node
        character(len=24), intent(in) :: list_cmp
        character(len=24), intent(in) :: list_node_1
        character(len=24), intent(in) :: list_node_2
        character(len=8), intent(out) :: cmp_name
        integer, intent(out) :: nb_node_sele
    end subroutine nmmein
end interface
