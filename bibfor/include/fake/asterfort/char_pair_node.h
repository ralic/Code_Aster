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
    subroutine char_pair_node(mesh, cent, angl_naut, tran, nb_node, &
                              list_node_i1, list_node_i2, list_node_o1, list_node_o2, i_error)
        character(len=8), intent(in) :: mesh
        real(kind=8), intent(in) :: cent(3)
        real(kind=8), intent(in) :: angl_naut(3)
        real(kind=8), intent(in) :: tran(3)
        integer, intent(in) :: nb_node
        character(len=24), intent(in) :: list_node_i1
        character(len=24), intent(in) :: list_node_i2
        character(len=24), intent(in) :: list_node_o1
        character(len=24), intent(in) :: list_node_o2
        integer, intent(out) :: i_error
    end subroutine char_pair_node
end interface
