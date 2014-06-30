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
    subroutine drz03d_tria(dist_mini, nb_node, list_node, coor_node, numnoe_a, &
                           numnoe_b, numnoe_c, ab, ac, l_trian)
        real(kind=8), intent(in) :: dist_mini
        integer, intent(in) :: nb_node
        integer, intent(in) :: list_node(*)
        real(kind=8), intent(in) :: coor_node(*)
        integer, intent(in) :: numnoe_a
        integer, intent(out) :: numnoe_b
        integer, intent(out) :: numnoe_c
        real(kind=8), intent(out) :: ab(3)
        real(kind=8), intent(out) :: ac(3)
        logical(kind=1), intent(out) :: l_trian
    end subroutine drz03d_tria
end interface
