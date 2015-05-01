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
    subroutine equa_print(mesh         , i_equa   , type_equa, name_node   , name_cmp,&
                          name_cmp_lagr, name_subs, nume_link, nb_node_lagr, list_node_lagr,&
                          ligrel)
        character(len=8), intent(in) :: mesh
        character(len=1), intent(in) :: type_equa
        integer, intent(in) :: i_equa
        character(len=8), intent(in) :: name_node
        character(len=8), intent(in) :: name_cmp
        character(len=8), intent(in) :: name_cmp_lagr
        character(len=8), intent(in) :: name_subs
        integer, intent(in) :: nume_link
        integer, intent(in) :: nb_node_lagr
        integer, pointer, intent(in) :: list_node_lagr(:)
        character(len=8), intent(in) :: ligrel
    end subroutine equa_print
end interface
