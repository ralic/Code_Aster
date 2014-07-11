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
    subroutine get_equa_info(nume_ddlz     , i_equa    , type_equa , nume_nodez  , nume_cmpz,&
                             nume_cmp_lagrz, nume_subsz, nume_linkz, nb_node_lagr, list_node_lagr,&
                             ligrelz)
        character(len=*), intent(in) :: nume_ddlz
        integer, intent(in) :: i_equa
        character(len=*), intent(out) :: type_equa
        integer, optional, intent(out) :: nume_nodez
        integer, optional, intent(out) :: nume_cmpz
        integer, optional, intent(out) :: nume_subsz
        integer, optional, intent(out) :: nume_linkz
        integer, optional, intent(out) :: nume_cmp_lagrz
        integer, optional, intent(out) :: nb_node_lagr
        integer, optional, pointer, intent(out) :: list_node_lagr(:)
        character(len=*), optional, intent(out) :: ligrelz
    end subroutine get_equa_info
end interface
