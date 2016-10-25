!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine gtvois(v_connex  , v_connex_lcum, list_elem, nb_elem   , elem_nume, elem_code,&
                      v_conx_inv, v_inv_lcum   , nb_neigh , list_neigh)
        integer, pointer, intent(in) :: v_connex(:)
        integer, pointer, intent(in) :: v_connex_lcum(:)
        integer, pointer, intent(in) :: v_conx_inv(:)
        integer, pointer, intent(in) :: v_inv_lcum(:)
        integer, intent(in) :: nb_elem
        integer, intent(in) :: list_elem(nb_elem)
        integer, intent(in) :: elem_nume
        character(len=8), intent(in) :: elem_code
        integer, intent(in) :: nb_neigh
        integer, intent(out) :: list_neigh(4)
    end subroutine gtvois
end interface
