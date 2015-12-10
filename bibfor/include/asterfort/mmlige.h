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
    subroutine mmlige(mesh      , ds_contact, v_list_elem, nb_cont_type, v_cnt_cont,&
                      v_cnt_frot, nt_node   , nb_grel    , nb_cont_elem)
        use NonLin_Datastructure_type
        character(len=8), intent(in) :: mesh
        type(NL_DS_Contact), intent(in) :: ds_contact
        integer, intent(out), pointer :: v_list_elem(:)
        integer, intent(out) :: nb_cont_type
        integer, intent(out), pointer :: v_cnt_cont(:)
        integer, intent(out), pointer :: v_cnt_frot(:)
        integer, intent(out) :: nt_node
        integer, intent(out) :: nb_grel
        integer, intent(out) :: nb_cont_elem
    end subroutine mmlige
end interface
