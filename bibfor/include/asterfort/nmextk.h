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
    subroutine nmextk(mesh     , keyw_fact , i_keyw_fact, field        , field_type,&
                     field_s  , field_disc, list_node  , list_elem    , list_poin ,&
                     list_spoi, nb_node   , nb_elem    , nb_poin      , nb_spoi   ,&
                     compor   , list_cmp  , list_vari  , nb_cmp     , type_sele_cmp)
        character(len=8), intent(in) :: mesh
        character(len=16), intent(in) :: keyw_fact
        integer, intent(in) :: i_keyw_fact
        character(len=19), intent(in) :: field
        character(len=24), intent(in) :: field_type
        character(len=24), intent(in) :: field_s
        character(len=4), intent(in) :: field_disc
        integer, intent(in) :: nb_node
        integer, intent(in) :: nb_elem
        integer, intent(in) :: nb_poin
        integer, intent(in) :: nb_spoi
        character(len=24), intent(in) :: list_node
        character(len=24), intent(in) :: list_elem
        character(len=24), intent(in) :: list_poin
        character(len=24), intent(in) :: list_spoi
        character(len=19), optional, intent(in) :: compor
        character(len=24), intent(in) :: list_cmp
        character(len=24), intent(in) :: list_vari
        integer, intent(out) :: nb_cmp
        character(len=8), intent(out) :: type_sele_cmp
    end subroutine nmextk
end interface 
