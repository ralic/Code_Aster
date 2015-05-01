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
    subroutine nmextn(field_disc, type_extr_cmp, type_extr_elem, type_extr, nb_node,&
                      nb_elem   , nb_cmp       , nb_poin       , nb_spoi  , nb_extr)
        character(len=4), intent(in) :: field_disc
        integer, intent(in) :: nb_node
        integer, intent(in) :: nb_elem
        integer, intent(in) :: nb_poin
        integer, intent(in) :: nb_spoi
        integer, intent(in) :: nb_cmp
        character(len=8), intent(in) :: type_extr
        character(len=8), intent(in) :: type_extr_elem
        character(len=8), intent(in) :: type_extr_cmp
        integer, intent(out) :: nb_extr
    end subroutine nmextn
end interface
