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
    subroutine ccchuc_chamel(field_in_s, field_out_s, nb_elem, nb_cmp, type_comp, &
                             crit, nb_form, name_form, name_gd, nb_cmp_resu, &
                             work_out_val, work_out_ele, nb_elem_out, ichk)
        character(len=19), intent(in) :: field_in_s
        character(len=19), intent(in) :: field_out_s
        integer, intent(in) :: nb_elem
        integer, intent(in) :: nb_cmp
        character(len=16), intent(in) :: type_comp
        character(len=16), intent(in) :: crit
        integer, intent(in) :: nb_form
        character(len=8), intent(in) :: name_form(nb_form)
        character(len=8), intent(in) :: name_gd
        integer, intent(in) :: nb_cmp_resu
        character(len=24), intent(in) :: work_out_val
        character(len=24), intent(in) :: work_out_ele
        integer, intent(out) :: ichk
        integer, intent(out) :: nb_elem_out
    end subroutine ccchuc_chamel
end interface
