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
    subroutine vrcomp_chck_cmp(mesh, nb_elem,&
                               compor_curr, compor_curr_r, compor_prev_r, &
                               vari_r, comp_comb_2,&
                               ligrel_curr, ligrel_prev,&
                               no_same_spg, no_same_cmp, l_modif_vari)
        character(len=8), intent(in) :: mesh
        integer, intent(in) :: nb_elem
        character(len=*), intent(in)  :: compor_curr
        character(len=19), intent(in) :: compor_curr_r
        character(len=19), intent(in) :: compor_prev_r
        character(len=19), intent(in) :: vari_r
        character(len=48), intent(in) :: comp_comb_2
        character(len=19), intent(in) :: ligrel_curr
        character(len=19), intent(in) :: ligrel_prev
        logical(kind=1), intent(out) :: no_same_spg
        logical(kind=1), intent(out) :: no_same_cmp
        logical(kind=1), intent(out) :: l_modif_vari
    end subroutine vrcomp_chck_cmp
end interface
