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
#include "asterf_types.h"
!
interface
    subroutine vrcomp_chck_rela(mesh, nb_elem,&
                                compor_curr_r, compor_prev_r,&
                                ligrel_curr, ligrel_prev,&
                                comp_comb_1, comp_comb_2,&
                                no_same_pg, no_same_rela, l_modif_vari)
        character(len=8), intent(in) :: mesh
        integer, intent(in) :: nb_elem
        character(len=19), intent(in) :: compor_curr_r
        character(len=19), intent(in) :: compor_prev_r
        character(len=19), intent(in) :: ligrel_curr
        character(len=19), intent(in) :: ligrel_prev
        character(len=48), intent(in) :: comp_comb_1
        character(len=48), intent(in) :: comp_comb_2
        aster_logical, intent(out) :: no_same_pg
        aster_logical, intent(out) :: no_same_rela
        aster_logical, intent(out) :: l_modif_vari
    end subroutine vrcomp_chck_rela
end interface
