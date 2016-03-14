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
#include "asterf_types.h"
!
interface
    subroutine cfcoem(ds_contact, l_frot      , node_slav_indx, i_cont_link,&
                      nb_dof_tot, nb_node_mast, nods_mast_indx, dof_indx   ,&
                      coef_cont , coef_fric_x , coef_fric_y)
        use NonLin_Datastructure_type
        type(NL_DS_Contact), intent(in) :: ds_contact
        integer, intent(in) :: node_slav_indx
        integer, intent(in) :: i_cont_link
        integer, intent(in) :: nb_dof_tot
        integer, intent(in) :: nb_node_mast
        integer, intent(in) :: nods_mast_indx(9)
        integer, intent(in) :: dof_indx(30)
        real(kind=8), intent(in) :: coef_cont(30)
        real(kind=8), intent(in) :: coef_fric_x(30)
        real(kind=8), intent(in) :: coef_fric_y(30)
        aster_logical, intent(in) :: l_frot
    end subroutine cfcoem
end interface
