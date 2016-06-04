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
    subroutine mmelem_data_l(set_cont_indx_ , l_axi_         , model_ndim_,&
                             elem_1_        , elem_2_        ,&
                             nb_cont_type_  , nb_node_elem_  ,&
                             cont_geom_nume_, cont_geom_name_,&
                             cont_elem_name_, frot_elem_name_,&
                             get_cont_indx_ )
        integer, intent(in), optional :: set_cont_indx_
        aster_logical, intent(in), optional :: l_axi_
        integer, intent(in), optional :: model_ndim_
        character(len=8), intent(in), optional :: elem_1_
        character(len=8), intent(in), optional :: elem_2_
        integer, intent(out), optional :: nb_node_elem_
        integer, intent(out), optional :: cont_geom_nume_
        integer, intent(out), optional :: nb_cont_type_
        character(len=8), intent(out), optional :: cont_elem_name_
        character(len=8), intent(out), optional :: frot_elem_name_
        character(len=8), intent(out), optional :: cont_geom_name_
        integer, intent(out), optional :: get_cont_indx_
    end subroutine mmelem_data_l
end interface
