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
    subroutine mmelem_data_l(l_axi_         ,&
                             typg_slav_name_, typg_mast_name_,&
                             typf_slav_name_,&
                             nb_cont_type_  , nb_node_elem_  ,&
                             typg_cont_nume_,&
                             typf_cont_nume_,&
                             set_elem_indx_ , get_elem_indx_)
        aster_logical, intent(in), optional :: l_axi_        
        character(len=8), intent(in), optional :: typg_slav_name_
        character(len=8), intent(in), optional :: typg_mast_name_
        character(len=16), intent(in), optional :: typf_slav_name_
        integer, intent(out), optional :: nb_cont_type_
        integer, intent(out), optional :: nb_node_elem_
        integer, intent(out), optional :: typg_cont_nume_
        integer, intent(out), optional :: typf_cont_nume_
        integer, intent(in), optional :: set_elem_indx_
        integer, intent(out), optional :: get_elem_indx_
    end subroutine mmelem_data_l
end interface
