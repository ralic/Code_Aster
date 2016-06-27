subroutine mmelem_data_c(l_axi_         , model_ndim_    ,&
                         typg_slav_name_, typg_mast_name_,&
                         nb_cont_type_  , nb_node_elem_  ,&
                         typg_cont_nume_, &
                         typf_cont_nume_, &
                         typf_frot_nume_, &
                         set_elem_indx_ , get_elem_indx_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    aster_logical, intent(in), optional :: l_axi_
    integer, intent(in), optional :: model_ndim_
    character(len=8), intent(in), optional :: typg_slav_name_
    character(len=8), intent(in), optional :: typg_mast_name_
    integer, intent(out), optional :: nb_cont_type_
    integer, intent(out), optional :: nb_node_elem_
    integer, intent(out), optional :: typg_cont_nume_
    integer, intent(out), optional :: typf_cont_nume_
    integer, intent(out), optional :: typf_frot_nume_
    integer, intent(in), optional :: set_elem_indx_
    integer, intent(out), optional :: get_elem_indx_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Define contact/friction elements
!
! --------------------------------------------------------------------------------------------------
!
! In  l_axi            : .true. for axi-symetric model
! In  model_ndim       : size of model
! In  typg_slav_name   : name of geometric type of slave element
! In  typg_mast_name   : name of geometric type of master element
! Out nb_cont_type     : total number of contact elements defined
! Out nb_node_elem     : number of nodes of contact/friction element
! Out typg_cont_nume   : index of geometric type of contact/friction element
! Out typf_cont_nume   : index of FE type of contact element
! Out typf_frot_nume   : index of FE type of friction element
! In  set_elem_indx    : index to select contact/friction element (set)
! Out get_elem_indx    : index to select contact/friction element (get)
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cont_geom = 40
    integer, parameter :: nb_cont_solv = 40
!
! - Name of geometry type for slave element
!
    character(len=8), parameter, dimension(nb_cont_geom) :: lypg_slav_name = (/&
        'SEG2    ','SEG3    ','SEG2    ','SEG3    ','TRIA3   ',&
        'TRIA3   ','TRIA6   ','TRIA6   ','QUAD4   ','QUAD4   ',&
        'QUAD8   ','QUAD8   ','QUAD4   ','TRIA3   ','TRIA6   ',&
        'QUAD4   ','TRIA6   ','QUAD8   ','TRIA6   ','QUAD9   ',&
        'QUAD8   ','TRIA3   ','QUAD8   ','QUAD9   ','QUAD9   ',&
        'QUAD4   ','QUAD9   ','TRIA3   ','QUAD9   ','SEG2    ',&
        'SEG2    ','SEG2    ','SEG2    ','SEG2    ','SEG2    ',&
        'SEG3    ','SEG3    ','SEG3    ','SEG3    ','SEG3    '/)
!
! - Name of geometry type for master element
!
    character(len=8), parameter, dimension(nb_cont_geom) :: lypg_mast_name = (/&
        'SEG2    ','SEG3    ','SEG3    ','SEG2    ','TRIA3   ',&
        'TRIA6   ','TRIA3   ','TRIA6   ','QUAD4   ','QUAD8   ',&
        'QUAD4   ','QUAD8   ','TRIA3   ','QUAD4   ','QUAD4   ',&
        'TRIA6   ','QUAD8   ','TRIA6   ','QUAD9   ','TRIA6   ',&
        'TRIA3   ','QUAD8   ','QUAD9   ','QUAD8   ','QUAD4   ',&
        'QUAD9   ','TRIA3   ','QUAD9   ','QUAD9   ','SEG2    ',&
        'TRIA3   ','TRIA6   ','QUAD4   ','QUAD8   ','QUAD9   ',&
        'TRIA3   ','TRIA6   ','QUAD4   ','QUAD8   ','QUAD9   '/)
!
! - Name of geometry type for contact/friction element
!
    character(len=8), parameter, dimension(nb_cont_geom) :: lypg_cont_name = (/&
        'SEG22   ','SEG33   ','SEG23   ','SEG32   ','TRIA33  ',&
        'TR3TR6  ','TR6TR3  ','TRIA66  ','QUAD44  ','QU4QU8  ',&
        'QU8QU4  ','QUAD88  ','QU4TR3  ','TR3QU4  ','TR6QU4  ',&
        'QU4TR6  ','TR6QU8  ','QU8TR6  ','TR6QU9  ','QU9TR6  ',&
        'QU8TR3  ','TR3QU8  ','QU8QU9  ','QU9QU8  ','QU9QU4  ',&
        'QU4QU9  ','QU9TR3  ','TR3QU9  ','QUAD99  ','SEG22   ',&
        'SE2TR3  ','SE2TR6  ','SE2QU4  ','SE2QU8  ','SE2QU9  ',&
        'SE3TR3  ','SE3TR6  ','SE3QU4  ','SE3QU8  ','SE3QU9  '/)
!
! - Number of nodes for contact/friction element
!
    integer, parameter, dimension(nb_cont_geom) :: nb_node = (/&
        4 ,6 ,5 ,5 ,6 ,&
        9 ,9 ,12,8 ,12,&
        12,16,7 ,7 ,10,&
        10,14,14,15,15,&
        11,11,17,17,13,&
        13,12,12,18,4 ,&
        5 ,8 ,6 ,10,11,&
        6 ,9 ,7 ,11,12/)
!
! - Name of FE type of contact element
!
    character(len=8), parameter, dimension(nb_cont_solv) :: lypf_cont_name = (/&
        'COS2S2  ','COS3S3  ','COS2S3  ','COS3S2  ','COT3T3  ',&
        'COT3T6  ','COT6T3  ','COT6T6  ','COQ4Q4  ','COQ4Q8  ',&
        'COQ8Q4  ','COQ8Q8  ','COQ4T3  ','COT3Q4  ','COT6Q4  ',&
        'COQ4T6  ','COT6Q8  ','COQ8T6  ','COT6Q9  ','COQ9T6  ',&
        'COQ8T3  ','COT3Q8  ','COQ8Q9  ','COQ9Q8  ','COQ9Q4  ',&
        'COQ4Q9  ','COQ9T3  ','COT3Q9  ','COQ9Q9  ','COP2P2  ',&
        'COS2T3  ','COS2T6  ','COS2Q4  ','COS2Q8  ','COS2Q9  ',&
        'COS3T3  ','COS3T6  ','COS3Q4  ','COS3Q8  ','COS3Q9  '/)
!
! - Name of FE type of friction element
!
    character(len=8), parameter, dimension(nb_cont_solv) :: lypf_frot_name = (/&
        'CFS2S2  ','CFS3S3  ','CFS2S3  ','CFS3S2  ','CFT3T3  ',&
        'CFT3T6  ','CFT6T3  ','CFT6T6  ','CFQ4Q4  ','CFQ4Q8  ',&
        'CFQ8Q4  ','CFQ8Q8  ','CFQ4T3  ','CFT3Q4  ','CFT6Q4  ',&
        'CFQ4T6  ','CFT6Q8  ','CFQ8T6  ','CFT6Q9  ','CFQ9T6  ',&
        'CFQ8T3  ','CFT3Q8  ','CFQ8Q9  ','CFQ9Q8  ','CFQ9Q4  ',&
        'CFQ4Q9  ','CFQ9T3  ','CFT3Q9  ','CFQ9Q9  ','CFP2P2  ',&
        'CFS2T3  ','CFS2T6  ','CFS2Q4  ','CFS2Q8  ','CFS2Q9  ',&
        'CFS3T3  ','CFS3T6  ','CFS3Q4  ','CFS3Q8  ','CFS3Q9  '/)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_cont_geom, elem_indx
    character(len=16) :: typg_cont_name, valk(2), typf_cont_name, typf_frot_name
!
! --------------------------------------------------------------------------------------------------
!
    elem_indx = 0
!
! - Total number of contact elements defined
!
    if (present(nb_cont_type_)) then
        nb_cont_type_ = nb_cont_solv
    endif 
!
! - Set index for contact/friction element
!
    if (present(set_elem_indx_)) then
        elem_indx = set_elem_indx_
    endif
!
! - Index to select contact/friction element
!
    if (present(typg_slav_name_) .and. present(typg_mast_name_)) then
        elem_indx = 0
        do i_cont_geom = 1, nb_cont_geom
            if (typg_slav_name_ .eq. lypg_slav_name(i_cont_geom)) then
                if (typg_mast_name_ .eq. lypg_mast_name(i_cont_geom)) then
                    elem_indx = i_cont_geom
                endif
            endif
        end do
        if (elem_indx .eq. 0) then
            valk(1) = typg_slav_name_
            valk(2) = typg_mast_name_
            call utmess('F', 'CONTACT_96', nk=2, valk=valk)
        endif
! ----- For beam elements
        if (elem_indx .eq. 1) then
            if (model_ndim_ .eq. 2) then
                elem_indx = 1
            else
                elem_indx = 30
            endif
        endif
    endif
!
! - Number of nodes of contact/friction element
!
    if (present(nb_node_elem_)) then
        ASSERT(elem_indx .ne. 0) 
        nb_node_elem_ = nb_node(elem_indx)
    endif
!
! - Index of geometric type of contact/friction element
!
    if (present(typg_cont_nume_)) then
        ASSERT(elem_indx .ne. 0)
        typg_cont_name = lypg_cont_name(elem_indx)
        call jenonu(jexnom('&CATA.TM.NOMTM', typg_cont_name), typg_cont_nume_)
    endif
!
! - Name of FE type of contact element
!
    if (present(typf_cont_nume_)) then
        ASSERT(elem_indx .ne. 0)
        typf_cont_name = lypf_cont_name(elem_indx)
        if (l_axi_) then
            typf_cont_name(7:7) = 'A'
        endif
        call jenonu(jexnom('&CATA.TE.NOMTE', typf_cont_name), typf_cont_nume_)
    endif
!
! - Name of FE type of friction element
!
    if (present(typf_frot_nume_)) then
        ASSERT(elem_indx .ne. 0)
        typf_frot_name = lypf_frot_name(elem_indx)
        if (l_axi_) then
            typf_frot_name(7:7) = 'A'
        endif
        call jenonu(jexnom('&CATA.TE.NOMTE', typf_frot_name), typf_frot_nume_)
    endif
!
! - Get index for contact/friction element
!
    if (present(get_elem_indx_)) then
        ASSERT(elem_indx .ne. 0)
        get_elem_indx_ = elem_indx
    endif
!
end subroutine
