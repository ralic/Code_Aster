subroutine mmelem_data_l(set_cont_indx_ , l_axi_         , model_ndim_,&
                         elem_1_        , elem_2_        ,&
                         nb_cont_type_  , nb_node_elem_  ,&
                         cont_geom_nume_, cont_geom_name_,&
                         cont_elem_name_, frot_elem_name_,&
                         get_cont_indx_ )
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
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Define late elements for contact 
!
! --------------------------------------------------------------------------------------------------
!
! In  set_cont_indx    : index of late element for contact (to set)
! In  l_axi            : .true. for axi-symetric model
! In  model_ndim       : size of model
! In  elem_1           : first geometric element to identify contact element
! In  elem_2           : second geometric element to identify contact element
! Out nb_node_elem     : number of nodes for late element contact
! Out cont_geom_nume   : index in element catalog for late element contact
! Out nb_cont_type     : total number of contact elements defined
! Out cont_elem_name   : type of contact element (finite element)
! Out frot_elem_name   : type of friction element (finite element)
! Out cont_geom_name   : name in element catalog for late element contact
! Out get_cont_indx    : index of late element for contact (to get/identification)
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cont_type = 40
!
! - Type of contact element (geometry)
!
    character(len=8), parameter, dimension(nb_cont_type) :: list_cont_geom = (/&
        'SEG22   ','SEG33   ','SEG23   ','SEG32   ','TRIA33  ',&
        'TR3TR6  ','TR6TR3  ','TRIA66  ','QUAD44  ','QU4QU8  ',&
        'QU8QU4  ','QUAD88  ','QU4TR3  ','TR3QU4  ','TR6QU4  ',&
        'QU4TR6  ','TR6QU8  ','QU8TR6  ','TR6QU9  ','QU9TR6  ',&
        'QU8TR3  ','TR3QU8  ','QU8QU9  ','QU9QU8  ','QU9QU4  ',&
        'QU4QU9  ','QU9TR3  ','TR3QU9  ','QUAD99  ','SEG22   ',&
        'SE2TR3  ','SE2TR6  ','SE2QU4  ','SE2QU8  ','SE2QU9  ',&
        'SE3TR3  ','SE3TR6  ','SE3QU4  ','SE3QU8  ','SE3QU9  '/)
!
! - Number of nodes for contact element
!
    integer, parameter, dimension(nb_cont_type) :: nb_node = (/&
        4 ,6 ,5 ,5 ,6 ,&
        9 ,9 ,12,8 ,12,&
        12,16,7 ,7 ,10,&
        10,14,14,15,15,&
        11,11,17,17,13,&
        13,12,12,18,4 ,&
        5 ,8 ,6 ,10,11,&
        6 ,9 ,7 ,11,12/)
!
! - Type of contact element (finite element)
!
    character(len=8), parameter, dimension(nb_cont_type) :: list_cont_elem = (/&
        'COS2S2  ','COS3S3  ','COS2S3  ','COS3S2  ','COT3T3  ',&
        'COT3T6  ','COT6T3  ','COT6T6  ','COQ4Q4  ','COQ4Q8  ',&
        'COQ8Q4  ','COQ8Q8  ','COQ4T3  ','COT3Q4  ','COT6Q4  ',&
        'COQ4T6  ','COT6Q8  ','COQ8T6  ','COT6Q9  ','COQ9T6  ',&
        'COQ8T3  ','COT3Q8  ','COQ8Q9  ','COQ9Q8  ','COQ9Q4  ',&
        'COQ4Q9  ','COQ9T3  ','COT3Q9  ','COQ9Q9  ','COP2P2  ',&
        'COS2T3  ','COS2T6  ','COS2Q4  ','COS2Q8  ','COS2Q9  ',&
        'COS3T3  ','COS3T6  ','COS3Q4  ','COS3Q8  ','COS3Q9  '/)
!
! - Type of friction element (finite element)
!
    character(len=8), parameter, dimension(nb_cont_type) :: list_frot_elem = (/&
        'CFS2S2  ','CFS3S3  ','CFS2S3  ','CFS3S2  ','CFT3T3  ',&
        'CFT3T6  ','CFT6T3  ','CFT6T6  ','CFQ4Q4  ','CFQ4Q8  ',&
        'CFQ8Q4  ','CFQ8Q8  ','CFQ4T3  ','CFT3Q4  ','CFT6Q4  ',&
        'CFQ4T6  ','CFT6Q8  ','CFQ8T6  ','CFT6Q9  ','CFQ9T6  ',&
        'CFQ8T3  ','CFT3Q8  ','CFQ8Q9  ','CFQ9Q8  ','CFQ9Q4  ',&
        'CFQ4Q9  ','CFQ9T3  ','CFT3Q9  ','CFQ9Q9  ','CFP2P2  ',&
        'CFS2T3  ','CFS2T6  ','CFS2Q4  ','CFS2Q8  ','CFS2Q9  ',&
        'CFS3T3  ','CFS3T6  ','CFS3Q4  ','CFS3Q8  ','CFS3Q9  '/)
!
! - Type of slave element
!
    character(len=8), parameter, dimension(nb_cont_type) :: list_geom_slav = (/&
        'SEG2    ','SEG3    ','SEG2    ','SEG3    ','TRIA3   ',&
        'TRIA3   ','TRIA6   ','TRIA6   ','QUAD4   ','QUAD4   ',&
        'QUAD8   ','QUAD8   ','QUAD4   ','TRIA3   ','TRIA6   ',&
        'QUAD4   ','TRIA6   ','QUAD8   ','TRIA6   ','QUAD9   ',&
        'QUAD8   ','TRIA3   ','QUAD8   ','QUAD9   ','QUAD9   ',&
        'QUAD4   ','QUAD9   ','TRIA3   ','QUAD9   ','SEG2    ',&
        'SEG2    ','SEG2    ','SEG2    ','SEG2    ','SEG2    ',&
        'SEG3    ','SEG3    ','SEG3    ','SEG3    ','SEG3    '/)
!
! - Type of master element
!
    character(len=8), parameter, dimension(nb_cont_type) :: list_geom_mast = (/&
        'SEG2    ','SEG3    ','SEG3    ','SEG2    ','TRIA3   ',&
        'TRIA6   ','TRIA3   ','TRIA6   ','QUAD4   ','QUAD8   ',&
        'QUAD4   ','QUAD8   ','TRIA3   ','QUAD4   ','QUAD4   ',&
        'TRIA6   ','QUAD8   ','TRIA6   ','QUAD9   ','TRIA6   ',&
        'TRIA3   ','QUAD8   ','QUAD9   ','QUAD8   ','QUAD4   ',&
        'QUAD9   ','TRIA3   ','QUAD9   ','QUAD9   ','SEG2    ',&
        'TRIA3   ','TRIA6   ','QUAD4   ','QUAD8   ','QUAD9   ',&
        'TRIA3   ','TRIA6   ','QUAD4   ','QUAD8   ','QUAD9   '/)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_cont_elem
    character(len=16) :: cont_geom_name, valk(2)
!
! --------------------------------------------------------------------------------------------------
!

!
! - Identify contact element 
!
    if (present(elem_1_).and.present(elem_2_)) then
        get_cont_indx_ = 0
        do i_cont_elem = 1, nb_cont_type
            if (elem_1_ .eq. list_geom_slav(i_cont_elem)) then
                if (elem_2_ .eq. list_geom_mast(i_cont_elem)) then
                    get_cont_indx_ = i_cont_elem
                endif
            endif
        end do
        if (get_cont_indx_ .eq. 0) then
            valk(1) = elem_1_
            valk(2) = elem_2_
            call utmess('F', 'CONTACT_96', nk=2, valk=valk)
        endif
!
! ----- Contact element (geometry)
!
        cont_geom_name = list_cont_geom(get_cont_indx_)
!
! ----- For beam elements
!
        if (cont_geom_name .eq. 'SEG22') then
            if (model_ndim_ .eq. 2) then
                get_cont_indx_ = 1
            else
                get_cont_indx_ = 30
            endif
        endif
!
! ----- Change name for axisymetric
!
        if (l_axi_) then
            if (cont_geom_name(1:3) .eq. 'SEG') then
                cont_elem_name_(7:7) = 'A'
                frot_elem_name_(7:7) = 'A'
            else
                ASSERT(.false.)
            endif
        endif
    endif
!
! - Total number of late elements defined
!
    if (present(nb_cont_type_)) then
        nb_cont_type_ = nb_cont_type
    endif
!
! - Number of nodes
!
    if (present(nb_node_elem_)) then
        nb_node_elem_ = nb_node(set_cont_indx_)
    endif
!
! - Index in element catalog
!
    if (present(cont_geom_nume_)) then
        cont_geom_name = list_cont_geom(set_cont_indx_)
        call jenonu(jexnom('&CATA.TM.NOMTM', cont_geom_name), cont_geom_nume_)
    endif
!
! - Type of contact element (finite element)
!
    if (present(cont_elem_name_)) then
        cont_elem_name_ = list_cont_elem(set_cont_indx_)
    endif
!
! - Type of friction element (finite element)
!
    if (present(frot_elem_name_)) then
        frot_elem_name_ = list_frot_elem(set_cont_indx_)
    endif
!
! - Name in element catalog for late element contact
!
    if (present(cont_geom_name_)) then
        cont_geom_name_ = list_cont_geom(set_cont_indx_)
    endif
!
end subroutine
