subroutine mmelem_data_l(l_axi_         ,&
                         typg_slav_name_, typg_mast_name_,&
                         typf_slav_name_,&
                         nb_cont_type_  , nb_node_elem_  ,&
                         typg_cont_nume_,&
                         typf_cont_nume_,&
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
    character(len=8), intent(in), optional :: typg_slav_name_
    character(len=8), intent(in), optional :: typg_mast_name_
    character(len=16), intent(in), optional :: typf_slav_name_
    integer, intent(out), optional :: nb_cont_type_
    integer, intent(out), optional :: nb_node_elem_
    integer, intent(out), optional :: typg_cont_nume_
    integer, intent(out), optional :: typf_cont_nume_
    integer, intent(in), optional :: set_elem_indx_
    integer, intent(out), optional :: get_elem_indx_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Define late elements for contact 
!
! --------------------------------------------------------------------------------------------------
!
! In  l_axi            : .true. for axi-symetric model
! In  typg_slav_name   : name of geometric type of slave element
! In  typg_mast_name   : name of geometric type of master element
! In  typf_slav_name   : name of geometric type of slave element
! Out nb_cont_type     : total number of contact elements defined
! Out nb_node_elem     : number of nodes of contact/friction element
! Out typf_cont_nume   : type of contact element (finite element)
! Out typg_cont_nume   : index of geometric type of contact/friction element
! Out typf_cont_nume   : index of FE type of contact element
! In  set_elem_indx    : index to select contact/friction element (set)
! Out get_elem_indx    : index to select contact/friction element (get)
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cont_geom = 40
    integer, parameter :: nb_cont_solv = 43
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
    integer, parameter, dimension(nb_cont_solv) :: nb_node = (/&
        6 ,12, 8, 8,16,&
        16, 9, 9,12,12,&
        12,12, 7, 7, 7,&
        10,10,10,14,14,&
        14,11,11,11,13,&
        13,13,12,18,17,&
        17,17,15,12,15,&
         4, 4, 4, 6, 5,&
         5, 5, 5/)
!
! - Type of contact element (geometry -> finite element)
!
    character(len=8), parameter, dimension(nb_cont_solv) :: typg_cont_indx = (/&
        'TRIA33','TRIA66','QUAD44','QUAD44','QUAD88',&
        'QUAD88','TR6TR3','TR3TR6','QU4QU8','QU4QU8',&
        'QU8QU4','QU8QU4','QU4TR3','QU4TR3','TR3QU4',&
        'QU4TR6','QU4TR6','TR6QU4','QU8TR6','QU8TR6',&
        'TR6QU8','QU8TR3','QU8TR3','TR3QU8','QU4QU9',&
        'QU4QU9','QU9QU4','TR3QU9','QUAD99','QU8QU9',&
        'QU8QU9','QU9QU8','TR6QU9','QU9TR3','QU9TR6',&
        'SEG22 ','SEG22 ','SEG22 ','SEG33 ','SEG23 ',&
        'SEG23 ','SEG23 ','SEG32 '/)
!
! - Type of slave element (geometry -> finite element)
!
    character(len=8), parameter, dimension(nb_cont_solv) :: typf_slav_indx = (/&
        'LACT33D ','LACT63D ','LACQ43D ','LACQ43DB','LACQ83D ',&
        'LACQ83DB','LACT63D ','LACT33D ','LACQ43D ','LACQ43DB',&
        'LACQ83D ','LACQ83DB','LACQ43D ','LACQ43DB','LACT33D ',&
        'LACQ43D ','LACQ43DB','LACT63D ','LACQ83D ','LACQ83DB',&
        'LACT63D ','LACQ83D ','LACQ83DB','LACT33D ','LACQ43D ',&
        'LACQ43DB','LACQ93D ','LACT33D ','LACQ93D ','LACQ83D ',&
        'LACQ83DB','LACQ93D ','LACT63D ','LACQ93D ','LACQ93D ',&
        'LACS22D ','LACS22DB','LACS22DT','LACS32D ','LACS22D ',&
        'LACS22DB','LACS22DT','LACS32D '/)
!
! - Name of FE type of contact element
!
    character(len=8), parameter, dimension(nb_cont_solv) :: lypf_cont_name = (/&
        'LACT3T3D','LACT6T6D','LACQ4Q4D','LACQ4Q4E','LACQ8Q8D',&
        'LACQ8Q8E','LACT6T3D','LACT3T6D','LACQ4Q8D','LACQ4Q8E',&
        'LACQ8Q4D','LACQ8Q4E','LACQ4T3D','LACQ4T3E','LACT3Q4D',&
        'LACQ4T6D','LACQ4T6E','LACT6Q4D','LACQ8T6D','LACQ8T6E',&
        'LACT6Q8D','LACQ8T3D','LACQ8T3E','LACT3Q8D','LACQ4Q9D',&
        'LACQ4Q9E','LACQ9Q4D','LACT3Q9D','LACQ9Q9D','LACQ8Q9D',&
        'LACQ8Q9E','LACQ9Q8D','LACT6Q9D','LACQ9T3D','LACQ9T6D',&
        'LCS2S2C ','LCS2S2D ','LCS2S2E ','LCS3S3C ','LCS2S3C ',&
        'LCS2S3D ','LCS2S3E ','LCS3S2C '/)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_cont_geom, geom_indx, elem_indx, typg_cont_nume, i_cont
    character(len=16) :: typf_cont_name, typg_cont_name, valk(2)
!
! --------------------------------------------------------------------------------------------------
!
    geom_indx = 0
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
! - Index to select contact/friction geometry
!
    if (present(typg_slav_name_) .and. present(typg_mast_name_)) then
        geom_indx = 0
        do i_cont_geom = 1, nb_cont_geom
            if (typg_slav_name_ .eq. lypg_slav_name(i_cont_geom)) then
                if (typg_mast_name_ .eq. lypg_mast_name(i_cont_geom)) then
                    geom_indx = i_cont_geom
                endif
            endif
        end do
        if (geom_indx .eq. 0) then
            valk(1) = typg_slav_name_
            valk(2) = typg_mast_name_
            call utmess('F', 'CONTACT_96', nk=2, valk=valk)
        endif
    endif
!
! - Index in element catalog (geometry)
!
    if (present(typg_cont_nume_)) then
        ASSERT(geom_indx .ne. 0)
        typg_cont_name = lypg_cont_name(geom_indx)
        call jenonu(jexnom('&CATA.TM.NOMTM', typg_cont_name), typg_cont_nume)
        typg_cont_nume_ = typg_cont_nume
    endif
!
! - Type of contact element (finite element)
!
    if (present(typf_cont_nume_)) then
        do i_cont = 1, nb_cont_solv
            if (typg_cont_indx(i_cont) .eq. typg_cont_name .and.&
                typf_slav_indx(i_cont) .eq. typf_slav_name_) then
                elem_indx = i_cont
                goto 10
            endif
        end do
10      continue
        ASSERT(elem_indx .ne. 0)
        typf_cont_name = lypf_cont_name(elem_indx)
        if (l_axi_) then
            typf_cont_name(8:8) = 'A'
        endif
        call jenonu(jexnom('&CATA.TE.NOMTE', typf_cont_name), typf_cont_nume_)
        ASSERT(typf_cont_nume_ .ne. 0)
    endif
!
! - Number of nodes of contact/friction element
!
    if (present(nb_node_elem_)) then
        ASSERT(elem_indx .ne. 0)
        nb_node_elem_ = nb_node(elem_indx)
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
