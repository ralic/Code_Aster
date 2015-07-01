subroutine mmelty(mesh, elem_nume, elem_type_, nb_node_, nb_dim_)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: elem_nume
    character(len=8), optional, intent(out) :: elem_type_
    integer, optional, intent(out)  :: nb_node_
    integer, optional, intent(out)  :: nb_dim_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Utility
!
! Continue method - Get informations about element
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  elem_nume        : index of element in mesh
! Out elem_type        : type of element
!                         'PO1' 'SE2' 'SE3'
!                         'TR3' 'TR6' 'TR7'
!                         'QU4' 'QU8' 'QU9'
! Out nb_node          : number of nodes
! Out nb_dim           : dimension of element
!
! --------------------------------------------------------------------------------------------------
!
    integer :: elem_type_nume, nb_node, nb_dim
    character(len=8) :: elem_type_name, elem_type
    integer, pointer :: v_mesh_typmail(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    elem_type_name = ' '
!
! - Access to mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = v_mesh_typmail)
    elem_type_nume = v_mesh_typmail(elem_nume)
!
! - Paramters
!
    call jenuno(jexnum('&CATA.TM.NOMTM', elem_type_nume), elem_type_name)
!
    if (elem_type_name .eq. 'POI1') then
        elem_type = 'PO1'
        nb_node   = 1
        nb_dim    = 1
    else if (elem_type_name .eq. 'SEG2') then
        elem_type = 'SE2'
        nb_node   = 2
        nb_dim    = 2
    else if (elem_type_name .eq. 'SEG3') then
        elem_type = 'SE3'
        nb_node   = 3
        nb_dim    = 2
    else if (elem_type_name .eq. 'TRIA3') then
        elem_type = 'TR3'
        nb_node   = 3
        nb_dim    = 3
    else if (elem_type_name .eq. 'TRIA6') then
        elem_type = 'TR6'
        nb_node   = 6
        nb_dim    = 3
    else if (elem_type_name .eq. 'TRIA7') then
        elem_type = 'TR7'
        nb_node   = 7
        nb_dim    = 3
    else if (elem_type_name .eq. 'QUAD4') then
        elem_type = 'QU4'
        nb_node   = 4
        nb_dim    = 3
    else if (elem_type_name .eq. 'QUAD8') then
        elem_type = 'QU8'
        nb_node   = 8
        nb_dim    = 3
    else if (elem_type_name .eq. 'QUAD9') then
        elem_type = 'QU9'
        nb_node   = 9
        nb_dim    = 3
    else
        ASSERT(.false.)
    endif
!
    if (present(elem_type_)) then
        elem_type_ = elem_type
    endif
    if (present(nb_node_)) then
        nb_node_ = nb_node
    endif
    if (present(nb_dim_)) then
        nb_dim_ = nb_dim
    endif
!
end subroutine
