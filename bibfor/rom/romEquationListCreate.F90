subroutine romEquationListCreate(ds_empi, nume_dof, grnode_int, v_equa_int)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnom.h"
#include "asterfort/select_dof.h"
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
    type(ROM_DS_Empi), intent(in) :: ds_empi
    character(len=24), intent(in) :: nume_dof
    character(len=24), intent(in) :: grnode_int
    integer, pointer, intent(out) :: v_equa_int(:)
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Prepare the list of equations at interface
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! In  nume_dof         : name of numbering (NUME_DDL)
! In  grnode_int       : name of GROUP_NO for interface
! Out v_equa_int       : pointer to list of equations for interface nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_node, nb_equa, nb_cmp
    integer, pointer :: v_list_node(:) => null()
    character(len=8), pointer :: v_list_cmp(:) => null() 
    character(len=24) :: field_type = ' '
    character(len=8) :: mesh = ' '
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_8', sk = ds_empi%base)
    endif
!
! - Get parameters
!
    mesh       = ds_empi%mesh
    field_type = ds_empi%field_type
!
! - Access to mesh
!
    call jelira(jexnom(mesh//'.GROUPENO', grnode_int), 'LONUTI', nb_node)
    call jeveuo(jexnom(mesh//'.GROUPENO', grnode_int), 'E'     , vi = v_list_node)
!
! - Create list of equations
!
    call dismoi('NB_EQUA', nume_dof, 'NUME_DDL', repi = nb_equa)
    AS_ALLOCATE(vi = v_equa_int, size = nb_equa)
!
! - List of components to search
!
    if (field_type .eq. 'TEMP') then
        nb_cmp        = 1
        AS_ALLOCATE(vk8 = v_list_cmp, size = nb_cmp)
        v_list_cmp(1) = 'TEMP'
    elseif (field_type .eq. 'DEPL') then
        nb_cmp        = 3
        AS_ALLOCATE(vk8 = v_list_cmp, size = nb_cmp)
        v_list_cmp(1) = 'DX'
        v_list_cmp(2) = 'DY'
        v_list_cmp(3) = 'DZ'
    else
        ASSERT(.false.)
    endif
!
! - Find index of equations
!
    call select_dof(list_equa  = v_equa_int , nume_ddlz = nume_dof, nb_nodez  = nb_node,&
                    list_nodez = v_list_node, nb_cmpz   = nb_cmp  , list_cmpz = v_list_cmp)
!
! - Clean
!
    AS_DEALLOCATE(vk8 = v_list_cmp)
!
end subroutine
