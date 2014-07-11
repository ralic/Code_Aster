subroutine nurota(modelz, nume_ddl, compor, sdnuro)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/exisdg.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
#include "asterfort/sele_elem_comp.h"
#include "asterfort/sele_node_elem.h"
#include "asterfort/select_dof.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: modelz
    character(len=24), intent(in) :: nume_ddl
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: sdnuro
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! Get position of large rotation dof 
!
! --------------------------------------------------------------------------------------------------
!
! In  modelz   : name of model
! In  nume_ddl : name of numbering (NUME_DDL)
! In  compor   : name of comportment CARTE
! In  sdnuro   : name of datastructure to save position of large rotation dof 
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_elem_type, nb_cmp, nb_node_found, nb_equa
    character(len=16) :: defo_comp
    character(len=8), pointer :: list_cmp(:) => null()
    integer, pointer :: list_equa(:) => null()
    integer, pointer :: list_node(:) => null()
    character(len=16), pointer :: list_elem_type(:) => null()
    integer, pointer :: list_elem_comp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Create list of elements type
!
    nb_elem_type = 3
    AS_ALLOCATE(vk16=list_elem_type, size = nb_elem_type)
    list_elem_type(1) = 'MECA_POU_D_T_GD'
    list_elem_type(2) = 'MEC3TR7H'
    list_elem_type(3) = 'MEC3QU9H'
!
! - Create list of components
!
    nb_cmp = 3
    AS_ALLOCATE(vk8=list_cmp, size = nb_cmp)
    list_cmp(1) = 'DRX'
    list_cmp(2) = 'DRY'
    list_cmp(3) = 'DRZ'
!
! - Pre-selection of elements which have GROT_GDEP
!
    defo_comp     = 'GROT_GDEP'
    call sele_elem_comp(modelz, compor, defo_comp, list_elem_comp)
!
! - Select nodes by element type
!
    call sele_node_elem(modelz        , nb_elem_type, list_elem_type, list_node, nb_node_found,&
                        list_elem_comp)
!
! - Create list of equations
!
    call dismoi('NB_EQUA', nume_ddl, 'NUME_DDL', repi=nb_equa)
    if (nb_node_found .gt. 0) then
        call wkvect(sdnuro, 'V V I', nb_equa, vi = list_equa)
    else
        goto 999
    endif
!
! - Find components in list of equations
!
    call select_dof(list_equa, &
                       nume_ddlz = nume_ddl,&
                       only_mesh = .true.  ,&
                       nb_nodez  = nb_node_found , list_node = list_node,&
                       nb_cmpz   = nb_cmp        , list_cmp  = list_cmp)
!
999 continue
!
    AS_DEALLOCATE(vi=list_node)
    AS_DEALLOCATE(vi=list_elem_comp)
    AS_DEALLOCATE(vk8=list_cmp)
    AS_DEALLOCATE(vk16=list_elem_type)
end subroutine
