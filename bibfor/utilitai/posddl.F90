subroutine posddl(typesd  , resu, node_name, cmp_name, node_nume,&
                  dof_nume)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/select_dof.h"
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
!
    character(len=*), intent(in) :: typesd
    character(len=*), intent(in) :: resu
    character(len=*), intent(in) :: node_name
    character(len=*), intent(in) :: cmp_name
    integer, intent(out) :: node_nume
    integer, intent(out) :: dof_nume
!
! --------------------------------------------------------------------------------------------------
!
! Get dof and node index
!
! --------------------------------------------------------------------------------------------------
!
! In  typesd    : type of datastructure (chamno/nume_ddl)
! In  resu      : name of datastructure (chamno/nume_ddl)
! In  node_name : name of (physical) node to find 
! In  cmp_name  : name of component to find
! Out node_nume : index of node (in mesh)
!                 0 if node doesn't exist
! Out dof_nume  : index of dof
!                 0 if (node,cmp) doesn't exist
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cmp, nb_node
    character(len=8) :: mesh
    integer, pointer :: list_idx_dof(:) => null()
    integer, pointer :: list_node(:) => null()
    character(len=8), pointer :: list_cmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
! 
    dof_nume = 0
    if (typesd .eq. 'NUME_DDL') then
        call dismoi('NOM_MAILLA', resu, 'NUME_DDL', repk=mesh)
    else if (typesd .eq. 'CHAM_NO') then
        call dismoi('NOM_MAILLA', resu, 'CHAM_NO', repk=mesh)
    else
        ASSERT(.false.)
    endif
    call jenonu(jexnom(mesh//'.NOMNOE', node_name), node_nume)
!
    if (node_nume .ne. 0) then
!
        nb_cmp = 1
!
! ----- Create list of components to ssek
!
        AS_ALLOCATE(vk8=list_cmp, size = nb_cmp)
        list_cmp(1) = cmp_name
!
! ----- Create list of results
!
        AS_ALLOCATE(vi=list_idx_dof, size = nb_cmp)
!
! ----- Create list of nodes
!
        nb_node = 1
        AS_ALLOCATE(vi=list_node, size = nb_node)
        list_node(1) = node_nume
!
! ----- Find specific dof
!
        if (typesd .eq. 'NUME_DDL') then
            call select_dof(list_idx_dof = list_idx_dof, &
                            nume_ddlz = resu, &
                            nb_nodez  = nb_node, list_nodez = list_node,&
                            nb_cmpz   = nb_cmp , list_cmpz  = list_cmp)
        else if (typesd .eq. 'CHAM_NO') then
            call select_dof(list_idx_dof = list_idx_dof,&
                            chamnoz   = resu,&
                            nb_nodez  = nb_node, list_nodez = list_node,&
                            nb_cmpz   = nb_cmp , list_cmpz  = list_cmp)
        else
            ASSERT(.false.)
        endif
        dof_nume = list_idx_dof(1)
!
        AS_DEALLOCATE(vi=list_idx_dof)
        AS_DEALLOCATE(vi=list_node)
        AS_DEALLOCATE(vk8=list_cmp)
!
    endif

end subroutine
