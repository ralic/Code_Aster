subroutine gt_linoma(mesh,list_elem,nb_elem,list_node,nb_node)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/gmgnre.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
! aslint: disable=W1306
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_elem
    integer, intent(in) :: list_elem(nb_elem)
    integer, pointer, intent(inout) :: list_node(:)
    integer, intent(out) :: nb_node
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Get list of nodes from untracked master elements
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  nb_elem_mast     : number of master elements on current zone
! In  list_elem_mast   : name of datastructure for list of master elements on current zone
! IO  elem_mast_flag   : flag to mark master elements already tracked
! Out nb_node_mast     : number of master nodes on current zone
! Out list_node_mast   : list of master nodes on current zone
!
! --------------------------------------------------------------------------------------------------
!
  integer, pointer :: list_node_aux(:) => null()
  integer, pointer :: litrav(:) => null()
  integer, pointer :: list_elem_aux(:) => null()
  integer          :: aux(nb_elem), nb_elem_aux, nbno, j_info
  integer          :: i_elem, indx_mini, elem_nume, elem_indx
  character(len =24)::klitrav, klist_elem_aux, klist_node_aux
!
! --------------------------------------------------------------------------------------------------
!
! - Initialisation
!   
    klitrav        = '&&OP070_klitrav'
    klist_elem_aux = '&&OP070_klist_elem_aux'
    klist_node_aux = '&&OP070_klist_node_aux'
    nb_elem_aux    = 0
    nb_node        = 0 
    indx_mini = minval(list_elem)
    call jeveuo(mesh//'.DIME', 'L', j_info)
    nbno = zi(j_info-1+1)
    AS_ALLOCATE(vi=litrav, size=nbno )
!
! - Get untracked elements    
!
   do i_elem=1,nb_elem
        elem_nume = list_elem(i_elem)
        elem_indx = elem_nume + 1 - indx_mini 
        nb_elem_aux      = nb_elem_aux + 1
        aux(nb_elem_aux) = elem_nume
    end do
    AS_ALLOCATE(vi=list_elem_aux, size=nb_elem_aux )
    list_elem_aux(:)=aux(1:nb_elem_aux)
    
    if (nb_elem_aux .gt. 0) then
!
! ----- Get list of nodes frome untracked elements
!    
        AS_ALLOCATE(vi=list_node_aux, size=9*nb_elem_aux )
        call gmgnre(mesh, nbno , litrav, list_elem_aux, nb_elem_aux,&
                   list_node_aux, nb_node, 'TOUS')
        AS_ALLOCATE(vi=list_node, size=nb_node )
        list_node(:)=list_node_aux(1:nb_node)
!
! ----- Print check
!
        !write(*,*)"LIST_AUX:",list_elem_aux(:)
        !write(*,*)"LIST:",list_elem(:)
        !write(*,*)"LIST_NO : ",list_node(:)
!
        AS_DEALLOCATE(vi=litrav)
        AS_DEALLOCATE(vi=list_node_aux)
    
    end if
    
    AS_DEALLOCATE(vi=list_elem_aux)
!
end subroutine    
