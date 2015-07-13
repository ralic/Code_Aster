subroutine elimco(sdcont      , mesh        , model       , nb_cont_surf,&
                  nb_cont_elem, nb_cont_node, l_elim_coq3d, nb_node_coq3d_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cflecq.h"
#include "asterfort/cflema.h"
#include "asterfort/cfleno.h"
#include "asterfort/cfmema.h"
#include "asterfort/cfmeno.h"
#include "asterfort/jedetr.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    integer, intent(in) :: nb_cont_surf
    integer, intent(inout) :: nb_cont_elem
    integer, intent(inout) :: nb_cont_node
    aster_logical, intent(in) :: l_elim_coq3d
    integer, optional, intent(out) :: nb_node_coq3d_
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress multiple nodes/elements
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  mesh             : name of mesh
! In  model            : name of model
! In  nb_cont_surf     : number of surfaces of contact
! IO  nb_cont_node     : number of nodes of contact
! IO  nb_elem_node     : number of elements of contact
! In  l_elim_coq3d     : .true. to suppress COQUE_3D nodes
! Out nb_node_coq3d    : number of nodes belongs to COQUE_3D
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_node0, nb_cont_elem0, nb_node_coq3d
    character(len=24) :: sdcont_defi
    integer, pointer :: v_poin_elem(:) => null()
    integer, pointer :: v_list_elem(:) => null()
    integer, pointer :: v_poin_node(:) => null()
    integer, pointer :: v_list_node(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
    nb_cont_elem0 = nb_cont_elem
    nb_cont_node0 = nb_cont_node
    nb_node_coq3d = 0
!
! - Detection of double elements
!
    call cflema(sdcont_defi , nb_cont_surf, nb_cont_elem0, v_list_elem, v_poin_elem,&
                nb_cont_elem)
!
! - Suppress double elements
!
    if (nb_cont_elem0 .ne. nb_cont_elem) then
        call cfmema(sdcont_defi, nb_cont_surf, nb_cont_elem0, v_list_elem, v_poin_elem,&
                    nb_cont_elem)
    endif
    AS_DEALLOCATE(vi=v_poin_elem)
    AS_DEALLOCATE(vi=v_list_elem)
!
! - Detection of double nodes
!
    call cfleno(sdcont_defi , nb_cont_surf, nb_cont_node0, v_list_node, v_poin_node,&
                nb_cont_node)
!
! - Suppress double nodes
!
    if (nb_cont_node0 .ne. nb_cont_node) then
        call cfmeno(sdcont_defi, nb_cont_surf, nb_cont_node0, v_list_node, v_poin_node,&
                    nb_cont_node)
    endif
    AS_DEALLOCATE(vi=v_poin_node)
    AS_DEALLOCATE(vi=v_list_node)
!
! - List of nodes for COQUE_3D
!
    nb_cont_node0 = nb_cont_node
    call cflecq(mesh       , model      , sdcont_defi , nb_cont_surf , nb_cont_node0,&
                v_list_node, v_poin_node, nb_cont_node, nb_node_coq3d)
!
! - Suppress nodes for COQUE_3D
!
    if (l_elim_coq3d) then
        if (nb_cont_node0 .ne. nb_cont_node) then
            call cfmeno(sdcont_defi , nb_cont_surf, nb_cont_node0, v_list_node, v_poin_node,&
                        nb_cont_node)
        endif
    endif
    AS_DEALLOCATE(vi=v_poin_node)
    AS_DEALLOCATE(vi=v_list_node)
!
    if (present(nb_node_coq3d_)) then
        nb_node_coq3d_ = nb_node_coq3d
    endif
!
end subroutine
