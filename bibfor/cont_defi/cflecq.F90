subroutine cflecq(mesh       , model      , sdcont_defi , nb_cont_surf , nb_cont_node0,&
                  v_list_node, v_poin_node, nb_cont_node, nb_node_coq3d)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/iscoqu.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: nb_cont_surf
    integer, intent(in) :: nb_cont_node0
    integer, pointer, intent(out) :: v_poin_node(:)
    integer, pointer, intent(out) :: v_list_node(:)
    integer, intent(out) :: nb_cont_node
    integer, intent(out) :: nb_node_coq3d
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! List of nodes for COQUE_3D
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  mesh             : name of mesh
! In  model            : name of model
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_node0    : initial number of nodes of contact
! Out v_list_node      : pointer to list of COQUE_3D nodes
! Out v_poin_node      : pointer to pointer of contact surface
! Out nb_cont_node     : new number of nodes of contact
! Out nb_node_coq3d    : number of nodes belongs to COQUE_3D
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jdecno, jdecma
    integer :: i_surf, nume_node_2, i_node, k, i_elem, type_nume, node_middle_nume
    integer :: nb_elem, nb_node
    integer :: elem_nume, node_nume
    character(len=8) :: type_name, elem_name, node_name
    aster_logical :: l_coq3d
    integer, pointer :: v_mesh_typmail(:) => null()
    integer, pointer :: v_mesh_connex(:) => null()
    integer, pointer :: v_node_indx(:) => null()
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_node_coq3d = 0
!
! - Datastructure for contact definition
!
    sdcont_noeuco = sdcont_defi(1:16)//'.NOEUCO'
    sdcont_mailco = sdcont_defi(1:16)//'.MAILCO'
    call jeveuo(sdcont_noeuco, 'L', vi = v_sdcont_noeuco)
    call jeveuo(sdcont_mailco, 'L', vi = v_sdcont_mailco)
!
! - Temporary vectors
!
    AS_ALLOCATE(vi=v_node_indx, size=nb_cont_node)
    AS_ALLOCATE(vi=v_poin_node, size=nb_cont_surf+1)
!
! - Access to mesh
!
    call jeveuo(mesh(1:8)//'.TYPMAIL', 'L', vi = v_mesh_typmail)
!
! - Identify middle nodes
!
    do i_surf = 1, nb_cont_surf
        call cfnbsf(sdcont_defi, i_surf, 'MAIL', nb_elem, jdecma)
        v_poin_node(i_surf+1) = v_poin_node(i_surf)
        do i_elem = 1, nb_elem
            elem_nume = v_sdcont_mailco(jdecma+i_elem)
            type_nume = v_mesh_typmail(elem_nume)
            call jenuno(jexnum('&CATA.TM.NOMTM', type_nume), type_name)
            call jenuno(jexnum(mesh//'.NOMMAI', elem_nume), elem_name)
            if (type_name(1:5) .eq. 'QUAD9') then
                call iscoqu(model, elem_nume, l_coq3d)
                node_middle_nume = 9
            else if (type_name(1:5).eq.'TRIA7') then
                call iscoqu(model, elem_nume, l_coq3d)
                node_middle_nume = 7
            else
                l_coq3d = .false.
            endif
            if (l_coq3d) then
                call jeveuo(jexnum(mesh//'.CONNEX', elem_nume), 'L', vi = v_mesh_connex)
                node_nume = v_mesh_connex(node_middle_nume)
                call jenuno(jexnum(mesh//'.NOMNOE', node_nume), node_name)
            endif
            if (l_coq3d) then
                call cfnbsf(sdcont_defi, i_surf, 'NOEU', nb_node, jdecno)
                do i_node = 1, nb_node
                    nume_node_2 = v_sdcont_noeuco(jdecno+i_node)
                    if (nume_node_2 .eq. node_nume) then
                        v_node_indx(jdecno+i_node) = 1
                        v_poin_node(i_surf+1) = v_poin_node(i_surf+1) + 1
                        nb_node_coq3d = nb_node_coq3d + 1
                        goto 90
                    endif
                end do
            endif
 90     continue
        end do
    end do
!
! - Non-suppressed nodes vector
!
    nb_cont_node = nb_cont_node0 - nb_node_coq3d
    AS_ALLOCATE(vi=v_list_node, size=nb_cont_node)
!
! - Copy list of non-suppressed nodes
!
    k = 0
    do i_node = 1, nb_cont_node0
        if (v_node_indx(i_node) .eq. 0) then
            k = k + 1
            v_list_node(k) = v_sdcont_noeuco(i_node)
        endif
    end do
    ASSERT(k.eq.nb_cont_node)
!
! - Clean
!
    AS_DEALLOCATE(vi=v_node_indx)
!
end subroutine
