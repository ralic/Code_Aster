subroutine cfleno(sdcont_defi , nb_cont_surf, nb_cont_node0, v_list_node, v_poin_node,&
                  nb_cont_node)
!
implicit none
!
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/assert.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
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
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: nb_cont_surf
    integer, intent(in) :: nb_cont_node0
    integer, intent(inout) :: nb_cont_node
    integer, pointer, intent(out) :: v_poin_node(:)
    integer, pointer, intent(out) :: v_list_node(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress multiple nodes - Create list of double nodes in the same contact surface
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_node0    : number of nodes of contact (before detection of multiple nodes)
! IO  nb_cont_node     : number of nodes of contact (after detection of multiple nodes)
! Out v_list_node      : pointer to list of non-double nodes
! Out v_poin_node      : pointer to pointer of contact surface
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jdecno
    integer :: i_surf, i_node, ii, node_nume_1, node_nume_2, k
    integer :: nb_node_elim, nb_node
    integer, pointer :: v_node_indx(:) => null()
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_node_elim = 0
!
! - Datastructure for contact definition
!
    sdcont_noeuco = sdcont_defi(1:16)//'.NOEUCO'
    call jeveuo(sdcont_noeuco, 'E', vi = v_sdcont_noeuco)
!
! - Temporary vectors
!
    AS_ALLOCATE(vi=v_node_indx, size=nb_cont_node)
    AS_ALLOCATE(vi=v_poin_node, size=nb_cont_surf+1)
!
! - Double-node detection
!
    do i_surf = 1, nb_cont_surf
        v_poin_node(i_surf+1) = v_poin_node(i_surf)
        call cfnbsf(sdcont_defi, i_surf, 'NOEU', nb_node, jdecno)
        do 20 i_node = 1, nb_node
            node_nume_1 = v_sdcont_noeuco(jdecno+i_node)
            do ii = 1, i_node - 1
                node_nume_2 = v_sdcont_noeuco(jdecno+ii)
                if (node_nume_1 .eq. node_nume_2) then
                    v_node_indx(jdecno+i_node) = 1
                    v_poin_node(i_surf+1) = v_poin_node(i_surf+1) + 1
                    nb_node_elim = nb_node_elim + 1
                    goto 20
                endif
            end do
20      continue
    end do
!
! - Non-suppressed nodes vector
!
    nb_cont_node = nb_cont_node0 - nb_node_elim
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
