subroutine cfleq8(mesh         , sdcont_defi, nb_cont_zone, nb_cont_surf, nb_cont_node,&
                  nb_cont_node0, v_list_node, v_poin_node)
!
implicit none
!
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/assert.h"
#include "asterfort/cfleqa.h"
#include "asterfort/cfleqb.h"
#include "asterfort/cfleqc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: nb_cont_zone
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
! Suppress quadratic middle nodes of QUAD8 - Create list of middle nodes
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  nb_cont_zone     : number of zones of contact
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_node0    : number of nodes of contact (before detection of middle nodes)
! IO  nb_cont_node     : number of nodes of contact (after detection of middle nodes)
! Out v_list_node      : pointer to list of non-double nodes
! Out v_poin_node      : pointer to pointer of contact surface
!
! --------------------------------------------------------------------------------------------------
!
    integer, pointer :: v_indi_node(:) => null()
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
    integer :: nb_node_elim, nt_node_middle, k, i_node
!
! --------------------------------------------------------------------------------------------------
!
    nt_node_middle = 0
    nb_node_elim   = 0
!
! - Datastructure for contact definition
!
    sdcont_noeuco  = sdcont_defi(1:16)//'.NOEUCO'
    call jeveuo(sdcont_noeuco , 'L', vi = v_sdcont_noeuco)
!
! - Total number of middle nodes
!
    call cfleqa(mesh, sdcont_defi, nb_cont_zone, nt_node_middle)
    if (nt_node_middle .eq. 0) then
        goto 999
    else
        call utmess('A', 'CONTACT_8')
    endif
!
! - Save list in contact datastructure
!
    call cfleqb(mesh, sdcont_defi, nb_cont_zone, nt_node_middle)
!
! - Create list of (middle) nodes to suppress
!
    call cfleqc(mesh       , sdcont_defi, nb_cont_zone, nb_cont_node, nb_cont_surf,&
                v_poin_node, v_indi_node, nb_node_elim)
!
! - Non-suppressed nodes vector
!
    nb_cont_node = nb_cont_node0 - nb_node_elim
    AS_ALLOCATE(vi=v_list_node, size=nb_cont_node)
!
! - Create list of nodes to suppress
!
    k = 0
    do i_node = 1, nb_cont_node0
        if (v_indi_node(i_node) .eq. 0) then
            k = k + 1
            v_list_node(k) = v_sdcont_noeuco(i_node)
        endif
    end do
    ASSERT(k.eq.nb_cont_node)
!
999 continue
!
! - Clean
!
    AS_DEALLOCATE(vi=v_indi_node)
!
end subroutine
