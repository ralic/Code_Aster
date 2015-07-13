subroutine elimcq(sdcont, mesh, nb_cont_zone, nb_cont_surf, nb_cont_node)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/cfleq8.h"
#include "asterfort/cfmeno.h"
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
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_cont_zone
    integer, intent(in) :: nb_cont_surf
    integer, intent(inout) :: nb_cont_node
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress middle nodes from QUAD8
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  mesh             : name of mesh
! In  nb_cont_zone     : number of zones of contact
! In  nb_cont_surf     : number of surfaces of contact
! IO  nb_cont_node     : number of nodes of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_node0
    character(len=24) :: sdcont_defi
    integer, pointer :: v_poin_node(:) => null()
    integer, pointer :: v_list_node(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
!
! - Create list of middle nodes
!
    nb_cont_node0 = nb_cont_node
    call cfleq8(mesh         , sdcont_defi, nb_cont_zone, nb_cont_surf, nb_cont_node,&
                nb_cont_node0, v_list_node, v_poin_node )
!
! - List of nodes update
!
    if (nb_cont_node0 .ne. nb_cont_node) then
        call cfmeno(sdcont_defi, nb_cont_surf, nb_cont_node0, v_list_node, v_poin_node,&
                    nb_cont_node)
    endif
!
    AS_DEALLOCATE(vi=v_poin_node)
    AS_DEALLOCATE(vi=v_list_node)
!
end subroutine
