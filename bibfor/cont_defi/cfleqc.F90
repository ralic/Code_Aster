subroutine cfleqc(mesh       , sdcont_defi, nb_cont_zone, nb_cont_node, nb_cont_surf,&
                  v_poin_node, v_indi_node, nb_node_elim)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/assert.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfl.h"
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
    integer, intent(in) :: nb_cont_node
    integer, pointer, intent(out) :: v_poin_node(:)
    integer, pointer, intent(out) :: v_indi_node(:)
    integer, intent(out) :: nb_node_elim
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress quadratic middle nodes of QUAD8 - Create list of (middle) nodes to suppress
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  nb_cont_zone     : number of zones of contact
! In  nb_cont_surf     : number of surfaces of contact
! In  nb_cont_node     : number of nodes of contact (after detection of middle nodes)
! Out v_indi_node      : pointer to indicator of middle nodes
! Out v_poin_node      : pointer to pointer of contact surface
! Out nb_node_elim     : number of nodes to suppress
!
! --------------------------------------------------------------------------------------------------
!
    integer :: elem_nume, type_nume
    integer :: i_surf_curr, i_zone, i_elem, i_node, i_node_quad, i_surf
    integer :: nb_surf, nb_elem, nb_node_quad, nb_node
    character(len=8) :: type_name
    aster_logical :: l_veri
    integer :: jdecma, jdecno, jdecqu
    integer :: node_nume_1, node_nume_2
    integer, pointer :: v_mesh_typmail(:) => null()
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
    character(len=24) :: sdcont_pzoneco
    integer, pointer :: v_sdcont_pzoneco(:) => null()
    character(len=24) :: sdcont_pnoeuqu
    integer, pointer :: v_sdcont_pnoeuqu(:) => null()
    character(len=24) :: sdcont_noeuqu
    integer, pointer :: v_sdcont_noeuqu(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_node_elim = 0
    jdecqu       = 0
!
! - Create vectors to use in cfmeno subroutine
!
    AS_ALLOCATE(vi=v_poin_node, size=nb_cont_surf+1)
    AS_ALLOCATE(vi=v_indi_node, size=nb_cont_node)
!
! - Datastructure for contact definition
!
    sdcont_pzoneco = sdcont_defi(1:16)//'.PZONECO'
    sdcont_mailco  = sdcont_defi(1:16)//'.MAILCO'
    sdcont_noeuco  = sdcont_defi(1:16)//'.NOEUCO'
    sdcont_pnoeuqu = sdcont_defi(1:16)//'.PNOEUQU'
    sdcont_noeuqu  = sdcont_defi(1:16)//'.NOEUQU'
    call jeveuo(sdcont_pzoneco, 'L', vi = v_sdcont_pzoneco)
    call jeveuo(sdcont_mailco , 'L', vi = v_sdcont_mailco)
    call jeveuo(sdcont_noeuco , 'L', vi = v_sdcont_noeuco)
    call jeveuo(sdcont_pnoeuqu, 'L', vi = v_sdcont_pnoeuqu)
    call jeveuo(sdcont_noeuqu , 'L', vi = v_sdcont_noeuqu)
!
! - Access to mesh
!
    call jeveuo(mesh(1:8)//'.TYPMAIL', 'L', vi = v_mesh_typmail)
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- No computation
!
        l_veri = mminfl(sdcont_defi,'VERIF',i_zone )
        if (l_veri) then
            goto 21
        endif
!
! ----- Number of contact surfaces
!
        nb_surf = v_sdcont_pzoneco(i_zone+1) - v_sdcont_pzoneco(i_zone)
        ASSERT(nb_surf.eq.2)
!
! ----- Loop on surfaces
!
        do i_surf = 1, nb_surf
!
! --------- Parameters of current surface
!
            i_surf_curr = nb_surf*(i_zone-1)+i_surf
            call cfnbsf(sdcont_defi, i_surf_curr, 'MAIL', nb_elem, jdecma)
!
! --------- Change pointer
!
            v_poin_node(i_surf_curr+1) = v_poin_node(i_surf_curr)
!
! --------- Loop on elements
!
            do i_elem = 1, nb_elem
!
! ------------- Current element
!
                elem_nume = v_sdcont_mailco(jdecma+i_elem)
!
! ------------- Type of element
!
                type_nume = v_mesh_typmail(elem_nume)
                call jenuno(jexnum('&CATA.TM.NOMTM', type_nume), type_name)
!
! ------------- Suppress middle nodes of QUAD8
!
                if (type_name(1:5) .eq. 'QUAD8') then
                    nb_node_quad = (v_sdcont_pnoeuqu(i_zone+1) - v_sdcont_pnoeuqu(i_zone))/3
                    jdecqu       = v_sdcont_pnoeuqu(i_zone)
                    call cfnbsf(sdcont_defi, i_surf_curr, 'NOEU', nb_node, jdecno)
                    do i_node_quad = 1, nb_node_quad
                        node_nume_1 = v_sdcont_noeuqu(jdecqu+3*(i_node_quad-1)+1)
                        do i_node = 1, nb_node
                            node_nume_2 = v_sdcont_noeuco(jdecno+i_node)
                            if (node_nume_1 .eq. node_nume_2) then
                                if (v_indi_node(jdecno+i_node) .eq. 0) then
                                    v_indi_node(jdecno+i_node) = 1
                                    v_poin_node(i_surf_curr+1) = v_poin_node(i_surf_curr+1)+1
                                    nb_node_elim = nb_node_elim + 1
                                endif
                            endif
                        end do
                    end do
                endif
            end do
        end do
 21     continue
    end do
!
    ASSERT((2*nb_cont_zone).eq.nb_cont_surf)
!
end subroutine
