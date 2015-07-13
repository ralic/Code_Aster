subroutine cfleqb(mesh, sdcont_defi, nb_cont_zone, nt_node_middle)
!
implicit none
!
#include "asterf_types.h"
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
    integer, intent(in) :: nt_node_middle
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Suppress quadratic middle nodes of QUAD8 - Save list in contact datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  nb_cont_zone     : number of zones of contact
! In  nt_node middle   : number of middle nodes from QUAD8 elements!
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jdecma, elem_nume, type_nume
    integer :: i_surf_curr, i_zone, i_elem, i_node_quad, i_surf
    integer :: inoqto, jdecqu
    integer :: node_quad(3, 4)
    integer :: nb_surf, nb_elem, nb_node_middle
    character(len=8) :: type_name
    aster_logical :: l_veri
    integer, pointer :: v_mesh_typmail(:) => null()
    integer, pointer :: v_mesh_connex(:) => null()
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=24) :: sdcont_pzoneco
    integer, pointer :: v_sdcont_pzoneco(:) => null()
    character(len=24) :: sdcont_pnoeuqu
    integer, pointer :: v_sdcont_pnoeuqu(:) => null()
    character(len=24) :: sdcont_noeuqu
    integer, pointer :: v_sdcont_noeuqu(:) => null()
!
! ----------------------------------------------------------------------
!
    jdecqu = 0
!
! - Datastructure for contact definition
!
    sdcont_pzoneco = sdcont_defi(1:16)//'.PZONECO'
    sdcont_mailco  = sdcont_defi(1:16)//'.MAILCO'
    sdcont_pnoeuqu = sdcont_defi(1:16)//'.PNOEUQU'
    sdcont_noeuqu  = sdcont_defi(1:16)//'.NOEUQU'
    call jeveuo(sdcont_pzoneco, 'L', vi = v_sdcont_pzoneco)
    call jeveuo(sdcont_mailco , 'L', vi = v_sdcont_mailco)
!
! - Create datastructure for middle nodes
!
    call wkvect(sdcont_pnoeuqu, 'V V I', nb_cont_zone+1  , vi = v_sdcont_pnoeuqu)
    call wkvect(sdcont_noeuqu , 'V V I', 3*nt_node_middle, vi = v_sdcont_noeuqu)
!
! - Access to mesh
!
    call jeveuo(mesh(1:8)//'.TYPMAIL', 'L', vi = v_mesh_typmail)
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
        inoqto = 0
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
! ------------- Connectivity of element
!
                call jeveuo(jexnum(mesh(1:8)//'.CONNEX', elem_nume), 'L', vi = v_mesh_connex)
!
! ------------- Get nodes: vertex_1 - vertex_2 - middle
!
                if (type_name .eq. 'QUAD8') then
                    nb_node_middle = 4
                    node_quad(1,1) = v_mesh_connex(5)
                    node_quad(1,2) = v_mesh_connex(6)
                    node_quad(1,3) = v_mesh_connex(7)
                    node_quad(1,4) = v_mesh_connex(8)
                    node_quad(2,1) = v_mesh_connex(1)
                    node_quad(2,2) = v_mesh_connex(2)
                    node_quad(2,3) = v_mesh_connex(3)
                    node_quad(2,4) = v_mesh_connex(4)
                    node_quad(3,1) = v_mesh_connex(2)
                    node_quad(3,2) = v_mesh_connex(3)
                    node_quad(3,3) = v_mesh_connex(4)
                    node_quad(3,4) = v_mesh_connex(1)
                else
                    nb_node_middle = 0
                endif
!
! ------------- Set list of nodes
!
                if (type_name(1:5) .eq. 'QUAD8') then
                    do i_node_quad = 1, nb_node_middle
                        v_sdcont_noeuqu(jdecqu+3*(i_node_quad-1)+1) = node_quad(1,i_node_quad)
                        v_sdcont_noeuqu(jdecqu+3*(i_node_quad-1)+2) = node_quad(2,i_node_quad)
                        v_sdcont_noeuqu(jdecqu+3*(i_node_quad-1)+3) = node_quad(3,i_node_quad)
                    end do
                    jdecqu = jdecqu + 3*nb_node_middle
                    inoqto = inoqto + 3*nb_node_middle
                endif
            end do
        end do
 21     continue
!
! ----- Update pointer
!
        v_sdcont_pnoeuqu(i_zone+1) = v_sdcont_pnoeuqu(i_zone)+inoqto
    end do
!
end subroutine
