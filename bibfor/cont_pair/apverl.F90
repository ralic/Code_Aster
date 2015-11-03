subroutine apverl(sdappa, mesh, sdcont_defi)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterc/r8rddg.h"
#include "asterfort/cfinvm.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfnumn.h"
#include "asterfort/appari.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmnorm.h"
#include "blas/ddot.h"
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
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Check normals discontinuity
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: node_mast_name, elem_mast_name, node_old
    integer :: nb_cont_zone, model_ndim
    integer :: i_zone
    integer :: jdecnm, nb_node_mast
    integer :: i_node_mast, i_elem, i_node_curr, i_elem_node
    integer :: jdeciv
    integer :: elem_mast_indx, elem_mast_nume, node_mast_indx(1), node_mast_nume(1)
    integer :: node_nbelem, elem_nbnode
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    real(kind=8) :: tau1_node(3), tau2_node(3), normnd(3)
    real(kind=8) :: noor1, noor2
    real(kind=8) :: angmax
    character(len=24) :: sdappa_tgel, sdappa_tgno
    real(kind=8), pointer :: v_sdappa_tgel(:) => null()
    real(kind=8), pointer :: v_sdappa_tgno(:) => null()
    character(len=24) :: sdappa_verk
    character(len=8), pointer :: v_sdappa_verk(:) => null()
    character(len=24) :: sdappa_vera
    real(kind=8), pointer :: v_sdappa_vera(:) => null()
    integer, pointer :: v_mesh_connex(:) => null()
    real(kind=8) :: prosca, angle, angl_old, val
    integer :: inoeu
    aster_logical :: apcald
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Get contact datastructures
!
    sdappa_verk = sdappa(1:19)//'.VERK'
    sdappa_vera = sdappa(1:19)//'.VERA'
    call jeveuo(sdappa_verk, 'E', vk8 = v_sdappa_verk)
    call jeveuo(sdappa_vera, 'E', vr  = v_sdappa_vera)
    call jelira(sdappa_verk, 'LONUTI', inoeu)
    if (inoeu .ne. 0) goto 999
    angmax = 5.d0
!
! - Acces to pairing datastructure
!
    sdappa_tgel = sdappa(1:19)//'.TGEL'
    sdappa_tgno = sdappa(1:19)//'.TGNO'
    call jeveuo(sdappa_tgno, 'L', vr = v_sdappa_tgno)
!
! - Get parameters
!
    call appari(sdappa, 'APPARI_NBZONE', nb_cont_zone)
    call appari(sdappa, 'APPARI_NDIMG' , model_ndim)
!
! - Loop on contact zones
!
    inoeu = 0
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters on current zone - Master
!
        call apzoni(sdappa, i_zone, 'NBNOM'         , nb_node_mast)
        call apzoni(sdappa, i_zone, 'JDECNM'        , jdecnm)
        call apzonl(sdappa, i_zone, 'CALC_NORM_MAIT', apcald)
        if (apcald) then
!
! --------- Loop on nodes
!
            do i_node_mast = 1, nb_node_mast
!
! ------------- Current node
!
                node_mast_indx(1) = i_node_mast+jdecnm 
                call cfnumn(sdcont_defi, 1, node_mast_indx(1), node_mast_nume(1))
                call jenuno(jexnum(mesh//'.NOMNOE', node_mast_nume(1)), node_mast_name)
!
! ------------- Get tangents
!
                tau1_node(1) = v_sdappa_tgno(6*(node_mast_indx(1)-1)+1)
                tau1_node(2) = v_sdappa_tgno(6*(node_mast_indx(1)-1)+2)
                tau1_node(3) = v_sdappa_tgno(6*(node_mast_indx(1)-1)+3)
                tau2_node(1) = v_sdappa_tgno(6*(node_mast_indx(1)-1)+4)
                tau2_node(2) = v_sdappa_tgno(6*(node_mast_indx(1)-1)+5)
                tau2_node(3) = v_sdappa_tgno(6*(node_mast_indx(1)-1)+6)
!
! ------------- Compute normal
!
                call mmnorm(model_ndim, tau1_node, tau2_node, normnd, noor2)
!
! ------------- Number of elements attached to node
!
                call cfnben(sdcont_defi, node_mast_indx(1), 'CONINV', node_nbelem, jdeciv)
!
! ------------- Loop on elements attached to node
!
                do i_elem = 1, node_nbelem
!
! ----------------- Current element
!   
                    call cfinvm(sdcont_defi, jdeciv, i_elem, elem_mast_indx)
                    call cfnumm(sdcont_defi, elem_mast_indx, elem_mast_nume)
                    call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
                    call cfnben(sdcont_defi, elem_mast_indx, 'CONNEX', elem_nbnode)
!
! ----------------- Access to connectivity
!
                    call jeveuo(jexnum(mesh//'.CONNEX', elem_mast_nume), 'L', vi = v_mesh_connex) 
!
! ----------------- Get current index of node
!
                    i_node_curr = 0
                    do i_elem_node = 1, elem_nbnode
                        if (v_mesh_connex(i_elem_node) .eq. node_mast_nume(1)) then
                            i_node_curr = i_elem_node
                        endif
                    end do
                    ASSERT(i_node_curr.ne.0)
!
! ----------------- Access to current tangent
!
                    call jeveuo(jexnum(sdappa_tgel, elem_mast_indx), 'L', vr = v_sdappa_tgel)
                    tau1(1) = v_sdappa_tgel(6*(i_node_curr-1)+1)
                    tau1(2) = v_sdappa_tgel(6*(i_node_curr-1)+2)
                    tau1(3) = v_sdappa_tgel(6*(i_node_curr-1)+3)
                    tau2(1) = v_sdappa_tgel(6*(i_node_curr-1)+4)
                    tau2(2) = v_sdappa_tgel(6*(i_node_curr-1)+5)
                    tau2(3) = v_sdappa_tgel(6*(i_node_curr-1)+6)
!
! ----------------- Compute normal
!
                    call mmnorm(model_ndim, tau1, tau2, norm, noor1)
!
! ----------------- Compute angle
!
                    prosca = ddot(3,norm,1,normnd,1)
                    if (abs(noor1*noor2) .gt. r8prem()) then
                        val = prosca/(noor1*noor2)
                        if (val .gt. 1.d0) then
                            val = 1.d0
                        endif
                        if (val .lt. -1.d0) then
                            val = -1.d0
                        endif
                        angle = acos(val)
                        angle = angle*r8rddg()
                        angl_old = v_sdappa_vera(node_mast_nume(1))
                        node_old = v_sdappa_verk(node_mast_nume(1))
                        if (angle .gt. angmax) then
                            if (node_old .eq. ' ') then
                                inoeu = inoeu+1
                                if (angl_old .lt. angle) then
                                   v_sdappa_verk(node_mast_nume(1)) = node_mast_name
                                   v_sdappa_vera(node_mast_nume(1)) = angle
                                endif
                            endif
                        endif
                    endif
                end do
            end do
        endif
    end do
!
    call jeecra(sdappa_verk, 'LONUTI', inoeu)
!
999 continue
!
    call jedema()
!
end subroutine
