subroutine aptgem(sdappa , mesh     , newgeo   , sdcont_defi, model_ndim,&
                  i_zone , zone_type, iter_maxi, epsi_maxi  , jdecma    ,&
                  nb_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8maem.h"
#include "asterfort/apcoma.h"
#include "asterfort/apcond.h"
#include "asterfort/apcpoi.h"
#include "asterfort/apcpou.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/aptypm.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmctan.h"
#include "asterfort/mmtann.h"
#include "asterfort/utmess.h"
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
    character(len=19), intent(in) :: newgeo
    integer, intent(in) :: model_ndim
    integer, intent(in) :: i_zone   
    integer, intent(in) :: jdecma    
    integer, intent(in) :: nb_elem
    character(len=4), intent(in) :: zone_type
    integer, intent(in) :: iter_maxi
    real(kind=8), intent(in) :: epsi_maxi
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Compute tangents at each node for each element - on current zone
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  model_ndim       : dimension of model
! In  i_zone           : index of contact zone
! In  jdecma           : shift in contact datastructure for the beginning of element in contact zone
! In  nb_elem          : number of elements in contact zone
! In  zone_type        : type of zone
!                        'MAIT' for master
!                        'ESCL' for slave
! In  iter_maxi        : maximum number of Newton iterations
! In  epsi_maxi        : maximum tolerance for Newton algorithm
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: elem_type, elem_name, node_name, valk(2)
    integer :: node_nume(9), longc
    integer :: elem_nbnode, niverr
    aster_logical :: l_beam, l_poi1
    integer :: i_node, i_elem, elem_ndim
    integer :: elem_indx, elem_nume
    real(kind=8) :: tau1(3), tau2(3)
    character(len=24) :: sdappa_tgel
    real(kind=8) :: elem_coor(27), node_coor(3)
    real(kind=8), pointer :: v_sdappa_tgel(:) => null()
    integer, pointer :: v_mesh_connex(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Acces to pairing datastructure
!
    sdappa_tgel = sdappa(1:19)//'.TGEL'
!
! - Loop on elements
!
    do i_elem = 1, nb_elem
!
! ----- Current element
!
        elem_indx = i_elem+jdecma
        call cfnumm(sdcont_defi, elem_indx, elem_nume)
!
! ----- Number of nodes
!
        call cfnben(sdcont_defi, elem_indx, 'CONNEX', elem_nbnode)
!
! ----- Parameters of current element
!
        call aptypm(mesh     , elem_nume, elem_ndim, elem_nbnode, elem_type, &
                    elem_name)
!
! ----- Coordinates of current element
!
        call apcoma(mesh, newgeo, elem_nume, elem_nbnode, elem_coor)
!
! ----- Get absolute index of nodes
!
        call jeveuo(jexnum(mesh//'.CONNEX', elem_nume), 'L', vi = v_mesh_connex)
        do i_node = 1, elem_nbnode
            node_nume(i_node) = v_mesh_connex(i_node)
        end do
!
! ----- Right length
!
        call jelira(jexnum(sdappa_tgel, elem_indx), 'LONMAX', longc)
        longc = longc /6
!
! ----- Special types of element
!
        l_beam = (elem_type(1:2).eq.'SE').and.(model_ndim.eq.3)
        l_poi1 = elem_type.eq.'PO1'
!
! ----- Current element
!
        call jeveuo(jexnum(sdappa_tgel, elem_indx), 'E', vr = v_sdappa_tgel)
!
! ----- Loop on nodes
!
        do i_node = 1, elem_nbnode
            tau1(1:3) = r8maem()
            tau2(1:3) = r8maem()
!
! --------- Current node
!
            call apcond(newgeo, node_nume(i_node), node_coor)
            call jenuno(jexnum(mesh//'.NOMNOE', node_nume(i_node)), node_name)
            valk(1) = elem_name
            valk(2) = node_name
!
! --------- Compute local basis for these node
!
            if (l_poi1) then
                call apcpoi(sdcont_defi, model_ndim, i_zone, elem_name,&
                            zone_type, tau1       , tau2)
            else
                call mmctan(elem_name, elem_type, elem_nbnode, elem_ndim, elem_coor,&
                            node_coor, iter_maxi, epsi_maxi  , tau1, tau2)
                if (l_beam) then
                    call apcpou(sdcont_defi, i_zone, elem_name, zone_type,&
                                tau1       , tau2)
                endif
            endif
!
! --------- Norm
!
            call mmtann(model_ndim, tau1, tau2, niverr)
            if (niverr .eq. 1) then
                call utmess('F', 'APPARIEMENT_14', nk=2, valk=valk)
            endif
!
! --------- Save tangents (careful: for QUAD8 only 4 nodes in contact datastructures!)
!
            if (i_node .le. longc) then
                v_sdappa_tgel(6*(i_node-1)+1) = tau1(1)
                v_sdappa_tgel(6*(i_node-1)+2) = tau1(2)
                v_sdappa_tgel(6*(i_node-1)+3) = tau1(3)
                v_sdappa_tgel(6*(i_node-1)+4) = tau2(1)
                v_sdappa_tgel(6*(i_node-1)+5) = tau2(2)
                v_sdappa_tgel(6*(i_node-1)+6) = tau2(3)
            endif
        end do
    end do
!
    call jedema()
end subroutine
