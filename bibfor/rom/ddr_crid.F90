subroutine ddr_crid(ds_para, nb_node_rid, v_list_rid)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/cpclma.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/addGroupElem.h"
#include "asterfort/addGroupNode.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_ParaDDR), intent(in) :: ds_para
    integer, intent(in)           :: nb_node_rid
    integer, intent(in)           :: v_list_rid(nb_node_rid)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_DOMAINE_REDUIT - Main process
!
! Construction of RID in mesh
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters of EIM computation
! In  nb_node_rid      : number of nodes in RID
! In  v_list_rid       : list of nodes in RID
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_elem, nb_node, node_nbelem, elem_nbnode
    integer :: nunolo
    integer :: i_layer
    integer :: i_elem, i_node, i_elem_node, i_node_elem
    integer :: nb_rid_elem, nb_int_node, nb_group_add, nb_sub_node
    integer :: indx, node_nume, elem_nume
    integer :: nb_layer_ma
    character(len=8) :: mesh
    character(len=24):: grelem_rid, grnode_int, grnode_sub
    integer, pointer :: v_coninv(:) => null()
    integer, pointer :: v_coninv_longcum(:) => null()
    integer, pointer :: v_connex(:) => null()
    integer, pointer :: v_connex_longcum(:) => null()
    aster_logical :: test, l_corr_ef
    aster_logical, pointer :: v_list_ma(:) => null()
    aster_logical, pointer :: v_list_no(:) => null()
    aster_logical, pointer :: v_loca_no(:) => null()
    aster_logical, pointer :: v_list_in(:) => null()
    aster_logical, pointer :: v_list_sb(:) => null()
    aster_logical, pointer :: v_loca_sb(:) => null()
    integer, pointer :: v_group(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM4_21')
    endif
!
! - Get parameters
!
    mesh         = ds_para%mesh
    nb_layer_ma  = ds_para%nb_layer_ma
    grelem_rid   = ds_para%grelem_rid
    grnode_int   = ds_para%grnode_int
    l_corr_ef    = ds_para%l_corr_ef
    grnode_sub   = ds_para%grnode_sub
!
! - Initializations
!
    call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', nb_elem)
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', nb_node)
!
! - Access to mesh
!
    call jeveuo(mesh//'.CONNEX', 'L', vi=v_connex)
    call jeveuo(jexatr(mesh//'.CONNEX', 'LONCUM'), 'L', vi = v_connex_longcum)
!
! - Access to inverse connectivity
!
    call cncinv(mesh, [0], 0, 'V', '&&MESHMA')
    call jeveuo('&&MESHMA', 'L', vi = v_coninv)
    call jeveuo(jexatr('&&MESHMA', 'LONCUM'), 'L', vi = v_coninv_longcum)
!
! - Create working objects
!
    AS_ALLOCATE(vl = v_list_ma, size = nb_elem)
    AS_ALLOCATE(vl = v_list_no, size = nb_node)
    AS_ALLOCATE(vl = v_list_in, size = nb_node)
    AS_ALLOCATE(vl = v_loca_no, size = nb_node)
    AS_ALLOCATE(vl = v_list_sb, size = nb_node)
    AS_ALLOCATE(vl = v_loca_sb, size = nb_node)
!
! - Add one GROUP_MA in mesh
!
    call addGroupElem(mesh, 1) 
!
! - Add one GROUP_NO in mesh
!
    if (ds_para%l_corr_ef) then
        nb_group_add = 2
    else
        nb_group_add = 1
    endif
    call addGroupNode(mesh, nb_group_add) 
!
! - Get all elements of RID
!
    do i_node = 1, nb_node_rid
        node_nume = v_list_rid(i_node)
        ASSERT(node_nume .gt. 0)
        v_list_no(node_nume) = .true._1
        v_loca_no(node_nume) = .true._1
    end do
    do i_layer = 1, nb_layer_ma+1
        do i_node = 1, nb_node
            if (v_list_no(i_node)) then
                node_nbelem = v_coninv_longcum(i_node+1)-v_coninv_longcum(i_node)
                do i_node_elem = 1, node_nbelem
                    elem_nume = v_coninv(v_coninv_longcum(i_node)+i_node_elem-1)
                    elem_nbnode = v_connex_longcum(elem_nume+1)-v_connex_longcum(elem_nume)
                    do i_elem_node = 1, elem_nbnode
                        nunolo = v_connex(v_connex_longcum(elem_nume)+i_elem_node-1)
                        v_loca_no(nunolo) = .true._1
                    end do
                end do
            endif
        end do
        do i_node = 1, nb_node
            v_list_no(i_node) = v_loca_no(i_node)
        end do
    end do
    do i_elem = 1, nb_elem
        elem_nbnode = v_connex_longcum(i_elem+1)-v_connex_longcum(i_elem)
        test = .false._1
        do i_elem_node = 1, elem_nbnode
            nunolo = v_connex(v_connex_longcum(i_elem)+i_elem_node-1)
            if (v_list_no(nunolo)) then
                test=.true._1
            else
                test=.false._1
                exit
            endif
        end do
        if (test) then
            v_list_ma(i_elem) = .true._1
        endif
    end do
!
! - Number of elements in RID
!
    nb_rid_elem = 0
    do i_elem = 1, nb_elem
        if (v_list_ma(i_elem)) then
            nb_rid_elem = nb_rid_elem + 1
        endif
    end do
    if (niv .ge. 2) then
        call utmess('I', 'ROM4_22', si = nb_rid_elem)
    endif
!
! - Create group for elements of RID
!
    call jecroc(jexnom(mesh//'.GROUPEMA', grelem_rid))
    call jeecra(jexnom(mesh//'.GROUPEMA', grelem_rid), 'LONMAX', max(1, nb_rid_elem))
    call jeecra(jexnom(mesh//'.GROUPEMA', grelem_rid), 'LONUTI', nb_rid_elem)
    call jeveuo(jexnom(mesh//'.GROUPEMA', grelem_rid), 'E', vi = v_group)
    indx = 0
    do i_elem = 1, nb_elem
        if (v_list_ma(i_elem)) then
            indx = indx + 1
            v_group(indx) = i_elem
        endif
    enddo
!
! - Create list of nodes of interface
!
    do i_elem = 1, nb_elem
        if (.not.v_list_ma(i_elem)) then     
            elem_nbnode = v_connex_longcum(i_elem+1)-v_connex_longcum(i_elem)
            do i_elem_node = 1, elem_nbnode
                nunolo = v_connex(v_connex_longcum(i_elem)+i_elem_node-1)
                if (v_list_no(nunolo)) then
                    v_list_in(nunolo) = .true._1
                    v_list_sb(nunolo) = .true._1
                endif
            enddo    
        endif
    enddo
!
! - Number of nodes of interface
!
    nb_int_node = 0
    do i_node = 1, nb_node
        if (v_list_in(i_node)) then
            nb_int_node = nb_int_node + 1
        endif
    enddo
    if (niv .ge. 2) then
        call utmess('I', 'ROM4_23', si = nb_int_node)
    endif
!
! - Create group for nodes of interface
!
    call jecroc(jexnom(mesh//'.GROUPENO', grnode_int))
    call jeecra(jexnom(mesh//'.GROUPENO', grnode_int), 'LONMAX', max(1, nb_int_node))
    call jeecra(jexnom(mesh//'.GROUPENO', grnode_int), 'LONUTI', nb_int_node)
    call jeveuo(jexnom(mesh//'.GROUPENO', grnode_int), 'E', vi = v_group)
    if (nb_int_node .eq. 0) then
        call utmess('A', 'ROM4_27')
    endif
    indx = 0
    do i_node = 1, nb_node
        if (v_list_in(i_node)) then
            indx = indx + 1
            v_group(indx) = i_node
        endif
    enddo
!
! - Create list of nodes for EF correctors
!
    do i_node = 1, nb_node
        if (v_list_sb(i_node)) then
            node_nbelem = v_coninv_longcum(i_node+1)-v_coninv_longcum(i_node)
            do i_node_elem = 1, node_nbelem
                elem_nume = v_coninv(v_coninv_longcum(i_node)+i_node_elem-1)
                if (.not.v_list_ma(elem_nume)) then
                    elem_nbnode = v_connex_longcum(elem_nume+1)-v_connex_longcum(elem_nume)
                    do i_elem_node = 1, elem_nbnode
                        nunolo = v_connex(v_connex_longcum(elem_nume)+i_elem_node-1)
                        v_loca_sb(nunolo) = .true._1
                    enddo
                endif
            enddo
        endif
    enddo
    do i_node = 1, nb_node
        v_list_sb(i_node) = v_loca_sb(i_node)
    enddo
!
! - Number of nodes for EF correcteurs
!
    nb_sub_node = 0
    do i_node = 1, nb_node
        if (v_list_sb(i_node)) then
            nb_sub_node = nb_sub_node + 1
        endif
    enddo
    if (ds_para%l_corr_ef .and. nb_sub_node .eq. 0) then
        call utmess('A', 'ROM4_28')
    endif
    if (niv .ge. 2 .and. l_corr_ef) then
        call utmess('I', 'ROM4_26', si = nb_sub_node)
    endif
!
! - Create group for nodes of rigid zone in EF correctors
!
    if (l_corr_ef) then
        call jecroc(jexnom(mesh//'.GROUPENO', grnode_sub))
        call jeecra(jexnom(mesh//'.GROUPENO', grnode_sub), 'LONMAX', max(1, nb_sub_node))
        call jeecra(jexnom(mesh//'.GROUPENO', grnode_sub), 'LONUTI', nb_sub_node)
        call jeveuo(jexnom(mesh//'.GROUPENO', grnode_sub), 'E', vi = v_group)
        indx = 0
        do i_node = 1, nb_node
            if (v_list_sb(i_node)) then
                indx = indx + 1
                v_group(indx) = i_node
            endif
        enddo
    endif
!
! - Clean
!
    AS_DEALLOCATE(vl=v_list_ma)
    AS_DEALLOCATE(vl=v_list_no)
    AS_DEALLOCATE(vl=v_list_in)
    AS_DEALLOCATE(vl=v_list_sb)
    AS_DEALLOCATE(vl=v_loca_sb)
    AS_DEALLOCATE(vl=v_loca_no)
!
end subroutine
