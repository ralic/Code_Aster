subroutine ddr_crid(mesh, nb_node_rid, v_list_rid, grelem_rid, grnode_int)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
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
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in)  :: mesh
    integer, intent(in)           :: nb_node_rid
    integer, intent(in)           :: v_list_rid(nb_node_rid)
    character(len=24), intent(in) :: grelem_rid
    character(len=24), intent(in) :: grnode_int
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_DOMAINE_REDUIT - Main process
!
! Construction of RID in mesh
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : mesh
! In  nb_node_rid      : number of nodes in RID
! In  v_list_rid       : list of nodes in RID
! In  grelem_rid       : name of GROUP_MA for RID
! In  grnode_int       : name of GROUP_NO for interface
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: group_name
    character(len=24) :: grpmai, gpptnm, grpmav, grpnoe, gpptnn, grpnov
    integer :: nb_group, nb_group_new, nb_enti
    integer :: nb_elem, nb_node, node_nbelem, node_nbelem2, elem_nbnode, elem_nbnode2
    integer :: i_group, i_enti
    integer :: nunolo, numamo, nunono
    integer :: i_elem, i_node, i_elem_node, i_node_elem, i_elem_node2, i_node_elem2, i_elem_node3
    integer :: nb_rid_elem, nb_int_node
    integer :: indx, iret, node_nume, elem_nume
    integer, pointer :: v_coninv(:) => null()
    integer, pointer :: v_coninv_longcum(:) => null()
    integer, pointer :: v_connex(:) => null()
    integer, pointer :: v_connex_longcum(:) => null()
    aster_logical :: test
    integer, pointer :: v_no_loca(:) => null()
    aster_logical, pointer :: v_list_ma(:) => null()
    aster_logical, pointer :: v_list_no(:) => null()
    aster_logical, pointer :: v_list_in(:) => null()
    integer, pointer :: v_list_old(:) => null()
    integer, pointer :: v_list_new(:) => null()
    integer, pointer :: v_group(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM4_21')
    endif
!
! - Initializations
!
    grpmai = mesh//'.GROUPEMA'
    gpptnm = mesh//'.PTRNOMMAI'
    grpmav = '&&OP0050'//'.GROUPEMA'
    grpnoe = mesh//'.GROUPENO'
    gpptnn = mesh//'.PTRNOMNOE'
    grpnov = '&&OP0050'//'.GROUPENO'
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
!
! - Add one GROUP_MA in mesh
!
    call jeexin(grpmai, iret)
    if (iret .eq. 0) then
        call jedetr(gpptnm)
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', 1, ' ')
        call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE', 1)
    else
        call jelira(grpmai, 'NOMUTI', nb_group)
        nb_group_new = nb_group + 1
        call cpclma(mesh, '&&OP0050', 'GROUPEMA', 'V')
        call jedetr(grpmai)
        call jedetr(gpptnm)
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', nb_group_new, ' ')
        call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE', nb_group_new)
        do i_group = 1, nb_group
            call jenuno(jexnum(grpmav, i_group), group_name)
            call jecroc(jexnom(grpmai, group_name))
            call jeveuo(jexnum(grpmav, i_group), 'L', vi = v_list_old)
            call jelira(jexnum(grpmav, i_group), 'LONUTI', nb_enti)
            call jeecra(jexnom(grpmai, group_name), 'LONMAX', max(nb_enti, 1))
            call jeecra(jexnom(grpmai, group_name), 'LONUTI', nb_enti)
            call jeveuo(jexnom(grpmai, group_name), 'E', vi = v_list_new)
            do i_enti = 1, nb_enti
                v_list_new(i_enti) = v_list_old(i_enti)
            enddo
        enddo
    endif    
!
! - Add one GROUP_NO in mesh
!
    call jeexin(grpnoe, iret)
    if (iret .eq. 0) then
        call jedetr(gpptnn)
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', 1, ' ')
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE', 1)
    else
        call jelira(grpnoe, 'NOMUTI', nb_group)
        nb_group_new = nb_group + 1
        call cpclma(mesh, '&&OP0050', 'GROUPENO', 'V')
        call jedetr(grpnoe)
        call jedetr(gpptnn)
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', nb_group_new, ' ')
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE', nb_group_new)
        do i_group = 1, nb_group
            call jenuno(jexnum(grpnov, i_group), group_name)
            call jecroc(jexnom(grpnoe, group_name))
            call jeveuo(jexnum(grpnov, i_group), 'L', vi = v_list_old)
            call jelira(jexnum(grpnov, i_group), 'LONUTI', nb_enti)
            call jeecra(jexnom(grpnoe, group_name), 'LONMAX', max(nb_enti, 1))
            call jeecra(jexnom(grpnoe, group_name), 'LONUTI', nb_enti)
            call jeveuo(jexnom(grpnoe, group_name), 'E', vi = v_list_new)
            do i_enti = 1, nb_enti
                v_list_new(i_enti) = v_list_old(i_enti)
            enddo
        enddo
    endif
!
! - Get all elements of RID
!
    do i_node = 1, nb_node_rid
! ----- 1/ Add magic point in RID_Nodes
        node_nume            = v_list_rid(i_node)
        v_list_no(node_nume) = .true._1   
        node_nbelem          = v_coninv_longcum(node_nume+1)-v_coninv_longcum(node_nume)
        do i_node_elem = 1, node_nbelem
! --------- 2/ Add element attached to magic point in RID_Elements
            elem_nume            = v_coninv(v_coninv_longcum(node_nume)+i_node_elem-1)
            v_list_ma(elem_nume) = .true._1
            elem_nbnode          = v_connex_longcum(elem_nume+1)-v_connex_longcum(elem_nume)
            AS_ALLOCATE(vi = v_no_loca, size = elem_nbnode)
            do i_elem_node = 1, elem_nbnode
! ------------- 3/ Add node attached to RID_Elements in RID_Nodes
                nunolo                 = v_connex(v_connex_longcum(elem_nume)+i_elem_node-1)
                v_list_no(nunolo)      = .true._1
                v_no_loca(i_elem_node) = nunolo
            enddo
            do i_elem_node = 1, elem_nbnode
! ------------- 3/ Add element attached to RID_Nodes
                nunolo       = v_connex(v_connex_longcum(elem_nume)+i_elem_node-1)
                node_nbelem2 = v_coninv_longcum(nunolo+1)-v_coninv_longcum(nunolo)
                do i_node_elem2 = 1, node_nbelem2
                    numamo       = v_coninv(v_coninv_longcum(nunolo)+i_node_elem2-1)
                    elem_nbnode2 = v_connex_longcum(numamo+1)-v_connex_longcum(numamo)
                    do i_elem_node2 = 1, elem_nbnode2
                        nunono = v_connex(v_connex_longcum(numamo)+i_elem_node2-1)
                        test   = .false._1
                        do i_elem_node3 = 1, elem_nbnode
                            if (nunono .eq. v_no_loca(i_elem_node3)) then
                                test = .true._1
                                exit
                            endif
                        enddo
                        if (.not.test) then
                            exit
                        endif
                    enddo
                    if (test) then
                        v_list_ma(numamo) = .true._1
                    endif   
                enddo
            enddo
            AS_DEALLOCATE(vi=v_no_loca)
        enddo
    enddo 
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
    indx = 0
    do i_node = 1, nb_node
        if (v_list_in(i_node)) then
            indx = indx + 1
            v_group(indx) = i_node
        endif
    enddo
!
! - Clean
!
    AS_DEALLOCATE(vl=v_list_ma)
    AS_DEALLOCATE(vl=v_list_no)
    AS_DEALLOCATE(vl=v_list_in)
!
end subroutine
