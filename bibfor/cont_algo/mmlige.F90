subroutine mmlige(mesh        , ds_contact, &
                  nb_cont_pair, v_list_pair,&
                  nb_type     , v_list_type,&
                  nt_node     , nb_grel    )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
#include "asterfort/liglma.h"
#include "asterfort/mmelem_data_c.h"
#include "asterfort/mmelem_data_l.h"
#include "asterfort/mminfl.h"
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
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(out) :: nb_cont_pair
    integer, intent(out), pointer :: v_list_pair(:)
    integer, intent(out) :: nb_type
    integer, intent(out), pointer :: v_list_type(:)
    integer, intent(out) :: nt_node
    integer, intent(out) :: nb_grel
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Create list of late elements for contact
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! Out nb_cont_pair     : number of contact elements
! Out v_list_pair      : pointer for list of late elements
!      for v[1:nb_cont_pair,1] : index in element catalog for late element contact
!      for v[1:nb_cont_pair,2] : number of nodes for late element contact
! Out nb_type          : total number of contact elements defined
! Out v_list_type      : flag for contact element for each type
!      for v[1:nb_type,1] : number of contact element of this type
!      for v[1:nb_type,2] : number of friction element of this type
!      for v[1:nb_type,3] : index of finite element (contact) for this type
!      for v[1:nb_type,4] : index of finite element (friction) for this type
!      for v[1:nb_type,5] : index of geometry for this type
! Out nt_node          : total number of nodes
! Out nb_grel          : number of groups of elements (GREL)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_cont_pair, i_cont_type, i_cont_poin, i_zone, i_elem_slav
    integer :: nb_node_elem, nb_cont_poin, nb_elem_slav
    integer :: elem_mast_nume, elem_slav_nume
    integer :: model_ndim
    integer :: typg_cont_nume, elem_indx, typf_cont_nume, typf_frot_nume, typf_slav_nume
    character(len=19) :: ligrel_elem_slav
    integer :: typg_slav_nume, typg_mast_nume
    character(len=8) :: typg_slav_name, typg_mast_name, elem_slav_name, elem_mast_name
    character(len=16) :: typf_slav_name, typg_cont_name,typf_cont_name
    character(len=24) :: linuma = '&&MMLIGE.LINUMA'
    integer, pointer :: v_linuma(:) => null()
    character(len=24) :: linute = '&&MMLIGE.LINUTE'
    integer, pointer :: v_linute(:) => null()
    aster_logical :: l_frot, l_cont_cont, l_cont_lac, l_axi
    integer, pointer :: v_mesh_typmail(:) => null()
    integer :: ztabf
    character(len=24) :: sdcont_tabfin
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    character(len=24) :: sdappa_apli
    integer, pointer :: v_sdappa_apli(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
!
! - Initializations
!
    nt_node      = 0
    nb_grel      = 0
    nb_cont_pair = 0
!
! - Get contact parameters
!
    l_axi        = cfdisl(ds_contact%sdcont_defi, 'AXISYMETRIQUE')
    nb_cont_poin = cfdisi(ds_contact%sdcont_defi, 'NTPC')
    model_ndim   = cfdisi(ds_contact%sdcont_defi, 'NDIM')
    l_cont_cont  = cfdisl(ds_contact%sdcont_defi, 'FORMUL_CONTINUE')
    l_cont_lac   = cfdisl(ds_contact%sdcont_defi, 'FORMUL_LAC')
!
! - Access to mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = v_mesh_typmail)
!
! - Access to datastructure for contact solving
!
    if (l_cont_cont) then
        sdcont_tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
        call jeveuo(sdcont_tabfin, 'L', vr = v_sdcont_tabfin)
        ztabf = cfmmvd('ZTABF')
    else
        sdappa_apli = ds_contact%sdcont_solv(1:14)//'.APPA.APLI'
        call jeveuo(sdappa_apli, 'L', vi = v_sdappa_apli)
    endif
!
! - Get type of element for slave contact elements
!
    if (l_cont_lac) then
        ligrel_elem_slav = ds_contact%ligrel_elem_slav//'.CHME.LIGRE'
        call liglma(ligrel_elem_slav, nb_elem_slav, linuma, linute)
        call jeveuo(linuma, 'L', vi = v_linuma)
        call jeveuo(linute, 'L', vi = v_linute)
    endif
!
! - Number of contact elements
!
    nb_cont_pair = ds_contact%nb_cont_pair
!
! - Print
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... Total number of contact elements :', nb_cont_pair
    endif
!
! - Total number of contact elements defined
!
    if (l_cont_cont) then
        call mmelem_data_c(nb_cont_type_ = nb_type)
    else
        call mmelem_data_l(nb_cont_type_ = nb_type)
    endif
!
! - List of contact/friction elements detected
!
    AS_ALLOCATE(vi = v_list_type, size = 5*nb_type)
!
! - List of contact elements
!
    AS_ALLOCATE(vi = v_list_pair, size = 2*nb_cont_pair)
!
! - Loop on contact points (=contact elements)
!
    do i_cont_pair = 1, nb_cont_pair
!
! ----- Get parameters
!
        if (l_cont_cont) then
            i_cont_poin    = i_cont_pair
            i_zone         = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+14))
            l_frot         = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE', i_zone )
            elem_slav_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+2))
            elem_mast_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+3))
        else
            i_zone         = v_sdappa_apli(3*(i_cont_pair-1)+3)
            l_frot         = .false._1
            elem_slav_nume = v_sdappa_apli(3*(i_cont_pair-1)+1)
            elem_mast_nume = v_sdappa_apli(3*(i_cont_pair-1)+2)   
        endif
!
! ----- Type of slave/master element
!  
        typg_slav_nume = v_mesh_typmail(elem_slav_nume)
        typg_mast_nume = v_mesh_typmail(elem_mast_nume)
        call jenuno(jexnum('&CATA.TM.NOMTM', typg_slav_nume), typg_slav_name)
        call jenuno(jexnum('&CATA.TM.NOMTM', typg_mast_nume), typg_mast_name)
        call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
        call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_nume), elem_slav_name)
!
! ----- Identify contact element
!
        if (l_cont_cont) then
            call mmelem_data_c(l_axi_          = l_axi         , model_ndim_ = model_ndim    ,&
                               typg_slav_name_ = typg_slav_name, typg_mast_name_ = typg_mast_name,&
                               nb_node_elem_   = nb_node_elem  ,&
                               typg_cont_nume_ = typg_cont_nume,&
                               typf_cont_nume_ = typf_cont_nume,&
                               typf_frot_nume_ = typf_frot_nume,&
                               get_elem_indx_  = elem_indx)
        else
!
! --------- Index of FE type for slave element
!
            typf_slav_nume = 0
            typf_slav_name = ' '
            do i_elem_slav = 1, nb_elem_slav
                if (v_linuma(i_elem_slav) .eq. elem_slav_nume) then
                    typf_slav_nume = v_linute(i_elem_slav)
                    call jenuno(jexnum('&CATA.TE.NOMTE', typf_slav_nume), typf_slav_name)
                end if
            end do
            ASSERT(typf_slav_nume .ne. 0)
            call mmelem_data_l(l_axi_          = l_axi         ,&
                               typg_slav_name_ = typg_slav_name, typg_mast_name_ = typg_mast_name,&
                               typf_slav_name_ = typf_slav_name,&
                               nb_node_elem_   = nb_node_elem  ,&
                               typg_cont_nume_ = typg_cont_nume,&
                               typf_cont_nume_ = typf_cont_nume,&
                               get_elem_indx_  = elem_indx)

        endif
        call jenuno(jexnum('&CATA.TM.NOMTM', typg_cont_nume), typg_cont_name)
        call jenuno(jexnum('&CATA.TE.NOMTE', typf_cont_nume), typf_cont_name)
        if (niv .ge. 2) then
            WRITE(6,*) 'MMLIGE: ',i_cont_pair
            WRITE(6,*) 'MMLIGE - Master  : ',elem_mast_nume, elem_mast_name, typg_mast_name
            WRITE(6,*) 'MMLIGE - Slave   : ',elem_slav_nume, elem_slav_name, typg_slav_name
            WRITE(6,*) 'MMLIGE - Contact : ',nb_node_elem  , typg_cont_name, typf_cont_name
        endif
!
! ----- Save contact/friction element geometry parameters
!
        v_list_pair(2*(i_cont_pair-1)+1) = typf_cont_nume
        v_list_pair(2*(i_cont_pair-1)+2) = nb_node_elem
!
! ----- Save contact/friction element FE parameters
!  
        if (l_frot) then
            v_list_type(5*(elem_indx-1)+2) = v_list_type(5*(elem_indx-1)+2) + 1
            v_list_type(5*(elem_indx-1)+3) = typf_cont_nume
            v_list_type(5*(elem_indx-1)+4) = typf_frot_nume
        else
            v_list_type(5*(elem_indx-1)+1) = v_list_type(5*(elem_indx-1)+1) + 1
            v_list_type(5*(elem_indx-1)+3) = typf_cont_nume
        endif
        v_list_type(5*(elem_indx-1)+5) = typg_cont_nume
    end do
!
! - Total number of nodes
!
    do i_cont_type = 1, nb_type
        elem_indx = i_cont_type
        if (l_cont_cont) then
            call mmelem_data_c(set_elem_indx_ = elem_indx, nb_node_elem_ = nb_node_elem)
        else
            call mmelem_data_l(set_elem_indx_ = elem_indx, nb_node_elem_ = nb_node_elem)
        endif
        nt_node = nt_node + &
                  (v_list_type(5*(elem_indx-1)+1)+v_list_type(5*(elem_indx-1)+2))*(nb_node_elem)
    end do
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... Total number of nodes (slave+master) :', nt_node
    endif
!
! - Number of groups of elements (GREL)
!
    do i_cont_type = 1, nb_type
        elem_indx = i_cont_type
        if (v_list_type(5*(elem_indx-1)+1) .gt. 0) then
            nb_grel = nb_grel + 1
        endif
        if (v_list_type(5*(elem_indx-1)+2) .gt. 0) then
            nb_grel = nb_grel + 1
        endif
    end do
!
    call jedetr(linuma)
    call jedetr(linute)
!
end subroutine
