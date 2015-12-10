subroutine mmlige(mesh      , ds_contact, v_list_elem, nb_cont_type, v_cnt_cont,&
                  v_cnt_frot, nt_node   , nb_grel    , nb_cont_elem)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmelem_data.h"
#include "asterfort/mminfl.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(out), pointer :: v_list_elem(:)
    integer, intent(out) :: nb_cont_type
    integer, intent(out), pointer :: v_cnt_cont(:)
    integer, intent(out), pointer :: v_cnt_frot(:)
    integer, intent(out) :: nt_node
    integer, intent(out) :: nb_grel
    integer, intent(out) :: nb_cont_elem
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
! Out v_list_elem      : pointer for list of late elements
!      for v[1:nb_cont_elem,1] : index in element catalog for late element contact
!      for v[1:nb_cont_elem,2] : number of nodes for late element contact
! Out nb_cont_type     : total number of contact elements defined
! Out v_cnt_cont       : flag for contact element for each type  [1:nb_cont_type]
! Out v_cnt_frot       : flag for friction element for each type [1:nb_cont_type]
! Out nt_node          : total number of nodes
! Out nb_grel          : number of groups of elements (GREL)
! Out nb_cont_elem     : number of contact elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_cont_elem, i_cont_type, i_cont_poin
    integer :: nb_node_elem, nb_cont_poin
    integer :: elem_mast_nume, elem_slav_nume, i_zone
    integer :: model_ndim
    integer :: cont_geom_nume, cont_indx
    character(len=8) :: slav_type_name, mast_type_name
    integer :: slav_type_nume, mast_type_nume
    integer :: ifm, niv
    aster_logical :: l_frot, l_cont_cont
    integer, pointer :: v_mesh_typmail(:) => null()
    integer :: ztabf
    character(len=24) :: sdcont_tabfin
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
!
! - Initializations
!
    nt_node      = 0
    nb_grel      = 0
    nb_cont_elem = 0
!
! - Get contact parameters
!
    nb_cont_poin = cfdisi(ds_contact%sdcont_defi, 'NTPC')
    model_ndim   = cfdisi(ds_contact%sdcont_defi, 'NDIM')
    l_cont_cont  = cfdisl(ds_contact%sdcont_defi, 'FORMUL_CONTINUE')
!
! - Datastructure for contact solving
!
    sdcont_tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    call jeveuo(sdcont_tabfin, 'L', vr = v_sdcont_tabfin)
    ztabf = cfmmvd('ZTABF')
!
! - Access to mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = v_mesh_typmail)
!
! - Get number of contact elements
!
    nb_cont_elem = nb_cont_poin
!
! - Print
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... Total number of contact elements :', nb_cont_elem
    endif
!
! - Total number of late elements defined
!
    call mmelem_data(cont_indx, nb_cont_type_ = nb_cont_type)
    AS_ALLOCATE(vi = v_cnt_cont, size = nb_cont_type)
    AS_ALLOCATE(vi = v_cnt_frot, size = nb_cont_type)
!
! - List of contact elements
!
    AS_ALLOCATE(vi = v_list_elem, size = 2*nb_cont_elem)
!
! - Loop on contact points (=contact elements)
!
    do i_cont_elem = 1, nb_cont_elem
!
! ----- Get parameters
!
        i_cont_poin    = i_cont_elem
        i_zone         = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+14))
        l_frot         = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE', i_zone )
        elem_slav_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+2))
        elem_mast_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+3))
!
! ----- Type of slave/master element
!  
        slav_type_nume = v_mesh_typmail(elem_slav_nume)
        mast_type_nume = v_mesh_typmail(elem_mast_nume)
        call jenuno(jexnum('&CATA.TM.NOMTM', slav_type_nume), slav_type_name)
        call jenuno(jexnum('&CATA.TM.NOMTM', mast_type_nume), mast_type_name)
!
! ----- Contact element
!
        call mmelem_data(cont_indx       = cont_indx     ,&
                         model_ndim_     = model_ndim    ,&
                         elem_1_         = slav_type_name, elem_2_ = mast_type_name,&
                         nb_node_elem_   = nb_node_elem  ,&
                         cont_geom_nume_ = cont_geom_nume)
        v_list_elem(2*(i_cont_elem-1)+1) = cont_geom_nume
        v_list_elem(2*(i_cont_elem-1)+2) = nb_node_elem
!
! ----- Friction/contact count
!
        if (l_frot) then
            v_cnt_frot(cont_indx) = v_cnt_frot(cont_indx) + 1
        else
            v_cnt_cont(cont_indx) = v_cnt_cont(cont_indx) + 1
        endif
    end do
!
! - Total number of nodes
!
    do i_cont_type = 1, nb_cont_type
        cont_indx = i_cont_type
        call mmelem_data(cont_indx, nb_node_elem_ = nb_node_elem)
        nt_node = nt_node + (v_cnt_cont(i_cont_type)+v_cnt_frot(i_cont_type))*(nb_node_elem+1)
    end do
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... Total number of nodes (slave+master) :', nt_node
    endif
!
! - Number of groups of elements (GREL)
!
    do i_cont_type = 1, nb_cont_type
        if (v_cnt_cont(i_cont_type) .gt. 0) then
            nb_grel = nb_grel + 1
        endif
        if (v_cnt_frot(i_cont_type) .gt. 0) then
            nb_grel = nb_grel + 1
        endif
    end do
!
end subroutine
