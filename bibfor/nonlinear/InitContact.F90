subroutine InitContact(mesh, model, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jeexin.h"
#include "asterfort/xrela_elim.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: model
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Contact management
!
! Initializations for contact management
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: cont_form
    character(len=8) :: sdcont
    character(len=24) :: sdcont_defi, sdunil_defi
    character(len=24) :: iden_rela
    aster_logical :: l_cont, l_unil
    aster_logical :: l_form_disc, l_form_cont, l_form_xfem, l_form_lac
    aster_logical :: l_cont_xfem_gg, l_edge_elim, l_all_verif
    integer :: i_exist
    character(len=8), pointer :: v_load_type(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
!
! - Initializations
!
    l_cont      = .false._1
    l_unil      = .false._1
    l_form_disc = .false._1
    l_form_cont = .false._1
    l_form_xfem = .false._1
    l_form_lac  = .false._1
! 
    if (ds_contact%l_contact) then
!
! ----- Print
!
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... Initializations for contact management'
        endif
!
! ----- Datastructure from DEFI_CONTACT
!
        sdcont = ds_contact%sdcont
!
! ----- Define datastructure names
!
        sdcont_defi = sdcont(1:8)//'.CONTACT'
        sdunil_defi = sdcont(1:8)//'.UNILATE'
!
! ----- Contact formulation
! 
        cont_form      = cfdisi(sdcont_defi, 'FORMULATION')
        ASSERT(cont_form.ge.1 .and. cont_form.le.5)
        l_form_disc    = cont_form .eq. 1
        l_form_cont    = cont_form .eq. 2
        l_form_xfem    = cont_form .eq. 3
        l_unil         = cont_form .eq. 4
        l_form_lac     = cont_form .eq. 5
        l_cont         = cont_form .ne. 4
        l_cont_xfem_gg = cfdisl(sdcont_defi, 'CONT_XFEM_GG')
        l_edge_elim    = cfdisl(sdcont_defi, 'ELIM_ARETE')
        l_all_verif    = cfdisl(sdcont_defi, 'ALL_VERIF')  
!
! ----- Field for CONT_NODE
!
        if (l_form_cont .or. l_form_disc .or. l_form_xfem) then
            ds_contact%field_cont_node  = '&&CFMXR0.CNOINR'
            ds_contact%fields_cont_node = '&&CFMXR0.CNSINR'
            ds_contact%field_cont_perc  = '&&CFMXR0.CNSPER'
        endif
!
! ----- Special for discrete contact
!
        if (l_form_disc) then
            ds_contact%nume_dof_frot = '&&CFMXSD.NUMDF'
            call jeexin(sdcont(1:8)//'.CHME.LIGRE.LGRF', i_exist)
            ds_contact%l_dof_rela = i_exist .gt. 0
            if (i_exist .gt. 0) then
                ds_contact%ligrel_dof_rela = sdcont
            endif
        endif 
!
! ----- Special for continue contact
!
        if (l_form_cont) then
            ds_contact%field_input      = ds_contact%sdcont_solv(1:14)//'.CHML'
            ds_contact%l_elem_slav      = .true.
            ds_contact%ligrel_elem_slav = sdcont
            ds_contact%l_elem_cont      = .true.
            ds_contact%ligrel_elem_cont = '&&LIGRCF.CHME.LIGRE'
            call wkvect(ds_contact%ligrel_elem_cont(1:8)//'.TYPE', 'V V K8', 1, vk8 = v_load_type)
            v_load_type(1) = 'ME'
        endif
!
! ----- Special for xfem contact
!
        if (l_form_xfem) then
            ds_contact%field_input = ds_contact%sdcont_solv(1:14)//'.CHML'
            if (l_edge_elim) then
                call xrela_elim(mesh, sdcont_defi, iden_rela)
                call jeexin(iden_rela(1:19)//'.INFO', i_exist)
                if (i_exist .gt. 0) then
                    ds_contact%iden_rela   = iden_rela
                    ds_contact%l_iden_rela = .true._1
                endif
            else
                call jeexin(sdcont(1:8)//'.CHME.LIGRE.LGRF', i_exist)
                ds_contact%l_dof_rela = i_exist .gt. 0
                if (i_exist .gt. 0) then
                    ds_contact%ligrel_dof_rela = sdcont
                endif
            endif
            if (l_cont_xfem_gg) then  
                ds_contact%ligrel_elem_cont = model(1:8)//'.MODELE'
            endif
            ds_contact%l_elem_cont      = .false.
            ds_contact%ligrel_elem_cont = model(1:8)//'.MODELE'
        endif
!
! ----- Special for xfem contact (large sliding)
!
        if (l_cont_xfem_gg) then          
            ds_contact%l_elem_cont      = .true.
            ds_contact%ligrel_elem_cont = '&&LIGRXF.CHME.LIGRE'
            call wkvect(ds_contact%ligrel_elem_cont(1:8)//'.TYPE', 'V V K8', 1, vk8 = v_load_type)
            v_load_type(1) = 'ME'
            if (ds_contact%l_dof_rela) then
                ds_contact%ligrel_elem_slav = sdcont
                ds_contact%l_elem_slav      = .false.
            else
                ds_contact%ligrel_elem_slav = sdcont
                ds_contact%l_elem_slav      = .true.
            endif
        endif
!
! ----- Special for LAC contact
!
        if (l_form_lac) then
            ASSERT(.false.)
        endif
!
! ----- Flag for (re) numbering
!
        if (l_form_cont) then
            if (l_all_verif) then
                ds_contact%l_renumber = .false._1
            else
                ds_contact%l_renumber = .true._1
            endif
        endif
!
! ----- Flag for pairing
!
        if (l_form_disc) then
            ds_contact%l_pair       = .true._1
            ds_contact%l_first_geom = .true._1
        endif
!
! ----- Save parameters
!
        ds_contact%sdcont_defi = sdcont(1:8)//'.CONTACT'
        ds_contact%sdunil_defi = sdcont(1:8)//'.UNILATE'
        ds_contact%l_meca_cont = l_cont
        ds_contact%l_meca_unil = l_unil
        ds_contact%l_form_cont = l_form_cont
        ds_contact%l_form_disc = l_form_disc
        ds_contact%l_form_xfem = l_form_xfem
        ds_contact%l_form_lac  = l_form_lac
    endif
!
end subroutine
