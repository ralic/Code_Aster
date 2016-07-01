subroutine mmligr(mesh, model, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/initel.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmelem_data_c.h"
#include "asterfort/mmelem_data_l.h"
#include "asterfort/mmimp2.h"
#include "asterfort/mminfl.h"
#include "asterfort/mmlige.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Create late elements for contact (LIGREL)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_type, nb_cont_pair, nb_cont, nb_frot
    integer :: nb_node_elem, nb_node_mast, nb_node_slav, nb_grel, nt_node
    integer :: jv_liel, jtabf
    integer :: i_elem, i_cont_poin, i_cont_type, i_node, i_zone, i_cont_pair
    integer :: elem_mast_nume, elem_slav_nume, elem_indx
    integer :: ligrcf_liel_lont, typf_cont_nume, typg_cont_nume, typf_frot_nume  
    aster_logical :: l_cont_cont, l_cont_lac
    character(len=19) :: ligrcf, sdappa
    integer, pointer :: v_list_pair(:) => null()
    integer, pointer :: v_list_type(:) => null()
    aster_logical :: l_renumber, l_frot
    integer, pointer :: v_connex(:) => null()
    integer, pointer :: v_connex_lcum(:) => null()
    integer :: ztabf
    character(len=24) :: sdcont_tabfin
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    integer, pointer :: v_ligrcf_nbno(:) => null()
    integer, pointer :: v_ligrcf_nema(:) => null()
    integer, pointer :: v_ligrcf_liel(:) => null()
    character(len=24) :: sdappa_apli
    integer, pointer :: v_sdappa_apli(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
!
! - Re-create LIGREL or not ?
!
    l_renumber   = ds_contact%l_renumber
!
! - Get pairing datastructure
!
    sdappa       = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! - Get parameters
!
    l_cont_cont  = cfdisl(ds_contact%sdcont_defi, 'FORMUL_CONTINUE')
    l_cont_lac   = cfdisl(ds_contact%sdcont_defi, 'FORMUL_LAC')
!
! - Print
!
    if (l_renumber) then
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT> . Create late elements for contact'
        endif
    else
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT> . Don''t create late elements for contact'
        endif
        goto 999
    endif
!
! - Acces to mesh
!
    call jeveuo(mesh//'.CONNEX', 'L', vi = v_connex)
    call jeveuo(jexatr(mesh//'.CONNEX', 'LONCUM'), 'L', vi = v_connex_lcum)
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
! - Get <LIGREL> for contact elements
!
    ligrcf = ds_contact%ligrel_elem_cont
    call detrsd('LIGREL', ligrcf)
!
! - Create list of late elements for contact
!
    call mmlige(mesh        , ds_contact, &
                nb_cont_pair, v_list_pair,&
                nb_type     , v_list_type,&
                nt_node     , nb_grel    )
!
! - No late nodes
!
    call wkvect(ligrcf//'.NBNO', 'V V I', 1, vi = v_ligrcf_nbno)
    v_ligrcf_nbno(1) = 0
!
! - Create object NEMA
!
    call jecrec(ligrcf//'.NEMA', 'V V I', 'NU', 'CONTIG', 'VARIABLE', nb_cont_pair)
    call jeecra(ligrcf//'.NEMA', 'LONT', nt_node + nb_cont_pair)
    do i_cont_pair = 1, nb_cont_pair
!
! ----- Get parameters
!
        if (l_cont_cont) then
            i_cont_poin    = i_cont_pair
            elem_slav_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+2))
            elem_mast_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+3))
        else
            elem_slav_nume = v_sdappa_apli(3*(i_cont_pair-1)+1)
            elem_mast_nume = v_sdappa_apli(3*(i_cont_pair-1)+2)   
        endif
!
! ----- Get parameters for current contact element
!
        typg_cont_nume = v_list_pair(2*(i_cont_pair-1)+1)
        nb_node_elem   = v_list_pair(2*(i_cont_pair-1)+2)
!
! ----- Check number of nodes
!
        nb_node_slav   = v_connex_lcum(elem_slav_nume+1) - v_connex_lcum(elem_slav_nume)
        nb_node_mast   = v_connex_lcum(elem_mast_nume+1) - v_connex_lcum(elem_mast_nume)
        ASSERT(nb_node_elem .eq. (nb_node_mast+nb_node_slav))
!
! ----- Create contact element in LIGREL
!
        call jecroc(jexnum(ligrcf//'.NEMA', i_cont_pair))
        call jeecra(jexnum(ligrcf//'.NEMA', i_cont_pair), 'LONMAX', nb_node_elem+1)
        call jeveuo(jexnum(ligrcf//'.NEMA', i_cont_pair), 'E', vi = v_ligrcf_nema)
        v_ligrcf_nema(nb_node_elem+1) = typg_cont_nume
!
! ----- Copy slave nodes
!
        do i_node = 1, nb_node_slav
            v_ligrcf_nema(i_node) = v_connex(v_connex_lcum(elem_slav_nume)-1+i_node)
        end do
!
! ----- Copy master nodes
!
        do i_node = 1, nb_node_mast
            v_ligrcf_nema(nb_node_slav+i_node) = v_connex(v_connex_lcum(elem_mast_nume)-1+i_node)
        end do
    end do
!
! - Size of LIEL object
!
    ligrcf_liel_lont = nb_grel
    do i_cont_type = 1, nb_type
        elem_indx        = i_cont_type
        nb_cont          = v_list_type(5*(elem_indx-1)+1)
        nb_frot          = v_list_type(5*(elem_indx-1)+2)
        ligrcf_liel_lont = ligrcf_liel_lont + nb_cont + nb_frot
    end do
    ASSERT(nb_grel .gt. 0)
!
! - Create LIEL object
!
    call jecrec(ligrcf//'.LIEL', 'V V I', 'NU', 'CONTIG', 'VARIABLE', nb_grel)
    call jeecra(ligrcf//'.LIEL', 'LONT', ligrcf_liel_lont)
    i_elem = 0
    do i_cont_type = 1, nb_type
        elem_indx      = i_cont_type
        nb_cont        = v_list_type(5*(elem_indx-1)+1)
        nb_frot        = v_list_type(5*(elem_indx-1)+2)
        typf_cont_nume = v_list_type(5*(elem_indx-1)+3)
        typf_frot_nume = v_list_type(5*(elem_indx-1)+4)
        typg_cont_nume = v_list_type(5*(elem_indx-1)+5)

        if (nb_cont .ne. 0) then
!
! --------- Create new element
!
            i_elem = i_elem + 1
            call jecroc(jexnum(ligrcf//'.LIEL', i_elem))
            call jeecra(jexnum(ligrcf//'.LIEL', i_elem), 'LONMAX', nb_cont+1)
            call jeveuo(jexnum(ligrcf//'.LIEL', i_elem), 'E', vi = v_ligrcf_liel)
!
! --------- Add contact element
!
            v_ligrcf_liel(nb_cont+1) = typf_cont_nume
            jv_liel = 0
            do i_cont_pair = 1, nb_cont_pair
                if (v_list_pair(2*(i_cont_pair-1)+1) .eq. typf_cont_nume) then
                    if (l_cont_cont) then
                        i_cont_poin = i_cont_pair
                        i_zone      = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+14))
                        l_frot      = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE', i_zone )
                    else
                        l_frot      = .false._1 
                    endif
                    if (.not. l_frot) then
                        jv_liel                = jv_liel + 1
                        v_ligrcf_liel(jv_liel) = -i_cont_pair
                    endif
                endif
            end do
            ASSERT(jv_liel.eq.nb_cont)
        endif
        if (nb_frot .ne. 0) then
!
! --------- Create new element
!
            i_elem = i_elem + 1
            call jecroc(jexnum(ligrcf//'.LIEL', i_elem))
            call jeecra(jexnum(ligrcf//'.LIEL', i_elem), 'LONMAX', nb_frot+1)
            call jeveuo(jexnum(ligrcf//'.LIEL', i_elem), 'E', vi = v_ligrcf_liel)
!
! --------- Add friction element
!
            v_ligrcf_liel(nb_frot+1) = typf_frot_nume
            jv_liel = 0
            do i_cont_pair = 1, nb_cont_pair
                if (v_list_pair(2*(i_cont_pair-1)+1) .eq. typf_cont_nume) then
                    if (l_cont_cont) then
                        i_cont_poin = i_cont_pair
                        i_zone      = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+14))
                        l_frot      = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE', i_zone )
                    else
                        l_frot      = .false._1 
                    endif
                    if (l_frot) then
                        jv_liel                = jv_liel + 1
                        v_ligrcf_liel(jv_liel) = -i_cont_pair
                    endif
                endif
            end do
            ASSERT(jv_liel.eq.nb_frot)
        endif
    end do
    ASSERT(i_elem .eq. nb_grel)
!
! - Initialization of LIGREL
!
    call jedupo(model//'.MODELE    .LGRF', 'V', ligrcf//'.LGRF', .false._1)
    call adalig(ligrcf)
    call initel(ligrcf)
!
! - Print
!
    if (niv .ge. 2) then
        if (l_cont_cont) then
            call jeveuo(sdcont_tabfin, 'L', jtabf)
            call mmimp2(ifm, mesh, ligrcf, jtabf)
        endif
    endif
!
! - Clean
!
    AS_DEALLOCATE(vi = v_list_type)
    AS_DEALLOCATE(vi = v_list_pair)
!
999 continue
!
end subroutine
