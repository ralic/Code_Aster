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
    integer :: nb_cont_type, nb_cont_elem
    integer :: ico, jco, i_cont_poin, i_cont_type, i_node, i_zone, jtabf, i_cont_elem
    integer :: elem_mast_nume, elem_slav_nume, cont_indx
    integer :: ligrcf_liel_lont, cont_elem_nume, cont_geom_nume, frot_elem_nume
    integer :: nb_node_elem, nb_node_mast, nb_node_slav, nb_grel, nt_node
    aster_logical :: l_cont_cont, l_cont_lac
    character(len=8) :: cont_geom_name, cont_elem_name, frot_elem_name
    character(len=19) :: ligrcf
    integer, pointer :: v_list_elem(:) => null()
    integer, pointer :: v_cnt_cont(:) => null()
    integer, pointer :: v_cnt_frot(:) => null()
    aster_logical :: l_pair, l_axi, l_frot
    integer, pointer :: v_connex(:) => null()
    integer, pointer :: v_connex_lcum(:) => null()
    integer :: ztabf
    character(len=24) :: sdcont_tabfin
    real(kind=8), pointer :: v_sdcont_tabfin(:) => null()
    character(len=24) :: sdcont_crnudd
    aster_logical, pointer :: v_sdcont_crnudd(:) => null()
    integer, pointer :: v_ligrcf_nbno(:) => null()
    integer, pointer :: v_ligrcf_nema(:) => null()
    integer, pointer :: v_ligrcf_liel(:) => null()
!   character(len=24) :: sdcont_aplist
!   integer, pointer :: v_sdcont_aplist(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
!
! - Datastructure for contact solving
!
    sdcont_crnudd = ds_contact%sdcont_solv(1:14)//'.NUDD'
    call jeveuo(sdcont_crnudd, 'L', vl   = v_sdcont_crnudd)
!
! - Pairing or not ?
!
    l_pair = v_sdcont_crnudd(1)
!
! - Get parameters
!
    l_axi        = cfdisl(ds_contact%sdcont_defi, 'AXISYMETRIQUE')
    l_cont_cont  = cfdisl(ds_contact%sdcont_defi, 'FORMUL_CONTINUE')
    l_cont_lac   = .false._1
!   l_cont_lac   = cfdisl(ds_contact%sdcont_defi, 'FORMUL_LAC')
!
! - Print
!
    if (l_pair) then
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
! - Access to contact elements
!
    if (l_cont_cont) then
        sdcont_tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
        call jeveuo(sdcont_tabfin, 'L', vr   = v_sdcont_tabfin)
        ztabf = cfmmvd('ZTABF')
    else if (l_cont_lac) then
!       sdcont_aplist = ds_contact%sdcont_solv(1:14)//'.APLIST'
!       call jeveuo(sdcont_aplist, 'L', vi = v_sdcont_aplist)
    endif
!
! - <LIGREL> for contact elements
!
    ligrcf = ds_contact%ligrel_elem_cont
    call detrsd('LIGREL', ligrcf)
!
! - Acces to mesh
!
    call jeveuo(mesh//'.CONNEX', 'L', vi = v_connex)
    call jeveuo(jexatr(mesh//'.CONNEX', 'LONCUM'), 'L', vi = v_connex_lcum)
!
! - Create list of late elements for contact
!
    call mmlige(mesh      , ds_contact, v_list_elem, nb_cont_type, v_cnt_cont,&
                v_cnt_frot, nt_node   , nb_grel    , nb_cont_elem)
!
! - No late nodes
!
    call wkvect(ligrcf//'.NBNO', 'V V I', 1, vi = v_ligrcf_nbno)
    v_ligrcf_nbno(1) = 0
!
! - Create object NEMA
!
    call jecrec(ligrcf//'.NEMA', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nb_cont_elem)
    call jeecra(ligrcf//'.NEMA', 'LONT', nt_node)
    do i_cont_elem = 1, nb_cont_elem
!
! ----- Get parameters
!
        if (l_cont_cont) then
            i_cont_poin    = i_cont_elem
            elem_slav_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+2))
            elem_mast_nume = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+3))
        else
!           elem_slav_nume = v_sdcont_aplist(3*(i_cont_elem-1)+1)
!           elem_mast_nume = v_sdcont_aplist(3*(i_cont_elem-1)+2)   
        endif
!
! ----- Check number of nodes
!
        nb_node_elem   = v_list_elem(2*(i_cont_elem-1)+2)
        nb_node_slav   = v_connex_lcum(elem_slav_nume+1) - v_connex_lcum(elem_slav_nume)
        nb_node_mast   = v_connex_lcum(elem_mast_nume+1) - v_connex_lcum(elem_mast_nume)
        ASSERT(nb_node_elem .eq. (nb_node_mast+nb_node_slav))
!
! ----- Create contact element in LIGREL
!
        call jecroc(jexnum(ligrcf//'.NEMA', i_cont_elem))
        call jeecra(jexnum(ligrcf//'.NEMA', i_cont_elem), 'LONMAX', nb_node_elem+1)
        call jeveuo(jexnum(ligrcf//'.NEMA', i_cont_elem), 'E', vi = v_ligrcf_nema)
        v_ligrcf_nema(nb_node_elem+1) = v_list_elem(2*(i_cont_elem-1)+1)
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
    do i_cont_type = 1, nb_cont_type
        ligrcf_liel_lont = ligrcf_liel_lont + v_cnt_cont(i_cont_type) + v_cnt_frot(i_cont_type)
    end do
    ASSERT(nb_grel.gt.0)
!
! - Create LIEL object
!
    call jecrec(ligrcf//'.LIEL', 'V V I', 'NU', 'CONTIG', 'VARIABLE',nb_grel)
    call jeecra(ligrcf//'.LIEL', 'LONT', ligrcf_liel_lont)
    ico = 0
    do i_cont_type = 1, nb_cont_type
        cont_indx = i_cont_type
        if (v_cnt_cont(i_cont_type) .ne. 0) then
!
! --------- Create new element
!
            ico = ico + 1
            call jecroc(jexnum(ligrcf//'.LIEL', ico))
            call jeecra(jexnum(ligrcf//'.LIEL', ico), 'LONMAX', v_cnt_cont(i_cont_type)+1)
            call jeveuo(jexnum(ligrcf//'.LIEL', ico), 'E', vi = v_ligrcf_liel)
!
! --------- Current contact element
!
            if (l_cont_cont) then
                call mmelem_data_c(set_cont_indx_  = cont_indx     , l_axi_ = l_axi,&
                                   cont_geom_name_ = cont_geom_name,&
                                   cont_elem_name_ = cont_elem_name)
            else
!               call mmelem_data_l(set_cont_indx_  = cont_indx     ,&
!                                  cont_geom_name_ = cont_geom_name,&
!                                  cont_elem_name_ = cont_elem_name)
            endif
!
! --------- Index of contact element in catalog
!
            call jenonu(jexnom('&CATA.TE.NOMTE', cont_elem_name), cont_elem_nume)
            call jenonu(jexnom('&CATA.TM.NOMTM', cont_geom_name), cont_geom_nume)
!
! --------- Add contact element
!
            v_ligrcf_liel(v_cnt_cont(i_cont_type)+1) = cont_elem_nume
            jco = 0
            do i_cont_elem = 1, nb_cont_elem
                if (v_list_elem(2*(i_cont_elem-1)+1) .eq. cont_geom_nume) then
                    if (l_cont_cont) then
                        i_cont_poin    = i_cont_elem
                        i_zone         = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+14))
                        l_frot         = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE', i_zone )
                    else
!                       i_zone = v_sdcont_aplist(3*(i_cont_elem-1)+3)
!                       l_frot = .false._1 
                    endif
                    if (.not.l_frot) then
                        jco = jco + 1
                        v_ligrcf_liel(jco) = -i_cont_elem
                    endif
                endif
            end do
            ASSERT(jco.eq.v_cnt_cont(i_cont_type))
        endif
        if (v_cnt_frot(i_cont_type) .ne. 0) then
!
! --------- Create new element
!
            ico = ico + 1
            call jecroc(jexnum(ligrcf//'.LIEL', ico))
            call jeecra(jexnum(ligrcf//'.LIEL', ico), 'LONMAX', v_cnt_frot( i_cont_type)+1)
            call jeveuo(jexnum(ligrcf//'.LIEL', ico), 'E', vi = v_ligrcf_liel)
!
! --------- Current friction element
!
            if (l_cont_cont) then
                call mmelem_data_c(set_cont_indx_  = cont_indx     , l_axi_ = l_axi,&
                                   cont_geom_name_ = cont_geom_name,&
                                   frot_elem_name_ = frot_elem_name)
            else
!               call mmelem_data_l(set_cont_indx_  = cont_indx     ,&
!                                  cont_geom_name_ = cont_geom_name,&
!                                  frot_elem_name_ = frot_elem_name)
            endif
!
! --------- Index of friction element in catalog
!
            call jenonu(jexnom('&CATA.TE.NOMTE', frot_elem_name), frot_elem_nume)
            call jenonu(jexnom('&CATA.TM.NOMTM', cont_geom_name), cont_geom_nume)
!
! --------- Add contact element
!
            v_ligrcf_liel(v_cnt_frot(i_cont_type)+1) = frot_elem_nume
            jco = 0
            do i_cont_elem = 1, nb_cont_elem
                if (v_list_elem(2*(i_cont_elem-1)+1) .eq. cont_geom_nume) then
                    if (l_cont_cont) then
                        i_cont_poin    = i_cont_elem
                        i_zone         = nint(v_sdcont_tabfin(ztabf*(i_cont_poin-1)+14))
                        l_frot         = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE', i_zone )
                    else
!                       i_zone = v_sdcont_aplist(3*(i_cont_elem-1)+3)
!                       l_frot = .false._1 
                    endif
                    if (l_frot) then
                        jco = jco + 1
                        v_ligrcf_liel(jco) = -i_cont_elem
                    endif
                endif
            end do
            ASSERT(jco.eq.v_cnt_frot(i_cont_type))
        endif
    end do
    ASSERT(ico.eq.nb_grel)
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
        call jeveuo(sdcont_tabfin, 'L', jtabf)
        call mmimp2(ifm, mesh, ligrcf, jtabf)
    endif
!
! - Clean
!
    AS_DEALLOCATE(vi = v_cnt_cont)
    AS_DEALLOCATE(vi = v_cnt_frot)
    AS_DEALLOCATE(vi = v_list_elem)
!
999 continue
!
end subroutine
