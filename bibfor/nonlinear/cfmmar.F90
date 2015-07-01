subroutine cfmmar(mesh   , sdcont_defi , sdcont_solv , nb_cont_zone, model_ndim,&
                  nt_poin, nb_cont_elem, nb_cont_node, nt_elem_node)
!
implicit none
!
#include "asterfort/apmmvd.h"
#include "asterfort/assert.h"
#include "asterfort/cfcald.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfnben.h"
#include "asterfort/infdbg.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
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
    character(len=24), intent(in) :: sdcont_solv
    integer, intent(in) :: model_ndim
    integer, intent(in) :: nb_cont_zone
    integer, intent(in) :: nt_poin    
    integer, intent(in) :: nb_cont_elem
    integer, intent(in) :: nb_cont_node
    integer, intent(in) :: nt_elem_node
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue/Discrete method - Fill pairing datastructure
!
! --------------------------------------------------------------------------------------------------
!
! /!\ Except point coordinates (see mmpoin/cfpoin)
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
! In  model_ndim       : size of model
! In  nb_cont_zone     : number of contact zones
! In  nt_poin          : total number of points (contact and non-contact)
! In  nb_cont_elem     : total number of contact elements
! In  nb_cont_node     : total number of contact nodes
! In  nt_elem_node     : total number of nodes at all contact elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: sdappa, newgeo
    character(len=24) :: sdappa_nosd
    character(len=24), pointer :: v_sdappa_nosd(:) => null()
    character(len=24) :: sdappa_inzi
    integer, pointer :: v_sdappa_inzi(:) => null()
    character(len=24) :: sdappa_inzr
    real(kind=8), pointer :: v_sdappa_inzr(:) => null()
    character(len=24) :: sdappa_infi
    integer, pointer :: v_sdappa_infi(:) => null()
    character(len=24) :: sdappa_infr
    real(kind=8), pointer :: v_sdappa_infr(:) => null()
    integer :: i_zone
    integer :: i_pair, type_pair, vect_slav_type, vect_mast_type
    integer :: nb_poin, nb_node_mast, nb_node_slav, nb_elem_mast, nb_elem_slav
    integer :: jdecnm, jdecmm, jdecne, jdecme
    character(len=24) :: sdappa_tgel
    integer :: longc, longt, nnosd, ibid, elem_indx, i_cont_elem
    integer :: zinzr, zinzi
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> .. Fill pairing datastructures for DISCRETE/CONTINUE methods'
    endif
!
! - Pairing datastructure
!
    sdappa = sdcont_solv(1:14)//'.APPA'
!
! - Displacement field for geometry update
!
    newgeo = sdcont_solv(1:14)//'.NEWG'
!
! - Create datastructure to save names of datastructures
!
    sdappa_nosd = sdappa(1:19)//'.NOSD'
    call jeveuo(sdappa_nosd, 'E', vk24 = v_sdappa_nosd)
    v_sdappa_nosd(1) = mesh
    v_sdappa_nosd(2) = newgeo
    v_sdappa_nosd(3) = sdcont_defi
!
! - Create datastructure for general parameters
!
    sdappa_infi = sdappa(1:19)//'.INFI'
    sdappa_infr = sdappa(1:19)//'.INFR'
    call jeveuo(sdappa_infi, 'E', vi = v_sdappa_infi)
    call jeveuo(sdappa_infr, 'E', vr = v_sdappa_infr)
    zinzr = apmmvd('ZINZR')
    zinzi = apmmvd('ZINZI')
    v_sdappa_infi(1) = nb_cont_zone
    v_sdappa_infi(2) = nt_poin
    v_sdappa_infi(3) = nb_cont_elem
    v_sdappa_infi(4) = cfdisi(sdcont_defi,'PROJ_NEWT_ITER')
    v_sdappa_infi(5) = model_ndim
    v_sdappa_infi(6) = nb_cont_node
    v_sdappa_infr(1) = cfdisr(sdcont_defi,'PROJ_NEWT_RESI')
!
! - Create datastructure for each contact zone
!
    sdappa_inzi = sdappa(1:19)//'.INZI'
    sdappa_inzr = sdappa(1:19)//'.INZR'
    call jeveuo(sdappa_inzi, 'E', vi = v_sdappa_inzi)
    call jeveuo(sdappa_inzr, 'E', vr = v_sdappa_inzr)
    do i_zone = 1, nb_cont_zone
!
! ----- Set parameters
!
        nb_poin      = mminfi(sdcont_defi, 'NBPT' , i_zone)
        nb_node_mast = mminfi(sdcont_defi, 'NBNOM', i_zone)
        nb_node_slav = mminfi(sdcont_defi, 'NBNOE', i_zone)
        nb_elem_mast = mminfi(sdcont_defi, 'NBMAM', i_zone)
        nb_elem_slav = mminfi(sdcont_defi, 'NBMAE', i_zone)
        jdecnm       = mminfi(sdcont_defi, 'JDECNM', i_zone)
        jdecmm       = mminfi(sdcont_defi, 'JDECMM', i_zone)
        jdecne       = mminfi(sdcont_defi, 'JDECNE', i_zone)
        jdecme       = mminfi(sdcont_defi, 'JDECME', i_zone)
        v_sdappa_inzi(zinzi*(i_zone-1)+1) = nb_poin
        v_sdappa_inzi(zinzi*(i_zone-1)+2) = nb_node_mast
        v_sdappa_inzi(zinzi*(i_zone-1)+3) = nb_node_slav
        v_sdappa_inzi(zinzi*(i_zone-1)+4) = nb_elem_mast
        v_sdappa_inzi(zinzi*(i_zone-1)+5) = nb_elem_slav
        v_sdappa_inzi(zinzi*(i_zone-1)+6) = jdecnm
        v_sdappa_inzi(zinzi*(i_zone-1)+7) = jdecmm
        v_sdappa_inzi(zinzi*(i_zone-1)+8) = jdecne
        v_sdappa_inzi(zinzi*(i_zone-1)+9) = jdecme
!
! ----- Pairing options
!
        type_pair = mminfi(sdcont_defi, 'TYPE_APPA'  , i_zone)
        i_pair    = mminfi(sdcont_defi, 'APPARIEMENT', i_zone)
        v_sdappa_inzi(zinzi*(i_zone-1)+10) = type_pair
        v_sdappa_inzi(zinzi*(i_zone-1)+11) = i_pair
        v_sdappa_inzr(zinzr*(i_zone-1)+4)  = mminfr(sdcont_defi, 'TOLE_APPA'    , i_zone)
        v_sdappa_inzr(zinzr*(i_zone-1)+5)  = mminfr(sdcont_defi, 'TOLE_PROJ_EXT', i_zone)
        if (type_pair .eq. 1) then
            v_sdappa_inzr(zinzr*(i_zone-1)+1) = mminfr(sdcont_defi, 'TYPE_APPA_DIRX', i_zone)
            v_sdappa_inzr(zinzr*(i_zone-1)+2) = mminfr(sdcont_defi, 'TYPE_APPA_DIRY', i_zone)
            v_sdappa_inzr(zinzr*(i_zone-1)+3) = mminfr(sdcont_defi, 'TYPE_APPA_DIRZ', i_zone)
        endif
!
! ----- Local basis for master side
!
        vect_mast_type = mminfi(sdcont_defi, 'VECT_MAIT', i_zone)
        v_sdappa_inzi(zinzi*(i_zone-1)+12) = vect_mast_type
        if (vect_mast_type .ne. 0) then
            v_sdappa_inzr(zinzr*(i_zone-1)+6) = mminfr(sdcont_defi, 'VECT_MAIT_DIRX', i_zone)
            v_sdappa_inzr(zinzr*(i_zone-1)+7) = mminfr(sdcont_defi, 'VECT_MAIT_DIRY', i_zone)
            v_sdappa_inzr(zinzr*(i_zone-1)+8) = mminfr(sdcont_defi, 'VECT_MAIT_DIRZ', i_zone)
        endif
!
! ----- Local basis for slave side
!
        vect_slav_type = mminfi(sdcont_defi, 'VECT_ESCL', i_zone)
        v_sdappa_inzi(zinzi*(i_zone-1)+13) = vect_slav_type
        if (vect_slav_type .ne. 0) then
            v_sdappa_inzr(zinzr*(i_zone-1)+9 ) = mminfr(sdcont_defi, 'VECT_ESCL_DIRX', i_zone)
            v_sdappa_inzr(zinzr*(i_zone-1)+10) = mminfr(sdcont_defi, 'VECT_ESCL_DIRY', i_zone)
            v_sdappa_inzr(zinzr*(i_zone-1)+11) = mminfr(sdcont_defi, 'VECT_ESCL_DIRZ', i_zone)
        endif
!
! ----- Compute which side ?
!
        if (cfcald(sdcont_defi,i_zone,'ESCL')) then
            v_sdappa_inzi(zinzi*(i_zone-1)+14) = 1
        endif
        if (cfcald(sdcont_defi,i_zone,'MAIT')) then
            v_sdappa_inzi(zinzi*(i_zone-1)+15) = 1
        endif
    end do
!
! - Tangents at nodes for each element
!
    sdappa_tgel = sdappa(1:19)//'.TGEL'
    longt       = 0
    do i_cont_elem = 1, nb_cont_elem
        elem_indx = i_cont_elem
        call cfnben(sdcont_defi, elem_indx, 'CONNEX', nnosd, ibid)
        longc = 6*nnosd
        call jeecra(jexnum(sdappa_tgel, i_cont_elem), 'LONMAX', ival=longc)
        call jecroc(jexnum(sdappa_tgel, i_cont_elem))
        longt = longt + longc
    end do
    ASSERT(longt.eq.6*nt_elem_node)
!
end subroutine
