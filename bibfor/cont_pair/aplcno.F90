subroutine aplcno(mesh, newgeo, sdcont_defi, sdappa)
!
implicit none
!
#include "asterfort/jeveuo.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecroc.h"
#include "asterfort/jecrec.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/cfdisi.h"
#include "asterfort/mminfi.h"
#include "asterfort/aptgnn.h"
#include "asterfort/aptgem.h"
#include "asterfort/aptnol.h"
#include "asterfort/apsvnl.h"
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
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: newgeo
    character(len=24), intent(in) :: sdcont_defi
    character(len=19), intent(in) :: sdappa
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Compute norms at nodes (smoothing)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdappa           : name of pairing datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: model_ndim
    integer :: nt_node_slav, nt_node_mast, nt_node, nt_elem_slav, nt_elem_mast, nt_elem
    integer :: nb_cont_zone, nb_elem_slav, nb_elem_mast, nb_node_slav, nb_node_mast
    integer :: iter_maxi
    integer :: i_zone, i_elem
    integer :: jdecnm, jdecmm, jdecne, jdecme
    character(len=24) :: sdappa_tgno, sdappa_tgel, sdappa_norl
    real(kind=8), pointer :: v_sdappa_tgno(:) => null()
    real(kind=8), pointer :: v_sdappa_norl(:) => null()
    character(len=4) :: zone_type 
    real(kind=8) :: norm_vect(3), epsi_maxi
    integer :: norm_type
!
! --------------------------------------------------------------------------------------------------
! 
    epsi_maxi = 1.d-12
    iter_maxi = 100
!
! - Get parameters
!
    nb_cont_zone = cfdisi(sdcont_defi, 'NZOCO')
    model_ndim   = cfdisi(sdcont_defi, 'NDIM' )
    nt_node_slav = cfdisi(sdcont_defi, 'NTNOE')
    nt_node_mast = cfdisi(sdcont_defi, 'NTNOM')
    nt_node      = nt_node_slav+nt_node_mast
    nt_elem_slav = cfdisi(sdcont_defi, 'NTMAE')
    nt_elem_mast = cfdisi(sdcont_defi, 'NTMAM')  
    nt_elem      = nt_elem_slav+nt_elem_mast
!
! - Access to pairing datastructures
!
    sdappa_tgno = sdappa(1:19)//'.TGNO'
    sdappa_tgel = sdappa(1:19)//'.TGEL'
    sdappa_norl = sdappa(1:19)//'.NORL'
    call jedetr(sdappa_tgno)
    call jedetr(sdappa_tgel)
    call jedetr(sdappa_norl)       
    call wkvect(sdappa_tgno, 'V V R', 6*nt_node, vr = v_sdappa_tgno)
    call wkvect(sdappa_norl, 'V V R', 3*nt_node, vr = v_sdappa_norl)
    call jecrec(sdappa_tgel, 'V V R', 'NU', 'CONTIG', 'CONSTANT',&
                nt_elem)
    call jeecra(sdappa_tgel, 'LONT', nt_elem*6*9)
    do i_elem = 1, nt_elem
        call jecroc(jexnum(sdappa_tgel, i_elem))
        call jeecra(jexnum(sdappa_tgel, i_elem), 'LONUTI', ival=6*9)
    end do
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Get parameters on current zone
!
        nb_node_mast = mminfi(sdcont_defi, 'NBNOM' , i_zone)
        nb_node_slav = mminfi(sdcont_defi, 'NBNOE' , i_zone)
        nb_elem_mast = mminfi(sdcont_defi, 'NBMAM' , i_zone)
        nb_elem_slav = mminfi(sdcont_defi, 'NBMAE' , i_zone)
        jdecnm       = mminfi(sdcont_defi, 'JDECNM', i_zone)
        jdecmm       = mminfi(sdcont_defi, 'JDECMM', i_zone)
        jdecne       = mminfi(sdcont_defi, 'JDECNE', i_zone)
        jdecme       = mminfi(sdcont_defi, 'JDECME', i_zone)
        norm_type    = 0
! 
! ----- Compute tangents at each node for each master element
!
        zone_type = 'MAIT'
        call aptgem(sdappa      , mesh     , newgeo   , sdcont_defi, model_ndim,&
                    i_zone      , zone_type, iter_maxi, epsi_maxi  , jdecmm    ,&
                    nb_elem_mast)
! 
! ----- Compute tangents at each node for each slave element
!
        zone_type = 'ESCL'
        call aptgem(sdappa      , mesh     , newgeo   , sdcont_defi, model_ndim,&
                    i_zone      , zone_type, iter_maxi, epsi_maxi  , jdecme    ,&
                    nb_elem_slav)
!
! ----- Compute tangents at each node by smoothing
!
        call aptgnn(sdappa      , mesh     , sdcont_defi, model_ndim, jdecnm,&
                    nb_node_mast, norm_type, norm_vect  )
        call aptgnn(sdappa      , mesh     , sdcont_defi, model_ndim, jdecne,&
                    nb_node_slav, norm_type, norm_vect  )
!
! ----- Compute normals at nodes
!   
        call aptnol(sdappa, model_ndim, nt_node)
!
    end do
!
! - Smooth normals at nodes
!
    call apsvnl(sdcont_defi, sdappa, model_ndim, nt_node)
!
end subroutine
