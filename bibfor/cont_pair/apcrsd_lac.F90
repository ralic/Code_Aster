subroutine apcrsd_lac(ds_contact  , sdappa      , mesh        ,&
                      nt_poin     , nb_cont_elem, nb_cont_node,&
                      nt_elem_node, nb_node_mesh)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/apcinv.h"
#include "asterfort/gtlima.h"
#include "asterfort/infdbg.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/crcnct.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnben.h"
#include "asterfort/jecroc.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nt_poin
    integer, intent(in) :: nb_cont_elem
    integer, intent(in) :: nb_cont_node
    integer, intent(in) :: nt_elem_node
    integer, intent(in) :: nb_node_mesh
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Create datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  sdappa           : name of pairing datastructure
! In  nt_poin          : total number of points (contact and non-contact)
! In  nb_cont_elem     : total number of contact elements
! In  nb_cont_node     : total number of contact nodes
! In  nt_elem_node     : total number of nodes at all contact elements
! In  nb_node_mesh     : number of nodes in mesh
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nt_patch, i_zone, nb_cont_zone
    character(len=24) :: pair_method
    integer :: i_cont_elem, longt, elem_indx, longc, elem_nbnode
    character(len=24) :: sdappa_poin
    real(kind=8), pointer :: v_sdappa_poin(:) => null()
    character(len=24) :: sdappa_infp
    integer, pointer :: v_sdappa_infp(:) => null()
    character(len=24) :: sdappa_noms
    character(len=16), pointer :: v_sdappa_noms(:) => null()
    character(len=24) :: sdappa_appa
    integer, pointer :: v_sdappa_appa(:) => null()
    character(len=24) :: sdappa_dist
    real(kind=8), pointer :: v_sdappa_dist(:) => null()
    character(len=24) :: sdappa_tau1
    real(kind=8), pointer :: v_sdappa_tau1(:) => null()
    character(len=24) :: sdappa_tau2
    real(kind=8), pointer :: v_sdappa_tau2(:) => null()
    character(len=24) :: sdappa_proj
    real(kind=8), pointer :: v_sdappa_proj(:) => null()
    character(len=24) :: sdappa_tgno, sdappa_tgel
    real(kind=8), pointer :: v_sdappa_tgno(:) => null()
    character(len=24) :: sdappa_verk
    character(len=8), pointer :: v_sdappa_verk(:) => null()
    character(len=24) :: sdappa_vera
    real(kind=8), pointer :: v_sdappa_vera(:) => null()
    character(len=24) :: sdappa_gapi
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    character(len=24) :: sdappa_coef
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    character(len=24) :: sdappa_poid
    real(kind=8), pointer :: v_sdappa_poid(:) => null()
    character(len=24) :: sdappa_psno
    character(len=24) :: sdappa_norl
    real(kind=8), pointer :: v_sdappa_norl(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<PAIRING> Create datastructure'
    endif
!
! - Get parameters
!
    nt_patch     = ds_contact%nt_patch
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi, 'NZOCO')
    pair_method  = 'PANG_ROBUSTE'
!
! - Datastructure for pairing results
!
    sdappa_appa = sdappa(1:19)//'.APPA'
    call wkvect(sdappa_appa, 'V V I', 4*nt_poin, vi = v_sdappa_appa)
!
! - Datastructure for distances and local basis
!
    sdappa_dist = sdappa(1:19)//'.DIST'
    sdappa_tau1 = sdappa(1:19)//'.TAU1'
    sdappa_tau2 = sdappa(1:19)//'.TAU2'
    call wkvect(sdappa_dist, 'V V R', 4*nt_poin, vr = v_sdappa_dist)
    call wkvect(sdappa_tau1, 'V V R', 3*nt_poin, vr = v_sdappa_tau1)
    call wkvect(sdappa_tau2, 'V V R', 3*nt_poin, vr = v_sdappa_tau2)
!
! - Datastructure for projection points
!
    sdappa_proj = sdappa(1:19)//'.PROJ'
    call wkvect(sdappa_proj, 'V V R', 2*nt_poin, vr = v_sdappa_proj)
!
! - Datastructure for coordinates of points
!
    sdappa_poin = sdappa(1:19)//'.POIN'
    call wkvect(sdappa_poin, 'V V R', 3*nt_poin, vr = v_sdappa_poin)
!
! - Datastructure for informations about points
!
    sdappa_infp = sdappa(1:19)//'.INFP'
    call wkvect(sdappa_infp, 'V V I', nt_poin, vi = v_sdappa_infp)
!
! - Datastructure for name of contact points
!
    sdappa_noms = sdappa(1:19)//'.NOMS'
    call wkvect(sdappa_noms, 'V V K16', nt_poin, vk16 = v_sdappa_noms)
!
! - Datastructure for tangents at each node
!
    sdappa_tgno = sdappa(1:19)//'.TGNO'
    call wkvect(sdappa_tgno, 'V V R', 6*nb_cont_node, vr = v_sdappa_tgno)
!
! - Datastructure for tangents at each node by element
!
    sdappa_tgel = sdappa(1:19)//'.TGEL'
    call jecrec(sdappa_tgel, 'V V R', 'NU', 'CONTIG', 'VARIABLE',&
                nb_cont_elem)
    call jeecra(sdappa_tgel, 'LONT', 6*nt_elem_node)
    longt = 0
    do i_cont_elem = 1, nb_cont_elem
        elem_indx = i_cont_elem
        call cfnben(ds_contact%sdcont_defi, elem_indx, 'CONNEX', elem_nbnode)
        longc = 6*elem_nbnode
        call jeecra(jexnum(sdappa_tgel, i_cont_elem), 'LONMAX', ival=longc)
        call jecroc(jexnum(sdappa_tgel, i_cont_elem))
        longt = longt + longc
    end do
    ASSERT(longt.eq.6*nt_elem_node)
!
! - Datastructure for check normals discontinuity
!
    sdappa_verk = sdappa(1:19)//'.VERK'
    sdappa_vera = sdappa(1:19)//'.VERA'
    call wkvect(sdappa_verk, 'V V K8', nb_node_mesh, vk8 = v_sdappa_verk)
    call wkvect(sdappa_vera, 'V V R' , nb_node_mesh, vr  = v_sdappa_vera)
    call jeecra(sdappa_verk, 'LONUTI', 0)
!
! - Datastructure for gap
!
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    call wkvect(sdappa_gapi, 'V V R', nt_patch, vr = v_sdappa_gapi)
!
! - Datastructures for intersection parameters
!
    sdappa_coef = sdappa(1:19)//'.COEF'
    sdappa_poid = sdappa(1:19)//'.POID'
    call wkvect(sdappa_poid, 'V V R', nt_patch, vr = v_sdappa_coef)
    call wkvect(sdappa_coef, 'V V R', nt_patch, vr = v_sdappa_poid)
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Create list of elements for current contact zone
!
        call gtlima(sdappa, ds_contact%sdcont_defi, i_zone)
!
! ----- Create objects for inverse connectivity
!
        if (pair_method(1:4).eq.'PANG') then
            call apcinv(mesh, sdappa, i_zone)
        endif
    end do
!
! - Datastructure for Smoothed NOrmals: CHAM_NO on complete mesh
!
    sdappa_psno = sdappa(1:14)//'.PSNO'
    call crcnct('V', sdappa_psno, mesh, 'GEOM_R', 3,&
                ['X','Y','Z'], [0.d0,0.d0,0.d0])
!
! - Datastructure for normals: only on contact zone (not a CHAM_NO ! )
!
    sdappa_norl = sdappa(1:19)//'.NORL'
    call wkvect(sdappa_norl, 'V V R', 3*nb_cont_node, vr = v_sdappa_norl)
!
    call jedema()
!
end subroutine
