subroutine cfpoin(mesh, ds_contact, newgeo, sdappa)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/mminfi.h"
#include "asterfort/assert.h"
#include "asterfort/cfcorn.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmex.h"
#include "asterfort/cfnumn.h"
#include "asterfort/infdbg.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/jerazo.h"
#include "asterfort/jelira.h"
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
    character(len=19), intent(in) :: newgeo
    character(len=19), intent(in) :: sdappa
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Fill pairing datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv, iret
    character(len=24) :: sdappa_poin, sdappa_infp, sdappa_noms
    character(len=24) :: sdappa_tau1, sdappa_tau2, sdappa_proj
    character(len=24) :: sdappa_dist, sdappa_appa, sdappa_tgno, sdappa_tgel    
    character(len=24) :: sdappa_mpia, sdappa_mpib, sdappa_mpic
    real(kind=8), pointer :: v_sdappa_poin(:) => null()
    integer, pointer :: v_sdappa_infp(:) => null()
    character(len=16), pointer :: v_sdappa_noms(:) => null()
    character(len=16), pointer :: valk(:) => null()
    integer :: i_node_escl
    integer :: i_poin, i_node_slav, i_zone
    integer :: nb_poin, nb_node_slav
    integer :: node_slav_indx(1), node_slav_nume(1)
    integer :: jdecne
    real(kind=8) :: poin_coor(3)
    character(len=8) :: node_slav_name
    character(len=16) :: poin_name
    integer :: nb_cont_zone, length
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... PREPARATION DE L''APPARIEMENT'
    endif
!
! - Access to pairing datastructure
!
    sdappa_poin = sdappa(1:19)//'.POIN'
    sdappa_infp = sdappa(1:19)//'.INFP'
    sdappa_noms = sdappa(1:19)//'.NOMS'
    call jeveuo(sdappa_poin, 'E', vr   = v_sdappa_poin)
    call jeveuo(sdappa_infp, 'E', vi   = v_sdappa_infp)
    call jeveuo(sdappa_noms, 'E', vk16 = v_sdappa_noms)
!
! - Get parameters
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO')
!
! - Loop on contact zones
!
    i_poin = 1
    do i_zone = 1, nb_cont_zone
!
! ----- Get parameters on current zone
!
        nb_poin      = mminfi(ds_contact%sdcont_defi, 'NBPT'  , i_zone)
        nb_node_slav = mminfi(ds_contact%sdcont_defi, 'NBNOE' , i_zone)
        jdecne       = mminfi(ds_contact%sdcont_defi, 'JDECNE', i_zone)
        ASSERT(nb_poin .eq. nb_node_slav)
!
! ----- Loop on contact nodes
!
        do i_node_slav = 1, nb_node_slav
!
! --------- Current contact point
!
            node_slav_indx(1) = jdecne + i_node_slav
            call cfnumn(ds_contact%sdcont_defi, 1, node_slav_indx(1), node_slav_nume(1))
!
! --------- Coordinates of contact point
!
            call cfcorn(newgeo, node_slav_nume(1), poin_coor)
            v_sdappa_poin(3*(i_poin-1)+1) = poin_coor(1)
            v_sdappa_poin(3*(i_poin-1)+2) = poin_coor(2)
            v_sdappa_poin(3*(i_poin-1)+3) = poin_coor(3)
!
! --------- Node is excluded ?
!
            call cfmmex(ds_contact%sdcont_defi, 'CONT', i_zone, node_slav_nume(1), i_node_escl)
            v_sdappa_infp(i_poin) = i_node_escl
!
! --------- Name of point
!
            call jenuno(jexnum(mesh//'.NOMNOE', node_slav_nume(1)), node_slav_name)
            poin_name = 'NOEUD   '//node_slav_name
            v_sdappa_noms(i_poin) = poin_name
!
! --------- Next point
!
            i_poin = i_poin + 1
        end do
    end do
!
! ------------- Pairing mpi data sutructure initialisation
!
    sdappa_appa = sdappa(1:19)//'.APPA'
    sdappa_dist = sdappa(1:19)//'.DIST'
    sdappa_tau1 = sdappa(1:19)//'.TAU1'
    sdappa_tau2 = sdappa(1:19)//'.TAU2'
    sdappa_proj = sdappa(1:19)//'.PROJ'
    sdappa_tgel = sdappa(1:19)//'.TGEL'
    sdappa_tgno = sdappa(1:19)//'.TGNO'
    call jerazo(sdappa_appa,4*(i_poin-1),1)
    call jerazo(sdappa_dist,4*(i_poin-1),1)
    call jerazo(sdappa_tau1,3*(i_poin-1),1)
    call jerazo(sdappa_tau2,3*(i_poin-1),1)
    call jerazo(sdappa_proj,2*(i_poin-1),1)
    call jerazo(sdappa_tgno,6*(i_poin-1),1)
    call jelira(sdappa_tgel, 'LONT', length)
    call jerazo(sdappa_tgel, length ,1)

    sdappa_mpia = sdappa(1:19)//'.MPIA'
    sdappa_mpib = sdappa(1:19)//'.MPIB'
    sdappa_mpic = sdappa(1:19)//'.MPIC'
    call jeexin(sdappa_mpia,iret)
    if (iret .eq. 0) then
        call wkvect(sdappa_mpia,'V V K16',1,vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call wkvect(sdappa_mpib,'V V K16',1,vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call wkvect(sdappa_mpic,'V V K16',1,vk16=valk)
        valk(1)='MPI_INCOMPLET'
    else 
        call jeveuo(sdappa_mpia, 'E',vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call jeveuo(sdappa_mpib, 'E',vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call jeveuo(sdappa_mpic, 'E',vk16=valk)
        valk(1)='MPI_INCOMPLET'
    endif   
! 
!
end subroutine
