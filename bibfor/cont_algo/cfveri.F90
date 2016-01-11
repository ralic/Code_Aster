subroutine cfveri(mesh        , ds_contact  , newgeo      , sdappa      , nt_ncomp_poin,&
                  v_ncomp_jeux, v_ncomp_loca, v_ncomp_enti, v_ncomp_zone, time_curr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/apcopt.h"
#include "asterfort/apinfi.h"
#include "asterfort/apinfr.h"
#include "asterfort/apvect.h"
#include "asterfort/assert.h"
#include "asterfort/cfcoor.h"
#include "asterfort/cfcorn.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdist.h"
#include "asterfort/cfnewj.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfnumn.h"
#include "asterfort/cftanr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmnpoi.h"
#include "asterfort/utmess.h"
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
    character(len=19), intent(in) :: newgeo
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: nt_ncomp_poin
    real(kind=8), pointer, intent(in) :: v_ncomp_jeux(:)
    integer, pointer, intent(in) :: v_ncomp_loca(:)
    character(len=16), pointer, intent(in) :: v_ncomp_enti(:)
    integer, pointer, intent(in) :: v_ncomp_zone(:)
    real(kind=8), intent(in) :: time_curr
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Post-treatment for no computation methods
!
! Method discrete - Evaluate
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  sdappa           : name of pairing datastructure
! In  nt_ncomp_poin    : number of points in no-computation mode
! In  v_ncomp_jeux     : pointer to save gaps
! In  v_ncomp_loca     : pointer to save index of node
! In  v_ncomp_enti     : pointer to save name of entities
! In  v_ncomp_zone     : pointer to save contact zone index
! In  time_curr        : current time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: pair_type, pair_enti
    integer :: jdecne
    integer :: posmae, elem_mast_nume, node_slav_indx(1), elem_mast_indx, node_slav_nume(1)
    integer :: i_zone, i_poin, i_cont_node, i_ncomp_poin
    integer :: model_ndim, nb_cont_zone
    integer :: nb_poin, nb_cont_node, nt_ncomp_poin0
    real(kind=8) :: node_coor_proj(3), poin_coor(3)
    real(kind=8) :: tau1m(3), tau2m(3)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: ksipr1, ksipr2
    real(kind=8) :: r8bid
    real(kind=8) :: gap, gap_user
    character(len=8) :: node_slav_name, elem_mast_name, k8bla
    character(len=16) :: poin_name, enti_name
    aster_logical :: l_veri
!
! --------------------------------------------------------------------------------------------------
!
    node_slav_indx = 0
    i_ncomp_poin   = 1
    k8bla          = ' '
!
! - Parameters
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO')
    model_ndim   = cfdisi(ds_contact%sdcont_defi,'NDIM')
!
! - Loop on contact zones
!
    i_poin         = 1
    nt_ncomp_poin0 = 0
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters of zone
!
        nb_cont_node = mminfi(ds_contact%sdcont_defi, 'NBPT'  , i_zone)
        jdecne       = mminfi(ds_contact%sdcont_defi, 'JDECNE', i_zone)
        l_veri       = mminfl(ds_contact%sdcont_defi, 'VERIF' , i_zone)
!
! ----- Computation: no evaluate (see cfresu)
!
        if (.not.l_veri) then
            nb_poin = mminfi(ds_contact%sdcont_defi, 'NBPC', i_zone)
            i_poin  = i_poin + nb_poin
            goto 25
        endif
!
! ----- Loop on nodes
!
        do i_cont_node = 1, nb_cont_node
!
! --------- Current slave node
!
            node_slav_indx(1) = jdecne + i_cont_node
            call cfnumn(ds_contact%sdcont_defi, 1, node_slav_indx(1), node_slav_nume(1))
            call jenuno(jexnum(mesh//'.NOMNOE', node_slav_nume(1)), node_slav_name)
!
! --------- Parameters from pairing
!
            call apinfi(sdappa, 'APPARI_TYPE'     , i_poin, pair_type)
            call apinfi(sdappa, 'APPARI_ENTITE'   , i_poin, pair_enti)
            call apinfr(sdappa, 'APPARI_PROJ_KSI1', i_poin, ksipr1)
            call apinfr(sdappa, 'APPARI_PROJ_KSI2', i_poin, ksipr2)
            call apvect(sdappa, 'APPARI_TAU1'     , i_poin, tau1m)
            call apvect(sdappa, 'APPARI_TAU2'     , i_poin, tau2m)
!
! --------- Coordinates of point
!
            call apcopt(sdappa, i_poin, poin_coor)
!
! --------- Name of point
!
            call mmnpoi(mesh, k8bla, node_slav_nume(1), i_cont_node, poin_name)
!
! --------- Depending on pairing type
!
            if (pair_type .eq. 2) then
!
! ------------- Master element
!
                elem_mast_indx = pair_enti
                call cfnumm(ds_contact%sdcont_defi, elem_mast_indx, elem_mast_nume)
                call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
                enti_name = elem_mast_name
!
! ------------- Coordinates of projection
!
                call cfcoor(mesh  , ds_contact%sdcont_defi   , newgeo, elem_mast_indx, ksipr1,&
                            ksipr2, node_coor_proj)
!
! ------------- Define new local basis
!
                call cftanr(mesh, model_ndim, ds_contact, i_zone,&
                            node_slav_indx(1), 'MAIL', elem_mast_indx, elem_mast_nume, ksipr1,&
                            ksipr2, tau1m, tau2m, tau1, tau2)
                call mmnorm(model_ndim, tau1, tau2, norm, noor)
                if (noor .le. r8prem()) then
                    call utmess('F', 'CONTACT3_26', sk=node_slav_name)
                endif
!
! ------------- Compute gap
!
                call cfnewj(model_ndim, poin_coor, node_coor_proj, norm, gap)
                call cfdist(ds_contact, i_zone, posmae, poin_coor, time_curr,&
                            gap_user, node_slav_indx_ = node_slav_indx(1))
                gap = gap+gap_user
            else if (pair_type .eq. 1) then
!
! ------------- Master node
!
                node_slav_indx(1) = pair_enti
                call cfnumn(ds_contact%sdcont_defi, 1, node_slav_indx(1), node_slav_nume(1))
                call jenuno(jexnum(mesh//'.NOMNOE', node_slav_nume(1)), node_slav_name)
                enti_name = node_slav_name
!
! ------------- Coordinate of master node
!
                call cfcorn(newgeo, node_slav_nume(1), node_coor_proj)
!
! ------------- Define new local basis
!
                call cftanr(mesh, model_ndim, ds_contact, i_zone,&
                            node_slav_indx(1), 'NOEU', node_slav_indx(1), node_slav_nume(1), r8bid,&
                            r8bid, tau1m, tau2m, tau1, tau2)
                call mmnorm(model_ndim, tau1, tau2, norm, noor)
                if (noor .le. r8prem()) then
                    call utmess('F', 'CONTACT3_26', sk=node_slav_name)
                endif
!
! ------------- Compute gap
!
                call cfnewj(model_ndim, poin_coor, node_coor_proj, norm, gap)
                call cfdist(ds_contact, i_zone, posmae, poin_coor, time_curr,&
                            gap_user, node_slav_indx_ = node_slav_indx(1))
                gap = gap+gap_user
!
            else if (pair_type.eq.-1) then
                enti_name = 'EXCLU'
                gap = r8vide()
            else if (pair_type.eq.-2) then
                enti_name = 'EXCLU'
                gap = r8vide()
            else if (pair_type.eq.-3) then
                enti_name = 'EXCLU'
                gap = r8vide()
            else
                ASSERT(.false.)
            endif
!
! --------- Save
!
            v_ncomp_jeux(i_ncomp_poin) = gap
            v_ncomp_loca(i_ncomp_poin) = node_slav_nume(1)
            v_ncomp_zone(i_ncomp_poin) = i_zone
            v_ncomp_enti(2*(i_ncomp_poin-1)+1) = poin_name
            v_ncomp_enti(2*(i_ncomp_poin-1)+2) = enti_name
!
! --------- Next points
!
            i_ncomp_poin   = i_ncomp_poin + 1
            nt_ncomp_poin0 = nt_ncomp_poin0+ 1
            i_poin         = i_poin + 1
!
        end do
 25     continue
    end do
!
    ASSERT(nt_ncomp_poin0.eq.nt_ncomp_poin)
!
end subroutine
