subroutine mmveri(mesh        , ds_contact  , time_curr   , nt_ncomp_poin,&
                  v_ncomp_jeux, v_ncomp_loca, v_ncomp_enti, v_ncomp_zone )
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
#include "asterfort/cfdisi.h"
#include "asterfort/cfdist.h"
#include "asterfort/cfnumm.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mcopco.h"
#include "asterfort/mmelty.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mmnewj.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmnpoi.h"
#include "asterfort/mmnumn.h"
#include "asterfort/mmpnoe.h"
#include "asterfort/mmtanr.h"
#include "asterfort/utmess.h"
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
    real(kind=8), intent(in) :: time_curr
    integer, intent(in) :: nt_ncomp_poin
    real(kind=8), pointer, intent(in) :: v_ncomp_jeux(:)
    integer, pointer, intent(in) :: v_ncomp_loca(:)
    character(len=16), pointer, intent(in) :: v_ncomp_enti(:)
    integer, pointer, intent(in) :: v_ncomp_zone(:)

!
! --------------------------------------------------------------------------------------------------
!
! Contact - Post-treatment for no computation methods
!
! Method continue - Evaluate
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  time_curr        : current time
! In  nt_ncomp_poin    : number of points in no-computation mode
! In  v_ncomp_jeux     : pointer to save gaps
! In  v_ncomp_loca     : pointer to save index of node
! In  v_ncomp_enti     : pointer to save name of entities
! In  v_ncomp_zone     : pointer to save contact zone index
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: newgeo, sdappa
    integer :: type_inte, ndexfr
    integer :: pair_type, pair_enti
    integer :: jdecme
    integer :: elem_slav_indx, elem_slav_nume, elem_mast_nume, node_slav_indx
    integer :: elem_mast_indx, node_slav_nume
    integer :: i_zone, i_elem_slav, i_poin, i_poin_elem, i_ncomp_poin
    integer :: model_ndim, nb_cont_zone
    integer :: nb_poin_elem, nb_elem_slav, nb_poin, elem_slav_nbno, nt_ncomp_poin0
    real(kind=8) :: node_coor_proj(3), poin_coor(3)
    real(kind=8) :: tau1m(3), tau2m(3)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: ksipr1, ksipr2
    real(kind=8) :: gap, gap_user
    character(len=8) :: elem_slav_name, elem_mast_name, elem_slav_type
    character(len=16) :: poin_name, enti_name
    aster_logical :: l_veri, l_excl_frot
!
! --------------------------------------------------------------------------------------------------
!
    node_slav_indx = 0
    i_ncomp_poin   = 1
!
! - Pairing datastructure
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! - New geometry name
!
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
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
        type_inte    = mminfi(ds_contact%sdcont_defi, 'INTEGRATION', i_zone)
        l_veri       = mminfl(ds_contact%sdcont_defi, 'VERIF'      , i_zone)
        nb_elem_slav = mminfi(ds_contact%sdcont_defi, 'NBMAE'      , i_zone)
        jdecme       = mminfi(ds_contact%sdcont_defi, 'JDECME'     , i_zone)
!
! ----- Computation: no evaluate (see mmmres)
!
        if (.not.l_veri) then
            nb_poin = mminfi(ds_contact%sdcont_defi, 'NBPC' , i_zone)
            i_poin = i_poin + nb_poin
            goto 25
        endif
!
! ----- Loop on slave elements
!
        do i_elem_slav = 1, nb_elem_slav
!
! --------- Slave element index in contact datastructure
!
            elem_slav_indx = jdecme + i_elem_slav
!
! --------- Slave element index in mesh
!
            call cfnumm(ds_contact%sdcont_defi, elem_slav_indx, elem_slav_nume)
!
! --------- Number of integration points on element
!
            call mminfm(elem_slav_indx, ds_contact%sdcont_defi, 'NPTM', nb_poin_elem)
!
! --------- Parameters of slave element
!
            call mmelty(mesh, elem_slav_nume, elem_slav_type, elem_slav_nbno)
            call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_nume), elem_slav_name)
            call mminfm(elem_slav_indx, ds_contact%sdcont_defi, 'NDEXFR', ndexfr)
            l_excl_frot = (ndexfr.ne.0)
!
! --------- Loop on integration points
!
            do i_poin_elem = 1, nb_poin_elem
!
! ------------- Parameters from pairing
!
                call apinfi(sdappa, 'APPARI_TYPE'     , i_poin, pair_type)
                call apinfi(sdappa, 'APPARI_ENTITE'   , i_poin, pair_enti)
                call apinfr(sdappa, 'APPARI_PROJ_KSI1', i_poin, ksipr1)
                call apinfr(sdappa, 'APPARI_PROJ_KSI2', i_poin, ksipr2)
                call apvect(sdappa, 'APPARI_TAU1'     , i_poin, tau1m)
                call apvect(sdappa, 'APPARI_TAU2'     , i_poin, tau2m)
                ASSERT(pair_type.ne.1)
!
! ------------- Coordinates of point
!
                call apcopt(sdappa, i_poin, poin_coor)
!
! ------------- Parameter about master element
!
                elem_mast_indx = pair_enti
                call cfnumm(ds_contact%sdcont_defi, elem_mast_indx, elem_mast_nume)
                call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
!
! ------------- Index of slave node in contact datastructure
!
                call mmpnoe(ds_contact%sdcont_defi,&
                            elem_slav_indx, elem_slav_type, type_inte, i_poin_elem, node_slav_indx)
!
! ------------- Index of slave node in mesh datastructures
!
                call mmnumn(mesh          , type_inte, elem_slav_nume, elem_slav_nbno, i_poin_elem,&
                            node_slav_nume)
!
! ------------- Coordinates of projection
!
                call mcopco(mesh  , newgeo        , model_ndim, elem_mast_nume, ksipr1,&
                            ksipr2, node_coor_proj)
!
! ------------- Contact point name
!
                call mmnpoi(mesh, elem_slav_name, node_slav_nume, i_poin_elem, poin_name)
!
! ------------- Define new local basis
!
                call mmtanr(mesh          , model_ndim    , ds_contact, i_zone,&
                            l_excl_frot   , node_slav_indx, ksipr1    , ksipr2     ,&
                            elem_mast_indx, elem_mast_nume, tau1m     , tau2m      , tau1  ,&
                            tau2)
                call mmnorm(model_ndim, tau1, tau2, norm, noor)
                if (noor .le. r8prem()) then
                    call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
                    call utmess('F', 'CONTACT3_24', sk=elem_mast_name)
                endif
!
! ------------- Compute gap
!
                call mmnewj(model_ndim, poin_coor, node_coor_proj, norm, gap)
                call cfdist(ds_contact, i_zone, elem_slav_indx, poin_coor, time_curr, &
                            gap_user)
                if (pair_type .eq. 2) then
                    enti_name = elem_mast_name
                    gap = gap+gap_user
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
! ------------- Save
!
                v_ncomp_jeux(i_ncomp_poin) = -gap
                v_ncomp_loca(i_ncomp_poin) = node_slav_nume
                v_ncomp_zone(i_ncomp_poin) = i_zone
                v_ncomp_enti(2*(i_ncomp_poin-1)+1) = poin_name
                v_ncomp_enti(2*(i_ncomp_poin-1)+2) = enti_name
!
! ------------- Next points
!
                i_ncomp_poin   = i_ncomp_poin + 1
                nt_ncomp_poin0 = nt_ncomp_poin0+ 1
                i_poin         = i_poin + 1
!
            end do
        end do
 25     continue
    end do
!
    ASSERT(nt_ncomp_poin0.eq.nt_ncomp_poin)
!
end subroutine
