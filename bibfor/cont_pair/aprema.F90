subroutine aprema(sdappa, mesh, sdcont_defi, newgeo)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/apcopt.h"
#include "asterfort/apinfi.h"
#include "asterfort/aporth.h"
#include "asterfort/appari.h"
#include "asterfort/apparr.h"
#include "asterfort/approj.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/apzonr.h"
#include "asterfort/apzonv.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jeveuo.h"
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
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    character(len=19), intent(in) :: newgeo
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Find nearest element from current contact point
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_zone, i_poin, i, node_mast_indx
    integer :: nb_cont_zone, model_ndim, nt_poin
    integer :: nb_poin, proj_stat_mini, elem_mast_mini
    real(kind=8) :: poin_coor(3), tau1_mini(3), tau2_mini(3), dist_mini, ksi1_mini, ksi2_mini
    real(kind=8) :: pair_vect(3), tole_proj_ext, epsi_maxi, vect_pm_mini(3)
    integer :: iter_maxi
    aster_logical :: l_pair_dire, l_pair_masl, l_save
    integer :: pair_type, pair_enti
    character(len=24) :: sdappa_dist, sdappa_appa
    integer, pointer :: v_sdappa_appa(:) => null()
    real(kind=8), pointer :: v_sdappa_dist(:) => null()
    character(len=24) :: sdappa_tau1, sdappa_tau2, sdappa_proj
    real(kind=8), pointer :: v_sdappa_tau1(:) => null()
    real(kind=8), pointer :: v_sdappa_tau2(:) => null()
    real(kind=8), pointer :: v_sdappa_proj(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> RECH. MAILLE PLUS PROCHE'
    endif
!
! - Acces to pairing datastructure
!
    sdappa_appa = sdappa(1:19)//'.APPA'
    sdappa_dist = sdappa(1:19)//'.DIST'
    sdappa_tau1 = sdappa(1:19)//'.TAU1'
    sdappa_tau2 = sdappa(1:19)//'.TAU2'
    sdappa_proj = sdappa(1:19)//'.PROJ'
    call jeveuo(sdappa_appa, 'E', vi = v_sdappa_appa)
    call jeveuo(sdappa_dist, 'E', vr = v_sdappa_dist)
    call jeveuo(sdappa_tau1, 'E', vr = v_sdappa_tau1)
    call jeveuo(sdappa_tau2, 'E', vr = v_sdappa_tau2)
    call jeveuo(sdappa_proj, 'E', vr = v_sdappa_proj)
!
! - Get parameters
!
    call appari(sdappa, 'APPARI_NBZONE' , nb_cont_zone)
    call appari(sdappa, 'PROJ_NEWT_ITER', iter_maxi)
    call apparr(sdappa, 'PROJ_NEWT_RESI', epsi_maxi)
    call appari(sdappa, 'APPARI_NDIMG'  , model_ndim)
    call appari(sdappa, 'APPARI_NTPT'   , nt_poin)
!
! - Loop on contact zones
!
    i_poin = 1
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters on current zone
!
        call apzoni(sdappa, i_zone, 'NBPT'          , nb_poin)
        call apzonl(sdappa, i_zone, 'APPA_MAIT_ESCL', l_pair_masl)
        call apzonl(sdappa, i_zone, 'DIRE_APPA_FIXE', l_pair_dire)
        if (l_pair_dire) then
            call apzonv(sdappa, i_zone, 'DIRE_APPA_VECT', pair_vect)
        endif
        call apzonr(sdappa, i_zone, 'TOLE_PROJ_EXT', tole_proj_ext)
!
! ----- Loop on points
!
        do i = 1, nb_poin
!
! --------- Point to paired ?
!
            call apinfi(sdappa, 'APPARI_TYPE', i_poin, pair_type)
            ASSERT(pair_type.ne.0)
            if (l_pair_masl) then
!
! ------------- Coordinates of point
!
                call apcopt(sdappa, i_poin, poin_coor)
!
! ------------- Nearest master node
!
                call apinfi(sdappa, 'APPARI_ENTITE', i_poin, pair_enti)
                node_mast_indx = pair_enti
!
! ------------- Projection of contact point on master element
!
            call approj(mesh          , newgeo        , sdcont_defi , node_mast_indx, l_pair_dire,&
                        pair_vect     , iter_maxi     , epsi_maxi   , tole_proj_ext , poin_coor  ,&
                        elem_mast_mini, proj_stat_mini, ksi1_mini   , ksi2_mini     , tau1_mini  ,&
                        tau2_mini     , dist_mini     , vect_pm_mini)
!
! ------------- Orthogonalization of local basis
!
                call aporth(mesh     , sdcont_defi, model_ndim, elem_mast_mini, poin_coor,&
                            tau1_mini, tau2_mini)
                if (pair_type .eq. 1) then
                    if (proj_stat_mini .eq. 2) then
                        pair_type = -3
                    else
                        pair_type = 2
                    endif
                endif
                l_save = .true.
            else
                l_save = .false.
            endif
!
! --------- Save
!
            if (l_save) then
                v_sdappa_appa(4*(i_poin-1)+1) = pair_type
                v_sdappa_appa(4*(i_poin-1)+2) = elem_mast_mini
                v_sdappa_appa(4*(i_poin-1)+3) = i_zone
                v_sdappa_dist(4*(i_poin-1)+1) = dist_mini
                v_sdappa_dist(4*(i_poin-1)+2) = vect_pm_mini(1)
                v_sdappa_dist(4*(i_poin-1)+3) = vect_pm_mini(2)
                v_sdappa_dist(4*(i_poin-1)+4) = vect_pm_mini(3)
                v_sdappa_proj(2*(i_poin-1)+1) = ksi1_mini
                v_sdappa_proj(2*(i_poin-1)+2) = ksi2_mini
                v_sdappa_tau1(3*(i_poin-1)+1) = tau1_mini(1)
                v_sdappa_tau1(3*(i_poin-1)+2) = tau1_mini(2)
                v_sdappa_tau1(3*(i_poin-1)+3) = tau1_mini(3)
                v_sdappa_tau2(3*(i_poin-1)+1) = tau2_mini(1)
                v_sdappa_tau2(3*(i_poin-1)+2) = tau2_mini(2)
                v_sdappa_tau2(3*(i_poin-1)+3) = tau2_mini(3)
            endif
!
! --------- Next point
!
            i_poin = i_poin + 1
        end do
    end do
!
end subroutine
