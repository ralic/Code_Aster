subroutine aprend(sdappa, sdcont_defi, newgeo)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8gaem.h"
#include "asterc/r8prem.h"
#include "asterfort/apcopt.h"
#include "asterfort/cfdisi.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jeveuo.h"
#include "blas/dcopy.h"
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
    character(len=24), intent(in) :: sdcont_defi
    character(len=19), intent(in) :: newgeo
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Find nearest master node from current contact point
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_node_mast, i_zone, i, i_poin
    integer :: nb_cont_zone, nt_poin
    integer :: nb_node_mast, nb_poin
    real(kind=8) :: node_mast_coor(3), poin_coor(3)
    real(kind=8) :: dist_mini, dist
    real(kind=8) :: normd, normv, pair_vect(3), pair_tole
    real(kind=8) :: vect_pm(3), vect_pm_mini(3)
    integer :: jdecnm, node_mast_nume, node_mast_indx
    integer :: node_mini_indx, pair_type
    aster_logical :: l_pair_dire, l_proj_tole, l_poin_excl
    character(len=24) :: sdappa_infp, sdcont_noeuco, newgeo_vale
    integer, pointer :: v_sdappa_infp(:) => null()
    integer, pointer :: v_sdcont_noeuco(:) => null()
    real(kind=8), pointer :: v_newgeo_vale(:) => null()
    character(len=24) :: sdappa_dist, sdappa_appa
    integer, pointer :: v_sdappa_appa(:) => null()
    real(kind=8), pointer :: v_sdappa_dist(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> RECH. NOEUD PLUS PROCHE'
    endif
!
! - Acces to pairing datastructure
!
    sdappa_infp = sdappa(1:19)//'.INFP'
    sdappa_appa = sdappa(1:19)//'.APPA'
    sdappa_dist = sdappa(1:19)//'.DIST'
    call jeveuo(sdappa_infp, 'L', vi = v_sdappa_infp)
    call jeveuo(sdappa_appa, 'E', vi = v_sdappa_appa)
    call jeveuo(sdappa_dist, 'E', vr = v_sdappa_dist)
!
! - Acces to contact datastructure
!
    sdcont_noeuco = sdcont_defi(1:16)//'.NOEUCO'
    call jeveuo(sdcont_noeuco, 'L', vi = v_sdcont_noeuco)
!
! - Acces to updated geometry
!
    newgeo_vale = newgeo(1:19)//'.VALE'
    call jeveuo(newgeo_vale, 'L', vr = v_newgeo_vale)
!
! - Get parameters
!
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO' )
    nt_poin      = cfdisi(sdcont_defi,'NTPT'  )
!
! - Loop on contact zones
!
    i_poin = 1
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters on current zone
!
        jdecnm       = mminfi(sdcont_defi, 'JDECNM'   , i_zone)
        pair_tole    = mminfr(sdcont_defi, 'TOLE_APPA', i_zone)
        nb_poin      = mminfi(sdcont_defi, 'NBPT'     , i_zone)
        nb_node_mast = mminfi(sdcont_defi, 'NBNOM'    , i_zone)
        l_pair_dire  = mminfi(sdcont_defi, 'TYPE_APPA', i_zone).eq.1
        if (l_pair_dire) then
            pair_vect(1) = mminfr(sdcont_defi, 'TYPE_APPA_DIRX', i_zone)
            pair_vect(2) = mminfr(sdcont_defi, 'TYPE_APPA_DIRY', i_zone)
            pair_vect(3) = mminfr(sdcont_defi, 'TYPE_APPA_DIRZ', i_zone)            
        endif

!
! ----- Loop on points
!
        do i = 1, nb_poin
!
            dist_mini      = r8gaem()
            l_proj_tole    = .false.
            l_poin_excl    = .false.
            node_mini_indx = 0
            pair_type      = 0
!
! --------- Coordinates of point
!
            call apcopt(sdappa, i_poin, poin_coor)
!
! --------- Excluded point ?
!
            l_poin_excl = v_sdappa_infp(i_poin) .eq. 1
!
! --------- Loop on master nodes
!
            do i_node_mast = 1, nb_node_mast
!
! ------------- Current node
!
                node_mast_indx = jdecnm + i_node_mast
                node_mast_nume = v_sdcont_noeuco(node_mast_indx)
                node_mast_coor(1) = v_newgeo_vale(3*(node_mast_nume-1)+1)
                node_mast_coor(2) = v_newgeo_vale(3*(node_mast_nume-1)+2)
                node_mast_coor(3) = v_newgeo_vale(3*(node_mast_nume-1)+3)
!
! ------------- Compute distance
!
                if (l_pair_dire) then
                    normd = sqrt(pair_vect(1)*pair_vect(1)+&
                                 pair_vect(2)*pair_vect(2)+&
                                 pair_vect(3)*pair_vect(3))
                    normv = sqrt((poin_coor(1)-node_mast_coor(1))**2+&
                                 (poin_coor(2)-node_mast_coor(2))**2+&
                                 (poin_coor(3)-node_mast_coor(3))**2)
                    if (normv .le. r8prem()) then
                        dist = 1.d0
                    else
                        dist = abs((poin_coor(1)-node_mast_coor(1))*pair_vect(1)+&
                                   (poin_coor(2)-node_mast_coor(2))*pair_vect(2)+&
                                   (poin_coor(3)-node_mast_coor(3))*pair_vect(3))/&
                                   (normd*normv)
                    endif
                else
                    dist = sqrt((poin_coor(1)-node_mast_coor(1))**2+&
                                (poin_coor(2)-node_mast_coor(2))**2+&
                                (poin_coor(3)-node_mast_coor(3))**2)
                endif
                vect_pm(1) = node_mast_coor(1) - poin_coor(1)
                vect_pm(2) = node_mast_coor(2) - poin_coor(2)
                vect_pm(3) = node_mast_coor(3) - poin_coor(3)
!
! ------------- Select distance
!
                if (dist .lt. dist_mini) then
                    node_mini_indx = node_mast_indx
                    dist_mini      = dist
                    call dcopy(3, vect_pm, 1, vect_pm_mini, 1)
                    if (pair_tole .gt. 0.d0) then
                        if (dist .le. pair_tole) then
                            l_proj_tole = .true.
                        endif
                    else
                        l_proj_tole = .true.
                    endif
                endif
            end do
!
! --------- Check TOLE_APPA
!
            if (l_proj_tole) then
                pair_type = 1
            else
                pair_type = -2
            endif
!
! --------- Excluded node
!
            if (l_poin_excl) then
                pair_type = -1
            endif
!
! --------- Some checks
!
            ASSERT(pair_type.ne.0)
            ASSERT(node_mini_indx.ne.0)
            ASSERT((pair_type.eq.-2).or.(pair_type.eq.-1).or. (pair_type.eq.1))
!
! --------- Save
!
            v_sdappa_appa(4*(i_poin-1)+1) = pair_type
            v_sdappa_appa(4*(i_poin-1)+2) = node_mini_indx
            v_sdappa_appa(4*(i_poin-1)+3) = i_zone
            v_sdappa_dist(4*(i_poin-1)+1) = dist_mini
            v_sdappa_dist(4*(i_poin-1)+2) = vect_pm_mini(1)
            v_sdappa_dist(4*(i_poin-1)+3) = vect_pm_mini(2)
            v_sdappa_dist(4*(i_poin-1)+4) = vect_pm_mini(3)
!
! --------- Next point
!
            i_poin = i_poin + 1
        end do
    end do
!
    ASSERT((i_poin-1).eq.nt_poin)
!
end subroutine
