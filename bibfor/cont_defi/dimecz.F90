subroutine dimecz(sdcont, mesh, nb_cont_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/cfzone.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmnbnz.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
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
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Save contact counters - Counters by zone
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  mesh             : name of mesh
! In  nb_cont_zone     : number of zones of contact
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_methco
    integer, pointer :: v_sdcont_methco(:) => null()
    integer :: zmeth
    integer :: i_zone
    integer :: jdecme, jdecmm, jdecne, jdecnm
    integer :: i_surf_escl, i_surf_mast
    integer :: nb_node_mast, nb_node_slav, nb_elem_mast, nb_elem_slav
    integer :: nb_node_mastc, nb_node_slavc, nb_elem_mastc, nb_elem_slavc
    integer :: nb_cont_poin
    character(len=24) :: sdcont_defi
    integer :: cont_form
    aster_logical :: l_verif
    integer :: nb_cont_poinc
!
! ----------------------------------------------------------------------
!
    nb_elem_slav  = 0
    nb_node_slav  = 0
    nb_elem_mast  = 0
    nb_elem_slav  = 0
    nb_cont_poin  = 0
    nb_elem_slavc = 0
    nb_node_slavc = 0
    nb_elem_mastc = 0
    nb_elem_slavc = 0
    nb_cont_poinc = 0
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
!
! - Parameters
!
    cont_form     = cfdisi(sdcont_defi, 'FORMULATION')
!
! - Datastructure for contact
!
    sdcont_methco = sdcont_defi(1:16)//'.METHCO'
    call jeveuo(sdcont_methco, 'E', vi = v_sdcont_methco)
    zmeth = cfmmvd('ZMETH')
!
! - Total number elements/zones and nodes / zones
!
    do i_zone = 1, nb_cont_zone
        call cfzone(sdcont_defi, i_zone, 'ESCL', i_surf_escl)
        call cfnbsf(sdcont_defi, i_surf_escl, 'MAIL', nb_elem_slav, jdecme)
        call cfnbsf(sdcont_defi, i_surf_escl, 'NOEU', nb_node_slav, jdecne)
        call cfzone(sdcont_defi, i_zone, 'MAIT', i_surf_mast)
        call cfnbsf(sdcont_defi, i_surf_mast, 'MAIL', nb_elem_mast, jdecmm)
        call cfnbsf(sdcont_defi, i_surf_mast, 'NOEU', nb_node_mast, jdecnm)
        l_verif = mminfl(sdcont_defi, 'VERIF', i_zone)
        if (l_verif) then
            nb_elem_slavc = 0
            nb_elem_mastc = 0
            nb_node_slavc = 0
            nb_node_mastc = 0
        else
            nb_elem_slavc = nb_elem_slav
            nb_elem_mastc = nb_elem_mast
            nb_node_slavc = nb_node_slav
            nb_node_mastc = nb_node_mast
        endif
        v_sdcont_methco(zmeth*(i_zone-1)+8 ) = nb_elem_slav
        v_sdcont_methco(zmeth*(i_zone-1)+9 ) = nb_node_slav
        v_sdcont_methco(zmeth*(i_zone-1)+10) = nb_elem_mast
        v_sdcont_methco(zmeth*(i_zone-1)+11) = nb_node_mast
        v_sdcont_methco(zmeth*(i_zone-1)+12) = nb_elem_slavc
        v_sdcont_methco(zmeth*(i_zone-1)+13) = nb_node_slavc
        v_sdcont_methco(zmeth*(i_zone-1)+14) = nb_elem_mastc
        v_sdcont_methco(zmeth*(i_zone-1)+15) = nb_node_mastc
    end do
!
! - Shift/zone
!
    do i_zone = 1, nb_cont_zone
        call cfzone(sdcont_defi, i_zone, 'ESCL', i_surf_escl)
        call cfnbsf(sdcont_defi, i_surf_escl, 'MAIL', nb_elem_slav, jdecme)
        call cfnbsf(sdcont_defi, i_surf_escl, 'NOEU', nb_node_slav, jdecne)
        call cfzone(sdcont_defi, i_zone, 'MAIT', i_surf_mast)
        call cfnbsf(sdcont_defi, i_surf_mast, 'MAIL', nb_elem_mast, jdecmm)
        call cfnbsf(sdcont_defi, i_surf_mast, 'NOEU', nb_node_mast, jdecnm)
        v_sdcont_methco(zmeth*(i_zone-1)+16) = jdecme
        v_sdcont_methco(zmeth*(i_zone-1)+17) = jdecmm
        v_sdcont_methco(zmeth*(i_zone-1)+18) = jdecne
        v_sdcont_methco(zmeth*(i_zone-1)+19) = jdecnm
    end do
!
! - Number of contact points/zone
!
    do i_zone = 1, nb_cont_zone
        call mmnbnz(mesh, sdcont_defi, i_zone, nb_cont_poin)
        v_sdcont_methco(zmeth*(i_zone-1)+20) = nb_cont_poin
    end do
!
! - No computation mode
!
    do i_zone = 1, nb_cont_zone
        nb_cont_poin = v_sdcont_methco(zmeth*(i_zone-1)+20)
        l_verif = mminfl(sdcont_defi, 'VERIF', i_zone)
        if (l_verif) then
            nb_cont_poinc = 0
        else
            nb_cont_poinc = nb_cont_poin
        endif
        v_sdcont_methco(zmeth*(i_zone-1)+21) = nb_cont_poinc
    end do
!
end subroutine
