subroutine mm_cycl_prop(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfl.h"
#include "asterfort/mm_cycl_laugf.h"
#include "asterfort/mm_cycl_zonf.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Propagating adapted ratio
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_poin, i_cont_poin, i_zone, cycl_stat
    character(len=24) :: sdcont_cyceta
    integer, pointer :: v_sdcont_cyceta(:) => null()
    character(len=24) :: sdcont_cychis
    real(kind=8), pointer :: v_sdcont_cychis(:) => null()
    character(len=24) :: sdcont_cyccoe
    real(kind=8), pointer :: v_sdcont_cyccoe(:) => null()
    real(kind=8) :: coef_frot, coef_frot_maxi, coef_frot_mini
    real(kind=8) :: nrese, nrese_prop, nrese_maxi, nrese_mini
    real(kind=8) :: pres_frot(3), dist_frot(3)
    aster_logical :: propa, l_frot_zone
    real(kind=8) :: tole_stick, tole_slide
    integer :: zone_frot, zone_frot_prop
!
! --------------------------------------------------------------------------------------------------
!
    nb_cont_poin = cfdisi(ds_contact%sdcont_defi,'NTPC' )
    tole_stick   = 0.95
    tole_slide   = 1.05
!
! - Acces to cycling objects
!
    sdcont_cyceta = ds_contact%sdcont_solv(1:14)//'.CYCETA'
    sdcont_cychis = ds_contact%sdcont_solv(1:14)//'.CYCHIS'
    sdcont_cyccoe = ds_contact%sdcont_solv(1:14)//'.CYCCOE'
    call jeveuo(sdcont_cyceta, 'E', vi = v_sdcont_cyceta)
    call jeveuo(sdcont_cychis, 'E', vr = v_sdcont_cychis)
    call jeveuo(sdcont_cyccoe, 'E', vr = v_sdcont_cyccoe)
!
! - Erasing cycling information
!
    do i_cont_poin = 1, nb_cont_poin
        i_zone      = nint(v_sdcont_cychis(60*(i_cont_poin-1)+60))
        l_frot_zone = mminfl(ds_contact%sdcont_defi,'FROTTEMENT_ZONE',i_zone)
        if (l_frot_zone) then
            cycl_stat      = v_sdcont_cyceta(4*(i_cont_poin-1)+2)
            coef_frot_mini = v_sdcont_cyccoe(6*(i_zone-1)+5)
            coef_frot_maxi = v_sdcont_cyccoe(6*(i_zone-1)+6)
            coef_frot      = v_sdcont_cychis(60*(i_cont_poin-1)+6)
            pres_frot(1)   = v_sdcont_cychis(60*(i_cont_poin-1)+7)
            pres_frot(2)   = v_sdcont_cychis(60*(i_cont_poin-1)+8)
            pres_frot(3)   = v_sdcont_cychis(60*(i_cont_poin-1)+9)
            dist_frot(1)   = v_sdcont_cychis(60*(i_cont_poin-1)+10)
            dist_frot(2)   = v_sdcont_cychis(60*(i_cont_poin-1)+11)
            dist_frot(3)   = v_sdcont_cychis(60*(i_cont_poin-1)+12)
            if (cycl_stat .ne. -1) then
!
! ------------- Norm of augmented lagrangian for friction
!
                call mm_cycl_laugf(pres_frot, dist_frot, coef_frot, nrese)
                call mm_cycl_laugf(pres_frot, dist_frot, coef_frot_maxi, nrese_maxi)
                call mm_cycl_laugf(pres_frot, dist_frot, coef_frot_mini, nrese_mini)
                nrese_prop = nrese_maxi
!
     10         continue
!
! ------------- Friction zone
!
                call mm_cycl_zonf(nrese, tole_stick, tole_slide, zone_frot)
                call mm_cycl_zonf(nrese_prop, tole_stick, tole_slide, zone_frot_prop)
!
! ------------- Propagation of adapted coefficient ?
!
                propa = .false.
                if (zone_frot .eq. zone_frot_prop) then
                    propa = .true.
                else
                    if ((zone_frot.eq.-2) .or. (zone_frot.eq.+2)) then
                        propa = .false.
                    else if (zone_frot.eq.-1) then
                        if (zone_frot_prop .eq. -2) then
                            propa = .true.
                        else
                            propa = .false.
                        endif
                    else if (zone_frot.eq.0) then
                        propa = .true.
                    else if (zone_frot.eq.+1) then
                        if (zone_frot_prop .eq. +2) then
                            propa = .true.
                        else
                            propa = .false.
                        endif
                    else
                        ASSERT(.false.)
                    endif
                endif
!
! ------------- New coefficient ?
!
                if (propa) then
                    if ( (coef_frot .ge. coef_frot_maxi-1.d-15) .or.&
                         (coef_frot .le. coef_frot_maxi+1.d-15) ) then
                        coef_frot = coef_frot_maxi
                    endif
                else
                    if ( (nrese_prop .ge. nrese_maxi-1.d-15) .or. &
                         (nrese_prop .le. nrese_maxi+1.d-15) )then
                        nrese_prop = nrese_mini
                        goto 10
                    endif
                endif
            endif
            v_sdcont_cychis(60*(i_cont_poin-1)+6) = coef_frot
        endif
    enddo
!
end subroutine
