subroutine mm_cycl_prop(sd_cont_defi, sd_cont_solv, cycl_hist, cycl_coef)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfl.h"
#include "asterfort/mm_cycl_laugf.h"
#include "asterfort/mm_cycl_zonf.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
    real(kind=8), intent(inout) :: cycl_hist(*)
    real(kind=8), intent(inout) :: cycl_coef(*)
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method)
!
! Propagating adapted ratio
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_cont_solv   : data structure for contact solving
! In  sd_cont_defi   : data structure from contact definition 
! I/O cycl_hist      : cycling history
! I/O cycl_coef      : coefficient history
!
! --------------------------------------------------------------------------------------------------
!
    integer :: point_number, point_index, zone_index, cycl_stat
    character(len=24) :: sd_cycl_eta
    integer ::  jcyeta
    real(kind=8) :: coef_frot, coef_frot_maxi, coef_frot_mini
    real(kind=8) :: nrese, nrese_prop, nrese_maxi, nrese_mini
    real(kind=8) :: pres_frot(3), dist_frot(3)
    logical :: propa, l_frot_zone
    real(kind=8) :: tole_stick, tole_slide
    integer :: zone_frot, zone_frot_prop
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    point_number = cfdisi(sd_cont_defi,'NTPC' )
    tole_stick = 0.95
    tole_slide = 1.05
!
! - Acces to cycling objects
!
    sd_cycl_eta = sd_cont_solv(1:14)//'.CYCETA'
    call jeveuo(sd_cycl_eta, 'L', jcyeta)
!
! - Erasing cycling information
!
    do point_index = 1, point_number
        zone_index     = nint(cycl_hist(25*(point_index-1)+25))
        l_frot_zone    = mminfl(sd_cont_defi,'FROTTEMENT_ZONE',zone_index)
        if (.not.l_frot_zone) goto 15
        cycl_stat  = zi(jcyeta-1+4*(point_index-1)+2)
        coef_frot_mini = cycl_coef(6*(zone_index-1)+5)
!        coef_frot_init = cycl_coef(6*(zone_index-1)+2)
        coef_frot_maxi = cycl_coef(6*(zone_index-1)+6)
        coef_frot      = cycl_hist(25*(point_index-1)+6)
        pres_frot(1)   = cycl_hist(25*(point_index-1)+7)
        pres_frot(2)   = cycl_hist(25*(point_index-1)+8)
        pres_frot(3)   = cycl_hist(25*(point_index-1)+9)
        dist_frot(1)   = cycl_hist(25*(point_index-1)+10)
        dist_frot(2)   = cycl_hist(25*(point_index-1)+11)
        dist_frot(3)   = cycl_hist(25*(point_index-1)+12)
        if (cycl_stat.ne.-1) then
!
! --------- Norm of augmented lagrangian for friction
!
            call mm_cycl_laugf(pres_frot, dist_frot, coef_frot     , nrese)
            call mm_cycl_laugf(pres_frot, dist_frot, coef_frot_maxi, nrese_maxi)
            call mm_cycl_laugf(pres_frot, dist_frot, coef_frot_mini, nrese_mini)
            nrese_prop = nrese_maxi

10          continue
!
! --------- Friction zone
!
            call mm_cycl_zonf(nrese     , tole_stick, tole_slide, zone_frot)
            call mm_cycl_zonf(nrese_prop, tole_stick, tole_slide, zone_frot_prop)
!
! --------- Propagation of adapted coefficient ?
!
            propa = .false.
            if (zone_frot.eq.zone_frot_prop) then
                propa = .true.
            else
                if ((zone_frot.eq.-2).or.(zone_frot.eq.+2)) then
                    propa = .false.
                elseif (zone_frot.eq.-1) then
                    if (zone_frot_prop.eq.-2) then
                        propa = .true.
                    else
                        propa = .false.
                    endif
               elseif (zone_frot.eq.0) then
                    propa = .true.
               elseif (zone_frot.eq.+1) then
                    if (zone_frot_prop.eq.+2) then
                        propa = .true.
                    else
                        propa = .false.
                    endif
               else
                    ASSERT(.false.)
               endif
            endif
!
! --------- New coefficient ?
!
            if (propa) then
                if (coef_frot.ne.coef_frot_maxi) then
                    coef_frot = coef_frot_maxi
                endif     
            else
                if (nrese_prop.eq.nrese_maxi) then
                    nrese_prop = nrese_mini
                    goto 10
                endif
            endif

        endif
        cycl_hist(25*(point_index-1)+6) = coef_frot

15      continue
    enddo
!
    call jedema()
end subroutine
