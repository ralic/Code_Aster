subroutine rcvarc(arret    , varc_name_, poum,&
                  fami     , kpg       , ksp ,&
                  varc_vale, iret)
!
use calcul_module, only : ca_decala_, ca_iactif_, ca_iel_, ca_iredec_, &
     ca_jfpgl_, ca_jvcnom_, ca_km_, ca_kp_,&
     ca_kr_, ca_nbcvrc_, ca_nfpg_, ca_nomte_, ca_option_, &
     ca_td1_, ca_tf1_, ca_timed1_, ca_timef1_, ca_ctempl_, ca_ctempr_, ca_ctempm_, ca_ctempp_
!
implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/rcvarp.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=1), intent(in) :: arret
    character(len=*), intent(in) :: varc_name_
    character(len=*), intent(in) :: poum
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    real(kind=8), intent(out) :: varc_vale
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Get value of external state variable
!
! --------------------------------------------------------------------------------------------------
!
! In  arret            : in case of problem
!                     ' '   no message and output return code (iret)
!                     'F'   fatal error
! In  varc_name        : name of external state variable
! In  poum             : parameters evaluation
!                     '-' at beginning of step time
!                     '+' at end of step time
!                   'REF' for reference value
! In  fami             : Gauss family for integration point rule
! In  kpg              : current point gauss
! In  ksp              : current "sous-point" gauss
! Out varc_vale        : value of external state variable
! Out iret             : code if error
!                      0    if OK
!                      1    if not found
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: varc_name
    integer :: nb2vrc
    integer :: varc_indx, ibid, nbsp, kpgvrc
    integer :: iadzi, iazk24, kpgmat, vali(3)
    integer :: iprem = 0
    integer :: k, itabm(7), itabp(7), itabr(7)
    character(len=24) :: valk(4)
    character(len=8) :: nomail
    real(kind=8) :: valvrm, valvrp, rundf
    save itabm,itabp,itabr,rundf
!
! --------------------------------------------------------------------------------------------------
!
    if (iprem .eq. 0) then
        rundf = r8nnem()
        iprem = 1
    endif
    varc_name = varc_name_
!
! - From SIMU_POINT_MAT
!
    if (ca_iactif_ .eq. 2) then
        ASSERT(fami.eq.'PMAT')
        call rcvarp(arret, varc_name, poum, varc_vale, iret)
        goto 999
    endif
!
! - For coupled problems
!
    if (ca_ctempl_ .eq. 1) then
        if (varc_name .eq. 'TEMP') then
            if (poum .eq. '-') then
                varc_vale = ca_ctempm_
            elseif (poum .eq. '+') then
                varc_vale = ca_ctempp_
            elseif (poum .eq. 'REF') then
                varc_vale = ca_ctempr_
            else
                ASSERT(.false.)
            endif
            iret = 0
            goto 999
        endif
    endif
!
! - No external state variable
!
    if (ca_nbcvrc_ .eq. 0) then
        goto 998
    endif
!
! - Get index of external state variable
!
    varc_indx = indik8(zk8(ca_jvcnom_), varc_name, 1, ca_nbcvrc_)
    if (varc_indx .eq. 0) then
        iret=1
        if (arret .eq. ' ') then
            varc_vale=rundf
            goto 999
        else
            call tecael(iadzi, iazk24)
            nomail  = zk24(iazk24-1+3)(1:8)
            valk(1) = varc_name
            valk(2) = nomail
            valk(3) = poum
            call utmess('F', 'CALCUL_26', nk=3, valk=valk)
        endif
    endif
!
! - Get index of gauss point in MATER integration rule
!
    k = indik8(zk8(ca_jfpgl_), fami, 1, ca_nfpg_)
    if (k .eq. 0) then
        valk(1) = varc_name
        valk(2) = fami
        valk(3) = ca_option_
        valk(4) = ca_nomte_
        call utmess('F', 'CALCUL_31', nk=4, valk=valk)
    endif
    kpgmat = ca_decala_(k)+kpg
!
! - Get information about current element: using SAVE to save time if it's same element
!
    if (poum .eq. '-' .or. (poum.eq.'+' .and. ca_iredec_.eq.1)) then
        if (ca_iel_ .ne. ca_km_) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCMR', 'L', ibid, nval=7, itab=itabm)
            else
                call tecach('NNN', 'PVARCMR', 'L', iret, nval=7, itab=itabm)
                if (iret .ne. 0) goto 998
            endif
            ca_km_=ca_iel_
        endif
    endif

    if (poum .eq. '+' .or. (poum.eq.'-' .and. ca_iredec_.eq.1)) then
        if (ca_iel_ .ne. ca_kp_) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCPR', 'L', ibid, nval=7, itab=itabp)
            else
                call tecach('NNN', 'PVARCPR', 'L', iret, nval=7, itab=itabp)
                if (iret .ne. 0) goto 998
            endif
            ca_kp_=ca_iel_
        endif
    endif

    if (poum .eq. 'REF') then
        if (ca_iel_ .ne. ca_kr_) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCRR', 'L', ibid, nval=7, itab=itabr)
            else
                call tecach('NNN', 'PVARCRR', 'L', iret, nval=7, itab=itabr)
                if (iret .ne. 0) goto 998
            endif
            ca_kr_=ca_iel_
        endif
    endif
!
! - Get value
!
    if (poum .eq. 'REF') then
        nb2vrc=itabr(6)
        if (nb2vrc .ne. ca_nbcvrc_) goto 998
        nbsp=itabr(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        varc_vale=zr(itabr(1) -1 + (kpgvrc-1)*ca_nbcvrc_ + varc_indx)

    else if (poum.eq.'+' .and. ca_iredec_.eq.0) then
        nb2vrc=itabp(6)
        if (nb2vrc .ne. ca_nbcvrc_) goto 998
        nbsp=itabp(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        varc_vale=zr(itabp(1) -1 + (kpgvrc-1)*ca_nbcvrc_ + varc_indx)

    else if (poum.eq.'-' .and. ca_iredec_.eq.0) then
        nb2vrc=itabm(6)
        if (nb2vrc .ne. ca_nbcvrc_) goto 998
        nbsp=itabm(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        varc_vale=zr(itabm(1) -1 + (kpgvrc-1)*ca_nbcvrc_ + varc_indx)

    else if (ca_iredec_.eq.1) then
        nb2vrc=itabm(6)
        if (nb2vrc .ne. ca_nbcvrc_) goto 998
        nbsp=itabm(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrm=zr(itabm(1) -1 + (kpgvrc-1)*ca_nbcvrc_ + varc_indx)

        nb2vrc=itabp(6)
        if (nb2vrc .ne. ca_nbcvrc_) goto 998
        nbsp=itabp(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrp=zr(itabp(1) -1 + (kpgvrc-1)*ca_nbcvrc_ + varc_indx)

        if ((.not.isnan(valvrm)) .and. (.not.isnan(valvrp))) then
            if (poum .eq. '-') then
                varc_vale=valvrm+(ca_td1_-ca_timed1_)*(valvrp-valvrm)/(ca_timef1_-&
                ca_timed1_)
            else if (poum.eq.'+') then
                varc_vale=valvrm+(ca_tf1_-ca_timed1_)*(valvrp-valvrm)/(ca_timef1_-&
                ca_timed1_)
            else
                ASSERT(.false.)
            endif
        else
            varc_vale=rundf
        endif

    else
        ASSERT(.false.)
    endif
!
    iret=0
    if (isnan(varc_vale)) then
        iret=1
    endif
!
! - Manage error
!
    if (iret .eq. 1) then
        if (arret .eq. ' ') then
            varc_vale=rundf
        else
            call tecael(iadzi, iazk24)
            nomail=zk24(iazk24-1+3)(1:8)
            valk(1) = varc_name
            valk(2) = nomail
            call utmess('F', 'CALCUL_26', nk=2, valk=valk)
        endif
    endif
    goto 999
!
998 continue
    if (arret .eq. ' ') then
        varc_vale=rundf
        iret=1
    else
        call tecael(iadzi, iazk24)
        vali(1)=nb2vrc
        vali(2)=ca_nbcvrc_
        valk(1)=zk24(iazk24-1+3)
        call utmess('F', 'CALCUL_32', sk=valk(1), ni=2, vali=vali)
    endif
!
999 continue

end subroutine
