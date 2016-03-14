subroutine cfresu(time_incr, sddisc, ds_contact, disp_cumu_inst, disp_iter,&
                  cnsinr   , cnsper)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/apinfi.h"
#include "asterfort/caladu.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfresa.h"
#include "asterfort/cfresb.h"
#include "asterfort/iseven.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmnorm.h"
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
    real(kind=8), intent(in) :: time_incr
    character(len=19), intent(in) :: sddisc
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: disp_cumu_inst
    character(len=19), intent(in) :: disp_iter
    character(len=19), intent(in) :: cnsinr
    character(len=19), intent(in) :: cnsper
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Post-treatment
!
! Discrete method - Prepare post-treatment fields
!
! --------------------------------------------------------------------------------------------------
!
! In  time_incr        : time increment
! In  sddisc           : datastructure for discretization
! In  ds_contact       : datastructure for contact management
! In  disp_cumu_inst   : displacement increment from beginning of current time
! In  disp_iter        : displacement iteration
! In  cnsinr           : nodal field (CHAM_NO_S) for CONT_NOEU 
! In  cnsper           : nodal field (CHAM_NO_S) to save percussions
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8), parameter :: eps=1.d-6
    integer :: zresu, zperc, ztacf
    integer :: node_slav_nume, iliac, icmp, iliai
    integer :: model_ndim, nbliai, nb_dof, nb_equa
    integer :: nbliac
    integer :: jdecal, lliac
    integer :: nesmax, pair_type
    real(kind=8) :: glix, gliy, glit
    real(kind=8) :: testmu, testcf, coefpt
    real(kind=8) :: val1, val2, node_status
    real(kind=8) :: r, rx, ry, rz
    real(kind=8) :: rn, rnx, rny, rnz, hn
    real(kind=8) :: rtax, rtay, rtaz, rtgx, rtgy, rtgz
    real(kind=8) :: tau1(3), tau2(3), norm(3), proj(3)
    character(len=19) :: sdappa
    character(len=19) :: sdcont_liac, sdcont_atmu, sdcont_afmu, sdcont_mu
    integer :: jatmu, jafmu
    character(len=24) :: sdcont_apddl, sdcont_apcofr
    integer :: japddl, japcof
    character(len=24) :: sdcont_tangco, sdcont_tacfin
    character(len=24) :: sdcont_appoin, sdcont_numlia, sdcont_approj
    character(len=24) :: sdcont_jeuite
    aster_logical :: l_cont_pena, l_frot
    aster_logical :: lcolli, laffle
    real(kind=8) :: imp, impx, impy, impz
    real(kind=8) :: valras
    real(kind=8), pointer :: v_disp_iter(:) => null()
    real(kind=8), pointer :: v_disp_cumu(:) => null()
    integer, pointer :: v_sdcont_liac(:) => null()
    integer, pointer :: v_sdcont_appoin(:) => null()
    integer, pointer :: v_sdcont_numlia(:) => null()
    real(kind=8), pointer :: v_sdcont_tangco(:) => null()
    real(kind=8), pointer :: v_sdcont_jeuite(:) => null()
    real(kind=8), pointer :: v_sdcont_tacfin(:) => null()
    real(kind=8), pointer :: v_sdcont_mu(:) => null()
    real(kind=8), pointer :: v_sdcont_approj(:) => null()
    real(kind=8), pointer :: v_cnsper_cnsv(:) => null()
    real(kind=8), pointer :: v_cnsinr_cnsv(:) => null()
    aster_logical, pointer :: v_cnsper_cnsl(:) => null()
    aster_logical, pointer :: v_cnsinr_cnsl(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
!
! - Parameters
!
    l_frot      = cfdisl(ds_contact%sdcont_defi,'FROT_DISCRET')
    l_cont_pena = cfdisl(ds_contact%sdcont_defi,'CONT_PENA' )
    nbliai      = cfdisd(ds_contact%sdcont_solv,'NBLIAI')
    nbliac      = cfdisd(ds_contact%sdcont_solv,'NBLIAC')
    model_ndim  = cfdisd(ds_contact%sdcont_solv,'NDIM' )
    nb_equa     = cfdisd(ds_contact%sdcont_solv,'NEQ' )
    nesmax      = cfdisd(ds_contact%sdcont_solv,'NESMAX')
!
! - Collision
!
    laffle = .false.
    valras = 1.d-3
    call iseven(sddisc, 'COLLISION', lcolli)
!
! - Acces to contact objects
!
    zresu = cfmmvd('ZRESU')
    zperc = cfmmvd('ZPERC')
    ztacf = cfmmvd('ZTACF')
    sdcont_afmu   = ds_contact%sdcont_solv(1:14)//'.AFMU'
    sdcont_apcofr = ds_contact%sdcont_solv(1:14)//'.APCOFR'
    sdcont_apddl  = ds_contact%sdcont_solv(1:14)//'.APDDL'
    sdcont_appoin = ds_contact%sdcont_solv(1:14)//'.APPOIN'
    sdcont_numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
    sdcont_atmu   = ds_contact%sdcont_solv(1:14)//'.ATMU'
    sdcont_liac   = ds_contact%sdcont_solv(1:14)//'.LIAC'
    sdcont_mu     = ds_contact%sdcont_solv(1:14)//'.MU'
    sdcont_tangco = ds_contact%sdcont_solv(1:14)//'.TANGCO'
    sdcont_tacfin = ds_contact%sdcont_solv(1:14)//'.TACFIN'
    sdcont_jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    sdcont_approj = ds_contact%sdcont_solv(1:14)//'.APPROJ'
    call jeveuo(sdcont_appoin, 'L', vi = v_sdcont_appoin)
    call jeveuo(sdcont_tacfin, 'L', vr = v_sdcont_tacfin)
    call jeveuo(sdcont_numlia, 'L', vi = v_sdcont_numlia)
    call jeveuo(sdcont_apddl , 'L', japddl)
    call jeveuo(sdcont_atmu  , 'L', jatmu)
    if (l_frot) then
        call jeveuo(sdcont_apcofr, 'L', japcof)
        call jeveuo(sdcont_afmu  , 'L', jafmu)
    endif
    if (l_cont_pena) then
        call jeveuo(sdcont_afmu, 'L', jafmu)
    endif
    call jeveuo(sdcont_liac  , 'L', vi  = v_sdcont_liac)
    call jeveuo(sdcont_mu    , 'L', vr  = v_sdcont_mu)
    call jeveuo(sdcont_tangco, 'L', vr  = v_sdcont_tangco)
    call jeveuo(sdcont_jeuite, 'L', vr  = v_sdcont_jeuite)
    call jeveuo(sdcont_approj, 'L', vr  = v_sdcont_approj)
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! - Access to fields
!
    call jeveuo(disp_iter(1:19)//'.VALE', 'L', vr = v_disp_iter)
    call jeveuo(disp_cumu_inst(1:19)//'.VALE', 'L', vr = v_disp_cumu)
    call jeveuo(cnsinr(1:19)//'.CNSV', 'E', vr = v_cnsinr_cnsv)
    call jeveuo(cnsinr(1:19)//'.CNSL', 'E', vl = v_cnsinr_cnsl)
    call jeveuo(cnsper(1:19)//'.CNSV', 'E', vr = v_cnsper_cnsv)
    call jeveuo(cnsper(1:19)//'.CNSL', 'E', vl = v_cnsper_cnsl)
!
! - Active nodes - Initializatios
!
    do iliai = 1, nbliai
        node_slav_nume = v_sdcont_numlia(4*(iliai-1)+3)
        do icmp = 1, zresu
            v_cnsinr_cnsl(zresu*(node_slav_nume-1)+icmp) = .true.
            v_cnsinr_cnsv(zresu*(node_slav_nume-1)+icmp) = 0.d0
        end do
        do icmp = 1, zperc
            v_cnsper_cnsl(zperc*(node_slav_nume-1)+icmp) = .true.
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+icmp) = 0.d0
        end do
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+2 ) = v_sdcont_jeuite(3*(iliai-1)+1)
    end do
!
! - Set fields
!
    do iliac = 1, nbliac
!
        node_status = 2.d0
        rtax = 0.d0
        rtay = 0.d0
        rtaz = 0.d0
        rtgx = 0.d0
        rtgy = 0.d0
        rtgz = 0.d0
        hn   = 0.d0
!
! ----- Current link
!
        lliac  = v_sdcont_liac(iliac)
        jdecal = v_sdcont_appoin(lliac)
        nb_dof = v_sdcont_appoin(lliac+1) - v_sdcont_appoin(lliac)
!
! ----- Current node
!
        node_slav_nume = v_sdcont_numlia(4*(lliac-1)+3)
!
! ----- Projection
!
        proj(1) = v_sdcont_approj(3*(lliac-1)+1)
        proj(2) = v_sdcont_approj(3*(lliac-1)+2)
        proj(3) = v_sdcont_approj(3*(lliac-1)+3)
!
! ----- Tangents
!
        tau1(1) = v_sdcont_tangco(6*(lliac-1)+1)
        tau1(2) = v_sdcont_tangco(6*(lliac-1)+2)
        tau1(3) = v_sdcont_tangco(6*(lliac-1)+3)
        tau2(1) = v_sdcont_tangco(6*(lliac-1)+4)
        tau2(2) = v_sdcont_tangco(6*(lliac-1)+5)
        tau2(3) = v_sdcont_tangco(6*(lliac-1)+6)
!
! ----- Get pairing
!
        call apinfi(sdappa, 'APPARI_TYPE', lliac, pair_type)
        if (pair_type.lt.0) then
            node_status = -1.d0
        endif
!
! ----- Compute normal
!
        call mmnorm(model_ndim, tau1, tau2, norm)
!
! ----- Compute normal reactions for contact
!
        if (l_cont_pena) then
            call cfresa(model_ndim, zr(jafmu+zi(japddl+jdecal)-1), norm, rnx, rny,&
                        rnz, rn)
        else
            call cfresa(model_ndim, zr(jatmu+zi(japddl+jdecal)-1), norm, rnx, rny,&
                        rnz, rn)
        endif
!
! ----- Very near contact
!
        if (rn .le. valras) then
            laffle = .true.
        endif
!
! ----- Compute informations for friction
!
        if (l_frot) then
!
            node_status = 2.d0
!
! --------- Compute slides
!
            call caladu(nb_equa, nb_dof, zr(japcof+jdecal), zi(japddl+jdecal), v_disp_cumu,&
                        val1)
            call caladu(nb_equa, nb_dof, zr(japcof+jdecal), zi(japddl+jdecal), v_disp_iter,&
                        val2)
            glix = val1 + val2
            gliy = 0.d0
            if (model_ndim .eq. 3) then
                call caladu(nb_equa, nb_dof, zr(japcof+jdecal+30*nesmax),&
                            zi(japddl+jdecal), v_disp_cumu,&
                            val1)
                call caladu(nb_equa, nb_dof, zr(japcof+jdecal+30*nesmax),&
                            zi(japddl+jdecal), v_disp_iter,&
                            val2)
                gliy = val1 + val2
            endif
            glit = sqrt(glix**2+gliy**2)
!
! --------- Compute tangential forces
!
            call cfresb(model_ndim, 'GL', zr(jafmu+zi(japddl+jdecal)-1),&
                        tau1, tau2, rtgx, rtgy, rtgz)
            testmu = v_sdcont_mu(3*nbliai+lliac)
            coefpt = v_sdcont_tacfin(ztacf*(lliac-1)+3)
            testcf = sqrt(coefpt)
            if (testcf .gt. r8miem()) then
                if (abs((testmu-testcf)/testcf) .gt. r8prem()) then
                    node_status = 1.d0
                    rtax = rtgx
                    rtay = rtgy
                    rtaz = rtgz
                    rtgx = 0.d0
                    rtgy = 0.d0
                    rtgz = 0.d0
                    hn = 0.d0
                else
                    node_status = 2.d0
                    rtax = 0.d0
                    rtay = 0.d0
                    rtaz = 0.d0
                    hn = 0.d0
                endif
            endif
        endif
!
! ----- Total reaction
!
        rx = rnx + rtax + rtgx
        ry = rny + rtay + rtgy
        rz = rnz + rtaz + rtgz
        r  = sqrt(rx**2+ry**2+rz**2)
!
! ----- Percussion
!
        if (r .le. eps) then
            imp  = 0.d0
            impx = 0.d0
            impy = 0.d0
            impz = 0.d0
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+1) = 0.d0
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+2) = 0.d0
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+3) = 0.d0
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+4) = 0.d0
        else
            imp  = v_cnsper_cnsv(zperc*(node_slav_nume-1)+1) + r*time_incr
            impx = v_cnsper_cnsv(zperc*(node_slav_nume-1)+2) + rx*time_incr
            impy = v_cnsper_cnsv(zperc*(node_slav_nume-1)+3) + ry*time_incr
            impz = v_cnsper_cnsv(zperc*(node_slav_nume-1)+4) + rz*time_incr
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+1) = imp
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+2) = impx        
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+3) = impy       
            v_cnsper_cnsv(zperc*(node_slav_nume-1)+4) = impz
        endif
!
! ----- Save in CONT_NOEU field
!
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+1 ) = node_status
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+3 ) = rn
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+4 ) = rnx
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+5 ) = rny
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+6 ) = rnz
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+7 ) = glix
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+8 ) = gliy
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+9 ) = glit
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+10) = rtax
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+11) = rtay
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+12) = rtaz
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+13) = rtgx
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+14) = rtgy
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+15) = rtgz
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+16) = rx
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+17) = ry
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+18) = rz
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+19) = r
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+20) = hn
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+21) = imp
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+22) = impx
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+23) = impy
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+24) = impz
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+25) = 0.d0
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+26) = 0.d0
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+27) = 0.d0
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+28) = proj(1)
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+29) = proj(2)
        v_cnsinr_cnsv(zresu*(node_slav_nume-1)+30) = proj(3)
    end do
!
! - Alarm for COLLISION
!
    if (laffle .and. lcolli) then
        call utmess('A', 'CONTACT3_98')
    endif
!
    call jedema()
!
end subroutine
