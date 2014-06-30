subroutine thm_kit_read(keywordfact, iocc     , rela_comp, rela_thmc, rela_hydr, &
                        rela_meca  , rela_ther)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=16), intent(in) :: rela_comp
    character(len=16), intent(out) :: rela_thmc
    character(len=16), intent(out) :: rela_hydr
    character(len=16), intent(out) :: rela_meca
    character(len=16), intent(out) :: rela_ther
!
! --------------------------------------------------------------------------------------------------
!
! THM
!
! Read informations for KIT
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact  : factor keyword to read (COMPORTEMENT)
! In  iocc         : factor keyword index in COMPORTEMENT
! In  rela_comp    : comportment relation (KIT_*)
! Out rela_thmc    : relation for coupling
! Out rela_hydr    : relation for hydraulic
! Out rela_meca    : relation for mechanic
! Out rela_ther    : relation for thermic
!
! --------------------------------------------------------------------------------------------------
!
    integer :: dmthmc, dmhydr, dmmeca
    parameter  ( dmthmc = 8, dmhydr = 4, dmmeca = 19 )
    character(len=16) :: pothmc(dmthmc), pohydr(dmhydr), pomeca(dmmeca)
    character(len=16) :: valk(3)
    integer :: ikit, ii, nocc
    logical(kind=1) :: l_thmc, l_hydr, l_meca
    character(len=16) :: rela_kit(4)
!
! --------------------------------------------------------------------------------------------------
!
    data pothmc / 'LIQU_SATU'       ,'LIQU_GAZ'        ,'GAZ'             ,&
                  'LIQU_GAZ_ATM'    ,'LIQU_VAPE_GAZ'   ,'LIQU_VAPE'       ,&
                  'LIQU_AD_GAZ_VAPE','LIQU_AD_GAZ'/
    data pohydr / 'HYDR_UTIL'       ,'HYDR_VGM'        ,&
                  'HYDR_VGC'        ,'HYDR_ENDO'/
    data pomeca / 'ELAS'            ,'CJS'             ,'HUJEUX'          ,&
                  'CAM_CLAY'        ,'BARCELONE'       ,'LAIGLE'          ,&
                  'LETK'            ,'VISC_DRUC_PRAG'  ,'HOEK_BROWN_EFF'  ,&
                  'HOEK_BROWN_TOT'  ,'MAZARS'          ,'ENDO_ISOT_BETON' ,&
                  'ELAS_GONF'       ,'DRUCK_PRAGER'    ,'DRUCK_PRAG_N_A'  ,&
                  'JOINT_BANDIS'    ,'CZM_LIN_REG'     ,'CZM_EXP_REG'     ,&
                  'MOHR_COULOMB'/
!
! --------------------------------------------------------------------------------------------------
!
    rela_thmc = 'VIDE'
    rela_hydr = 'VIDE'
    rela_meca = 'VIDE'
    rela_ther = 'VIDE'
    l_thmc = .false.
    l_hydr = .false.
    l_meca = .false.
!
! - Read command file
!
    call getvtx(keywordfact, 'RELATION_KIT', iocc = iocc, nbval = 0,&
                nbret = nocc)
    nocc = -nocc
    call getvtx(keywordfact, 'RELATION_KIT', iocc = iocc, nbval = nocc,&
                vect = rela_kit)
!
! - Get relations for kit
!
    do ikit = 1, nocc
!
! ----- Coupling
!
        do ii = 1, dmthmc
            if (rela_kit(ikit) .eq. pothmc(ii)) then
                rela_thmc = rela_kit(ikit)
                if (l_thmc) then
                    call utmess('F', 'THM1_36')
                endif
                l_thmc = .true.
                goto 10
            endif
        enddo
!
! ----- Hydraulic
!
        do ii = 1, dmhydr
            if (rela_kit(ikit) .eq. pohydr(ii)) then
                rela_hydr = rela_kit(ikit)
                if (l_hydr) then
                    call utmess('F', 'THM1_37')
                endif
                l_hydr = .true.
                goto 10
            endif
        enddo
!
! ----- Mechanic
!
        do ii = 1, dmmeca
            if (rela_kit(ikit) .eq. pomeca(ii)) then
                rela_meca = rela_kit(ikit)
                if (l_meca) then
                    call utmess('F', 'THM1_38')
                endif
                l_meca = .true.
                goto 10
            endif
        enddo
 10     continue
    enddo
!
! - Thermic
!
    if ((rela_comp.eq.'KIT_THH').or.(rela_comp.eq.'KIT_THV').or.&
        (rela_comp.eq.'KIT_THM').or.(rela_comp.eq.'KIT_THHM')) then
        rela_ther = 'THER'
    endif
!
! - Always coupling and hydraulic relation
!
    if (.not.l_thmc) then
        call utmess('F', 'THM1_39', 1, rela_comp)
    endif
    if (.not.l_hydr) then
        call utmess('F', 'THM1_40', 1, rela_comp)
    endif
!
! - Check if all relation was determined depending on kit
!
    if (rela_comp .eq. 'KIT_HM') then
!
! ----- KIT_HM
!
        if (.not.l_meca) call utmess('F', 'THM1_41', 1, rela_comp)
        if (rela_thmc .ne. 'LIQU_SATU' .and. rela_thmc .ne. 'GAZ' .and. &
            rela_thmc .ne. 'LIQU_GAZ_ATM') then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
        if (rela_hydr .eq. 'HYDR_ENDO' .and.&
            (rela_meca.ne.'MAZARS' .and. rela_meca.ne.'ENDO_ISOT_BETON' )) then
            valk(1) = rela_hydr
            valk(2) = rela_meca
            call utmess('F', 'THM1_43', nk=2, valk=valk)
        endif
        if (rela_meca .eq. 'BARCELONE') then
            valk(1) = rela_meca
            valk(2) = rela_comp
            call utmess('F', 'THM1_44', nk=2, valk=valk)
        endif
!
    else if (rela_comp.eq.'KIT_HHM') then
!
! ----- KIT_HHM
!
        if (.not.l_meca) call utmess('F', 'THM1_41', 1, rela_comp)
        if (rela_thmc .ne. 'LIQU_GAZ' .and. rela_thmc .ne. 'LIQU_VAPE_GAZ' .and. &
            rela_thmc .ne. 'LIQU_AD_GAZ_VAPE' .and. rela_thmc .ne. 'LIQU_AD_GAZ') then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
        if (rela_hydr .eq. 'HYDR_ENDO' .and.&
            (rela_meca.ne.'MAZARS' .and. rela_meca.ne.'ENDO_ISOT_BETON' )) then
            valk(1) = rela_hydr
            valk(2) = rela_meca
            call utmess('F', 'THM1_43', nk=2, valk=valk)
        endif
        if (rela_meca .eq. 'BARCELONE' .and. &
            (rela_thmc.ne.'LIQU_GAZ' .and. rela_thmc.ne.'LIQU_VAPE_GAZ')) then
            valk(1) = rela_meca
            valk(2) = rela_comp
            call utmess('F', 'THM1_44', nk=2, valk=valk)
        endif
!
    else if (rela_comp.eq.'KIT_H') then
!
! ----- KIT_H
!
        if (l_meca) then
            valk(1) = rela_comp
            call utmess('F', 'THM1_46', 1, valk)
        endif
        if (rela_thmc .ne. 'LIQU_SATU' .and. rela_thmc .ne. 'GAZ') then
            valk(1) = rela_comp
            call utmess('F', 'THM1_59', 1, valk)
        endif
!
    else if (rela_comp.eq.'KIT_THH') then
!
! ----- KIT_THH
!
        if (l_meca) then
            valk(1) = rela_comp
            call utmess('F', 'THM1_46', 1, valk)
        endif
        if (rela_thmc .ne. 'LIQU_GAZ' .and. rela_thmc .ne. 'LIQU_VAPE_GAZ' .and. &
            rela_thmc .ne.'LIQU_AD_GAZ_VAPE' .and. rela_thmc .ne. 'LIQU_AD_GAZ') then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
!
    else if (rela_comp.eq.'KIT_HH') then
!
! ----- KIT_HH
!
        if (l_meca) then
            valk(1) = rela_comp
            call utmess('F', 'THM1_46', 1, valk)
        endif
        if (rela_thmc .ne. 'LIQU_GAZ' .and. rela_thmc .ne. 'LIQU_VAPE_GAZ' .and. &
            rela_thmc .ne. 'LIQU_AD_GAZ_VAPE' .and. rela_thmc .ne. 'LIQU_AD_GAZ') then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
!
    else if (rela_comp.eq.'KIT_THV') then
!
! ----- KIT_THV
!
        if (l_meca) then
            valk(1) = rela_comp
            call utmess('F', 'THM1_46', 1, valk)
        endif
        if (rela_thmc .ne. 'LIQU_VAPE') then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
!
    else if (rela_comp.eq.'KIT_THM') then
!
! ----- KIT_THM
!
        if (.not.l_meca) call utmess('F', 'THM1_41', 1, rela_comp)
        if (rela_thmc .ne. 'LIQU_SATU' .and. rela_thmc .ne. 'LIQU_GAZ_ATM' .and. &
            rela_thmc .ne. 'GAZ') then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
        if (rela_hydr .eq. 'HYDR_ENDO' .and.&
            (rela_meca.ne.'MAZARS' .and. rela_meca.ne.'ENDO_ISOT_BETON' )) then
            valk(1) = rela_hydr
            valk(2) = rela_meca
            call utmess('F', 'THM1_43', nk=2, valk=valk)
        endif
        if (rela_meca .eq. 'BARCELONE') then
            valk(1) = rela_meca
            valk(2) = rela_comp
            call utmess('F', 'THM1_44', nk=2, valk=valk)
        endif
!
    else if (rela_comp.eq.'KIT_THHM') then
!
! ----- KIT_THHM
!
        if (.not.l_meca) call utmess('F', 'THM1_41', 1, rela_comp)
        if (rela_thmc .ne. 'LIQU_VAPE_GAZ' .and. rela_thmc .ne. 'LIQU_AD_GAZ_VAPE' .and. &
            rela_thmc .ne. 'LIQU_AD_GAZ' .and. rela_thmc .ne. 'LIQU_GAZ') then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
        if (rela_hydr .eq. 'HYDR_ENDO' .and.&
           (rela_meca.ne.'MAZARS' .and. rela_meca.ne.'ENDO_ISOT_BETON' )) then
            valk(1) = rela_hydr
            valk(2) = rela_meca
            call utmess('F', 'THM1_43', nk=2, valk=valk)
        endif
        if (rela_meca .eq. 'BARCELONE' .and. &
           (rela_thmc.ne.'LIQU_GAZ' .and. rela_thmc.ne.'LIQU_VAPE_GAZ')) then
            valk(1) = rela_thmc
            valk(2) = rela_comp
            call utmess('F', 'THM1_42', nk=2, valk=valk)
        endif
    endif
!
end subroutine
