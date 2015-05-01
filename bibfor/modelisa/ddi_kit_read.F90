subroutine ddi_kit_read(keywordfact, iocc, rela_flua, rela_plas, rela_cpla,&
                        rela_coup)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
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
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=16), intent(out) :: rela_flua
    character(len=16), intent(out) :: rela_plas
    character(len=16), intent(out) :: rela_cpla
    character(len=16), intent(out) :: rela_coup
!
! --------------------------------------------------------------------------------------------------
!
! KIT_DDI
!
! Read informations for KIT
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact  : factor keyword to read (COMPORTEMENT)
! In  iocc         : factor keyword index in COMPORTEMENT
! Out rela_flua    : comportment relation for fluage
! Out rela_plas    : comportment relation for plasticity
! Out rela_cpla    : comportment relation for plane stress (GLRC)
! Out rela_coup    : comportment relation for coupling (GLRC)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: dmflua, dmplas
    parameter  ( dmflua = 7, dmplas = 10)
    character(len=16) :: poflua(dmflua), poplas(dmplas)
    integer :: ikit, ii, nocc
    character(len=16) :: rela_kit(2)
!
! --------------------------------------------------------------------------------------------------
!
    data poflua / 'GRANGER_FP'      ,'GRANGER_FP_V'    ,'GRANGER_FP_INDT' ,        &
                  'BETON_UMLV_FP'   ,'GLRC_DM'         ,'GLRC_DAMAGE'     ,'FLUA_PORO_BETON'/
    data poplas / 'ELAS'            ,'VMIS_ISOT_TRAC'  ,'VMIS_ISOT_PUIS'  ,        &
                  'VMIS_ISOT_LINE'  ,'VMIS_CINE_LINE'  ,'ROUSS_PR'        ,        &
                  'BETON_DOUBLE_DP' ,'ENDO_ISOT_BETON' ,'MAZARS'          ,'ENDO_PORO_BETON'/
!
! --------------------------------------------------------------------------------------------------
!
    rela_flua = 'VIDE'
    rela_plas = 'VIDE'
    rela_cpla = 'VIDE'
    rela_coup = 'VIDE'
!
! - Read command file
!
    call getvtx(keywordfact, 'RELATION_KIT', iocc = iocc, nbval = 0, nbret = nocc)
    nocc = -nocc
    ASSERT(nocc.le.2)
    call getvtx(keywordfact, 'RELATION_KIT', iocc = iocc, nbval = nocc, vect = rela_kit)
!
! - Get relations for kit
!
    do ikit = 1, nocc
!
! ----- Fluage
!
        do ii = 1, dmflua
            if (rela_kit(ikit) .eq. poflua(ii)) then
                rela_flua = rela_kit(ikit)
                goto 10
            endif
        enddo
!
! ----- Elasto-plastic
!
        do ii = 1, dmplas
            if (rela_kit(ikit) .eq. poplas(ii)) then
                rela_plas = rela_kit(ikit)
                goto 10
            endif
        enddo
 10     continue
    enddo
!
! - Compatibility
!
    if (rela_flua(1:10) .eq. 'GRANGER_FP') then
        if (rela_plas .ne. 'ELAS' .and. rela_plas .ne. 'VMIS_ISOT_TRAC' .and. rela_plas&
            .ne. 'VMIS_ISOT_PUIS' .and. rela_plas .ne. 'VMIS_ISOT_LINE' .and. rela_plas&
            .ne. 'ROUSS_PR' .and. rela_plas .ne. 'BETON_DOUBLE_DP') then
            call utmess('F', 'COMPOR3_2', sk=rela_plas)
        endif
    else if (rela_flua.eq.'BETON_UMLV_FP') then
        if (rela_plas .ne. 'ENDO_ISOT_BETON' .and. rela_plas .ne. 'MAZARS') then
            call utmess('F', 'COMPOR3_3', sk=rela_plas)
        endif
    else if (rela_flua(1:4).eq.'GLRC') then
        if (rela_plas .ne. 'VMIS_ISOT_TRAC' .and. rela_plas .ne. 'VMIS_ISOT_LINE' .and.&
            rela_plas .ne. 'VMIS_CINE_LINE') then
            call utmess('F', 'COMPOR3_4', sk=rela_plas)
        endif
    else if (rela_flua.eq.'FLUA_PORO_BETON') then
        if (rela_plas .ne. 'ENDO_PORO_BETON') then
            call utmess('F', 'COMPOR3_3', sk=rela_plas)
        endif
    else
        call utmess('F', 'COMPOR3_6', sk=rela_flua)
    endif
!
! - For GLRC: internal Deborst Algorithm and special internal variables
!
    if (rela_flua(1:4) .eq. 'GLRC') then
        rela_coup = 'DDI_PLAS_ENDO'
        rela_cpla = 'DEBORST'
    endif
!
! - Alarm
!
    if (rela_flua .eq. 'BETON_UMLV_FP') then
        if (rela_plas .eq. 'ENDO_ISOT_BETON' .or. rela_plas .eq. 'MAZARS') then
            call utmess('A', 'COMPOR3_83')
        endif
    endif
!
end subroutine
