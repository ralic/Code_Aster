subroutine comp_comp_read(v_info_valk, v_info_vali)
!
implicit none
!
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lcdiscard.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(out) :: v_info_valk(:)
    integer          , intent(out) :: v_info_vali(:)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (AFFE_MATERIAU)
!
! Read informations from command file
!
! --------------------------------------------------------------------------------------------------
!
! Out v_info_valk      : comportment informations (character)
! Out v_info_vali      : comportment informations (integer)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: iocc, nocc, nbgmax, i, jdecal, idummy
    character(len=8) :: sdcomp, k8dummy
    character(len=16) :: comp_code
    character(len=16) :: rela_comp, defo_comp, type_comp, type_cpla, mult_comp
    integer :: nb_vari, nume_comp, nb_vari_exte, unit_comp
    character(len=24), pointer :: v_sdcomp_cprk(:) => null()
    integer, pointer :: v_sdcomp_cpri(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact = 'AFFE_COMPOR'
    call getfac(keywordfact, nocc)
!
    do iocc = 1, nocc
!
        nb_vari_exte = 0
        unit_comp    = 0
!
! ----- Get SD COMPOR from DEFI_COMPOR
!
        call getvid(keywordfact, 'COMPOR', iocc = iocc , scal = sdcomp)
        call jeveuo(sdcomp//'.CPRI', 'L', vi   = v_sdcomp_cpri)
        call jeveuo(sdcomp//'.CPRK', 'L', vk24 = v_sdcomp_cprk)
        ASSERT(v_sdcomp_cpri(1) .eq. 3)
!
! ----- First none-void COMPOR in fiber
!
        call jelira(sdcomp//'.CPRK', 'LONMAX', nbgmax, k8dummy)
        nbgmax = (nbgmax-1)/6
        do i = 1, nbgmax
            jdecal = 6*(i-1)-1
            if (v_sdcomp_cprk(1+jdecal+2) .ne. 'VIDE') then
                goto 25
            endif
        enddo
        call utmess('F', 'COMPOR1_85')
25      continue
!
! ----- Save options in list
!
        rela_comp = v_sdcomp_cprk(1+jdecal+3)(1:16)
        defo_comp = v_sdcomp_cprk(1+jdecal+5)(1:16)
        type_comp = 'COMP_INCR'
        type_cpla = v_sdcomp_cprk(1+jdecal+4)(1:16)
        mult_comp = sdcomp//'.CPRK'
        nb_vari   = v_sdcomp_cpri(2)
        call lccree(1, rela_comp, comp_code)
        call lcinfo(comp_code, nume_comp, idummy)
        call lcdiscard(comp_code)
!
! ----- Save options in list
!
        v_info_vali(4*(iocc-1) + 1)  = nb_vari_exte
        v_info_vali(4*(iocc-1) + 2)  = unit_comp
        v_info_vali(4*(iocc-1) + 3)  = nb_vari
        v_info_vali(4*(iocc-1) + 4)  = nume_comp
        v_info_valk(16*(iocc-1)+ 1)  = rela_comp
        v_info_valk(16*(iocc-1)+ 2)  = defo_comp
        v_info_valk(16*(iocc-1)+ 3)  = type_comp
        v_info_valk(16*(iocc-1)+ 4)  = type_cpla
        v_info_valk(16*(iocc-1)+ 5)  = 'VIDE'
        v_info_valk(16*(iocc-1)+ 6)  = 'VIDE'
        v_info_valk(16*(iocc-1)+ 7)  = 'VIDE'
        v_info_valk(16*(iocc-1)+ 8)  = 'VIDE'
        v_info_valk(16*(iocc-1)+ 14) = mult_comp
        v_info_valk(16*(iocc-1)+ 15) = 'VIDE'
        v_info_valk(16*(iocc-1)+ 16) = 'VIDE'
!
    end do
!
end subroutine
