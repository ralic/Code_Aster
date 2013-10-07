subroutine comp_comp_read(list_vale)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: list_vale
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE>
!
! Read informations in AFFE_MATERIAU/AFFE_COMPOR
!
! --------------------------------------------------------------------------------------------------
!
! In  list_vale   : list of informations to save
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: iocc, nocc, nbgmax, i, jdecal, idummy
    character(len=8) :: sdcomp, k8dummy
    integer :: j_cprci, j_cprck
    integer :: j_lvali, j_lvalk
    character(len=16) :: comp_code
    character(len=16) :: rela_comp, defo_comp, type_comp, type_cpla, mult_comp
    integer :: nb_vari, nume_comp, nb_vari_exte, unit_comp
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact = 'AFFE_COMPOR'
    call getfac(keywordfact, nocc)
!
! - List contruction
!
    call wkvect(list_vale(1:19)//'.VALI', 'V V I'  , 4*nocc, j_lvali)
    call wkvect(list_vale(1:19)//'.VALK', 'V V K24', 16*nocc, j_lvalk)
!
    do iocc = 1, nocc
!
        nb_vari_exte = 0
        unit_comp    = 0
!
! ----- Get SD COMPOR from DEFI_COMPOR
!
        call getvid(keywordfact, 'COMPOR', iocc = iocc , scal = sdcomp)
        call jeveuo(sdcomp//'.CPRI', 'L', j_cprci)
        call jeveuo(sdcomp//'.CPRK', 'L', j_cprck)
        ASSERT(zi(j_cprci).eq.3)
!
! ----- First none-void COMPOR in fiber
!
        call jelira(sdcomp//'.CPRK', 'LONMAX', nbgmax, k8dummy)
        nbgmax = (nbgmax-1)/6
        do i = 1, nbgmax
            jdecal = 6*(i-1)-1
            if (zk24(j_cprck+jdecal+2) .ne. 'VIDE') goto 25
        enddo
        call utmess('F', 'COMPOR1_85')
25      continue
!
! ----- Save options in list
!
        rela_comp = zk24(j_cprck+jdecal+3)(1:16)
        defo_comp = zk24(j_cprck+jdecal+5)(1:16)
        type_comp = 'COMP_INCR'
        type_cpla = zk24(j_cprck+jdecal+4)(1:16)
        mult_comp = sdcomp//'.CPRK'
        nb_vari   = zi(j_cprci+1)
        call lccree(1, rela_comp, comp_code)
        call lcinfo(comp_code, nume_comp, idummy)
!
! ----- Save options in list
!
        zi(j_lvali+4*(iocc-1) -1 + 1) = nb_vari_exte
        zi(j_lvali+4*(iocc-1) -1 + 2) = unit_comp
        zi(j_lvali+4*(iocc-1) -1 + 3) = nb_vari
        zi(j_lvali+4*(iocc-1) -1 + 4) = nume_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 1)  = rela_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 2)  = defo_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 3)  = type_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 4)  = type_cpla
        zk24(j_lvalk+16*(iocc-1) -1 + 14) = mult_comp
!
    end do
!
end subroutine

