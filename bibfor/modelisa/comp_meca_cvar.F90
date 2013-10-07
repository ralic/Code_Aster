subroutine comp_meca_cvar(list_vale)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_meca_vari.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
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
    character(len=19), intent(in) :: list_vale
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Count all internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  list_vale   : list of informations
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_vari_exte
    integer :: iocc, nbocc, icomp
    integer :: j_lvali, j_lvalk, j_lnvar
    character(len=16) :: keywordfact
    character(len=16) :: type_matg, post_iter
    character(len=16) :: rela_comp, defo_comp, mult_comp, kit_comp(9), type_cpla
    logical :: l_matr_tgsc, l_crit_rupt
    integer :: nume_comp, nb_vari
    integer :: nb_vari_comp(9)
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
!
! - Access to list
!
    if (nbocc.ne.0) then
        call wkvect(list_vale(1:19)//'.NVAR', 'V V I', 10*nbocc, j_lnvar)
        call jeveuo(list_vale(1:19)//'.VALI', 'L', j_lvali)
        call jeveuo(list_vale(1:19)//'.VALK', 'L', j_lvalk)
    endif
!
! - Loop on occurrences of COMPORTEMENT
!
    do iocc = 1, nbocc
!
! ----- Init
!
        nb_vari      = 0
        nume_comp    = 0 
        do icomp = 1,9
            nb_vari_comp(icomp) = 0
        enddo 
!
! ----- Options
!
        nb_vari_exte = zi(j_lvali+2*(iocc-1) -1 + 1)
        rela_comp   = zk24(j_lvalk+16*(iocc-1) -1 + 1)(1:16)
        defo_comp   = zk24(j_lvalk+16*(iocc-1) -1 + 2)(1:16)
        type_cpla   = zk24(j_lvalk+16*(iocc-1) -1 + 4)(1:16)
        kit_comp(1) = zk24(j_lvalk+16*(iocc-1) -1 + 5)(1:16)
        kit_comp(2) = zk24(j_lvalk+16*(iocc-1) -1 + 6)(1:16) 
        kit_comp(3) = zk24(j_lvalk+16*(iocc-1) -1 + 7)(1:16)
        kit_comp(4) = zk24(j_lvalk+16*(iocc-1) -1 + 8)(1:16)
        kit_comp(5) = zk24(j_lvalk+16*(iocc-1) -1 + 9)(1:16)
        kit_comp(6) = zk24(j_lvalk+16*(iocc-1) -1 + 10)(1:16)
        kit_comp(7) = zk24(j_lvalk+16*(iocc-1) -1 + 11)(1:16)
        kit_comp(8) = zk24(j_lvalk+16*(iocc-1) -1 + 12)(1:16)
        kit_comp(9) = zk24(j_lvalk+16*(iocc-1) -1 + 13)(1:16)
        mult_comp   = zk24(j_lvalk+16*(iocc-1) -1 + 14)(1:16)
        type_matg   = zk24(j_lvalk+16*(iocc-1) -1 + 15)(1:16)
        post_iter   = zk24(j_lvalk+16*(iocc-1) -1 + 16)(1:16)
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'MATR_TGSC', l_matr_tgsc, type_matg = type_matg)
        call comp_meca_l(rela_comp, 'CRIT_RUPT', l_crit_rupt, post_iter = post_iter)
        if (l_matr_tgsc) kit_comp(6) = type_matg
        if (l_crit_rupt) kit_comp(7) = post_iter
!
! ----- Count internal variables
!
        call comp_meca_vari(rela_comp   , defo_comp   , type_cpla   , nb_vari     , kit_comp    , &
                            mult_comp   , nb_vari_exte, nb_vari_comp, nume_comp   )
!
! ----- Save informations
!
        zi(j_lnvar+10*(iocc-1) -1 + 1) = nume_comp
        zi(j_lnvar+10*(iocc-1) -1 + 2) = nb_vari
        zi(j_lnvar+10*(iocc-1) -1 + 3) = nb_vari_comp(1)
        zi(j_lnvar+10*(iocc-1) -1 + 4) = nb_vari_comp(2)
        zi(j_lnvar+10*(iocc-1) -1 + 5) = nb_vari_comp(3)
        zi(j_lnvar+10*(iocc-1) -1 + 6) = nb_vari_comp(4)
        zi(j_lnvar+10*(iocc-1) -1 + 7) = nb_vari_comp(2)
        zi(j_lnvar+10*(iocc-1) -1 + 8) = nb_vari_comp(1)
    end do
!
end subroutine
