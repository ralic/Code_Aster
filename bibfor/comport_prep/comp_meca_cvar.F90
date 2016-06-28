subroutine comp_meca_cvar(p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_meca_vari.h"
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
    character(len=16), intent(in) :: p_info_comp_valk(:)
    integer, intent(in) :: p_info_comp_vali(:)
    integer, intent(out) :: p_info_comp_nvar(:)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Count all internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  p_info_comp_valk : comportment informations (character)
! In  p_info_comp_vali : comportment informations (integer)
! Out p_info_comp_nvar : comportment informations (int. vari. count)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iocc, nbocc
    character(len=16) :: keywordfact
    character(len=16) :: type_matg, post_iter
    character(len=16) :: rela_comp, defo_comp, mult_comp, kit_comp(4), type_cpla
    integer :: nume_comp(4), nb_vari, nb_vari_comp(4), nb_vari_exte
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
!
! - Loop on occurrences of COMPORTEMENT
!
    do iocc = 1, nbocc
!
! ----- Init
!
        nb_vari           = 0
        nume_comp(1:4)    = 0
        nb_vari_comp(1:4) = 0
!
! ----- Options
!
        nb_vari_exte = p_info_comp_vali(1*(iocc-1) + 1)
        rela_comp    = p_info_comp_valk(16*(iocc-1) + 1)
        defo_comp    = p_info_comp_valk(16*(iocc-1) + 2)
        type_cpla    = p_info_comp_valk(16*(iocc-1) + 4)
        kit_comp(1)  = p_info_comp_valk(16*(iocc-1) + 5)
        kit_comp(2)  = p_info_comp_valk(16*(iocc-1) + 6)
        kit_comp(3)  = p_info_comp_valk(16*(iocc-1) + 7)
        kit_comp(4)  = p_info_comp_valk(16*(iocc-1) + 8)
        mult_comp    = p_info_comp_valk(16*(iocc-1) + 14)
        type_matg    = p_info_comp_valk(16*(iocc-1) + 15)
        post_iter    = p_info_comp_valk(16*(iocc-1) + 16)
!
! ----- Count internal variables
!
        call comp_meca_vari(rela_comp, defo_comp, type_cpla, nb_vari     , kit_comp    ,&
                            type_matg, post_iter, mult_comp, nb_vari_exte, nb_vari_comp,&
                            nume_comp)
!
! ----- Save informations
!
        p_info_comp_nvar(10*(iocc-1) + 1) = nb_vari
        p_info_comp_nvar(10*(iocc-1) + 2) = nb_vari_comp(1)
        p_info_comp_nvar(10*(iocc-1) + 3) = nb_vari_comp(2)
        p_info_comp_nvar(10*(iocc-1) + 4) = nb_vari_comp(3)
        p_info_comp_nvar(10*(iocc-1) + 5) = nb_vari_comp(4)
        p_info_comp_nvar(10*(iocc-1) + 6) = nume_comp(1)
        p_info_comp_nvar(10*(iocc-1) + 7) = nume_comp(2)
        p_info_comp_nvar(10*(iocc-1) + 8) = nume_comp(3)
        p_info_comp_nvar(10*(iocc-1) + 9) = nume_comp(4)
    end do
!
end subroutine
