subroutine comp_meca_vari(rela_comp   , defo_comp   , type_cpla   , nb_vari     , kit_comp    , &
                          mult_comp   , nb_vari_exte, nb_vari_comp, nume_comp   )
!
    implicit none
!
#include "jeveux.h"
#include "asterc/lcinfo.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_code.h"
#include "asterfort/comp_meca_exc1.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/cg_kit_nvar.h"
#include "asterfort/ddi_kit_nvar.h"
#include "asterfort/thm_kit_nvar.h"
#include "asterfort/jeveuo.h"
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
    character(len=16), intent(in) :: rela_comp
    character(len=16), intent(in) :: defo_comp
    character(len=16), intent(in) :: type_cpla
    integer, intent(out) :: nb_vari
    character(len=16), optional, intent(in) :: kit_comp(9)
    character(len=16), optional, intent(in) :: mult_comp
    integer, optional, intent(out) :: nb_vari_comp(9)
    integer, optional, intent(in) :: nb_vari_exte
    integer, optional, intent(out) :: nume_comp
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Count internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp    : RELATION comportment
! In  defo_comp    : DEFORMATION comportment
! In  type_cpla    : plane stress method
! Out nb_vari      : number of internal variables
! In  kit_comp     : KIT comportment
! In  mult_comp    : multi-comportment
! In  nb_vari_exte : number of internal variable if external computing for comportment
! Out nb_vari_comp : number of internal variables kit comportment
! Out nume_comp    : number LCxxxx subroutine
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: comp_code_py, rela_code_py, meta_code_py
    character(len=16) :: rela_thmc, rela_hydr, rela_meca, rela_ther, rela_meta
    character(len=16) :: zit_comp(9)
    integer :: idummy, icomp
    integer :: nb_vari_thmc, nb_vari_hydr, nb_vari_meca, nb_vari_ther
    integer :: nb_vari_rela, nb_vari_meta
    character(len=16) :: rela_flua, rela_plas, rela_cpla, rela_coup
    integer :: nb_vari_flua, nb_vari_plas, nb_vari_cpla, nb_vari_coup
    logical :: l_cristal, l_kit_meta, l_kit_thm, l_exte_comp, l_kit_ddi, l_kit_cg
    integer :: nb_vari_cg(2)
    character(len=16) :: rela_cg(2)
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations
!
    do icomp = 1,9
        zit_comp(icomp) = 'VIDE'
    enddo
    if (present(kit_comp)) then
        do icomp = 1,9
            zit_comp(icomp) = kit_comp(icomp)
            nb_vari_comp(icomp) = 0
        enddo
    endif
!
! - Detection of specific cases
!
    call comp_meca_l(rela_comp, 'CRISTAL'  , l_cristal)
    call comp_meca_l(rela_comp, 'KIT_META' , l_kit_meta)
    call comp_meca_l(rela_comp, 'KIT_THM'  , l_kit_thm)
    call comp_meca_l(rela_comp, 'KIT_DDI'  , l_kit_ddi)
    call comp_meca_l(rela_comp, 'KIT_CG'   , l_kit_cg)
    call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp) 
!
! - Coding composite comportment (Python)
!
    call comp_meca_code(rela_comp   , defo_comp  , type_cpla , zit_comp, comp_code_py, &
                        rela_code_py, meta_code_py)
!
! - Coding composite comportment (Python)
!
    call lcinfo(rela_code_py, idummy, nb_vari_rela)
!
! - Number of internal variables
!
    if (present(nume_comp)) then      
        call lcinfo(comp_code_py, nume_comp, nb_vari)
    else
        call lcinfo(comp_code_py, idummy, nb_vari)
    endif
!
! - KIT THM
!
    if (l_kit_thm) then
        rela_thmc = kit_comp(1)
        rela_ther = kit_comp(2)
        rela_hydr = kit_comp(3)
        rela_meca = kit_comp(4)
        call thm_kit_nvar(rela_thmc, rela_hydr, rela_meca, rela_ther, nb_vari_thmc, &
                          nb_vari_hydr, nb_vari_meca, nb_vari_ther)
        nb_vari_comp(1) = nb_vari_thmc
        nb_vari_comp(2) = nb_vari_ther
        nb_vari_comp(3) = nb_vari_hydr
        nb_vari_comp(4) = nb_vari_meca
    endif
!
! - KIT META
!
    if (l_kit_meta) then
        rela_meta = kit_comp(1)
        call lcinfo(meta_code_py, idummy, nb_vari_meta)
        nb_vari = nb_vari_rela*(nb_vari_meta+1) + 1
        nb_vari_comp(1) = nb_vari_rela
        nb_vari_comp(2) = nb_vari_meta
    endif
!
! - KIT DDI
!
    if (l_kit_ddi) then
        rela_flua = kit_comp(1)
        rela_plas = kit_comp(2)
        rela_cpla = kit_comp(3)
        rela_coup = kit_comp(4)
        call ddi_kit_nvar(rela_flua, rela_plas, rela_cpla, rela_coup, nb_vari_flua, &
                          nb_vari_plas, nb_vari_cpla, nb_vari_coup)
        nb_vari_comp(1) = nb_vari_flua
        nb_vari_comp(2) = nb_vari_plas
        nb_vari_comp(3) = nb_vari_cpla
        nb_vari_comp(4) = nb_vari_coup
    endif
!
! - KIT CG
!
    if (l_kit_cg) then
        rela_cg(1) = kit_comp(1)
        rela_cg(2) = kit_comp(2)
        call cg_kit_nvar(rela_cg, nb_vari_cg)
        nb_vari_comp(1) = nb_vari_cg(1)
        nb_vari_comp(2) = nb_vari_cg(2)
    endif
!
! - Exception for number of internal variables
!
    if (l_kit_meta .or. l_cristal .or. l_exte_comp) then
        call comp_meca_exc1(defo_comp  , mult_comp  , nb_vari_exte, l_kit_meta, l_cristal , &
                            l_exte_comp, nb_vari)
    endif
!
end subroutine
