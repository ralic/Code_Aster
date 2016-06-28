subroutine comp_meca_vari(rela_comp , defo_comp , type_cpla , nb_vari      , kit_comp_    ,&
                          type_matg_, post_iter_, mult_comp_, nb_vari_exte_, nb_vari_comp_,&
                          nume_comp_)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/lcinfo.h"
#include "asterc/lcdiscard.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_code.h"
#include "asterfort/comp_meca_exc1.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/cg_kit_nvar.h"
#include "asterfort/ddi_kit_nvar.h"
#include "asterfort/thm_kit_nvar.h"
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
    character(len=16), intent(in) :: rela_comp
    character(len=16), intent(in) :: defo_comp
    character(len=16), intent(in) :: type_cpla
    integer, intent(out) :: nb_vari
    character(len=16), optional, intent(in) :: kit_comp_(4)
    character(len=16), optional, intent(in) :: type_matg_
    character(len=16), optional, intent(in) :: post_iter_
    character(len=16), optional, intent(in) :: mult_comp_
    integer, optional, intent(out) :: nb_vari_comp_(4)
    integer, optional, intent(in) :: nb_vari_exte_
    integer, optional, intent(out) :: nume_comp_(4)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Count internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp        : RELATION comportment
! In  defo_comp        : DEFORMATION comportment
! In  type_cpla        : plane stress method
! Out nb_vari          : number of internal variables
! In  kit_comp         : KIT comportment
! In  type_matg        : type of tangent matrix
! In  post_iter        : type of post_treatment
! In  mult_comp        : multi-comportment
! In  nb_vari_exte     : number of internal variable if external computing for comportment
! Out nb_vari_comp     : number of internal variables kit comportment
! Out nume_comp        : number LCxxxx subroutine
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: comp_elem_py, rela_comp_py, rela_meta_py
    character(len=16) :: rela_thmc, rela_hydr, rela_meca, rela_ther, rela_meta
    integer :: idummy
    integer :: nb_vari_thmc, nb_vari_hydr, nb_vari_meca, nb_vari_ther
    integer :: nb_vari_rela, nb_vari_meta
    character(len=16) :: rela_flua, rela_plas, rela_cpla, rela_coup
    integer :: nb_vari_flua, nb_vari_plas, nb_vari_cpla, nb_vari_coup
    aster_logical :: l_cristal, l_kit_meta, l_kit_thm, l_exte_comp, l_kit_ddi, l_kit_cg
    integer :: nb_vari_cg(2), nume_comp_plas
    character(len=16) :: rela_comp_cg(2)
    character(len=16) :: kit_comp(4), type_matg, post_iter, mult_comp
    integer :: nb_vari_exte, nume_comp(4)=0, nb_vari_comp(4)=0
!
! --------------------------------------------------------------------------------------------------
!
    call comp_meca_l(rela_comp, 'CRISTAL'  , l_cristal)
    call comp_meca_l(rela_comp, 'KIT_META' , l_kit_meta)
    call comp_meca_l(rela_comp, 'KIT_THM'  , l_kit_thm)
    call comp_meca_l(rela_comp, 'KIT_DDI'  , l_kit_ddi)
    call comp_meca_l(rela_comp, 'KIT_CG'   , l_kit_cg)
    call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
    kit_comp(1:4) = 'VIDE'
    type_matg     = 'VIDE'
    post_iter     = 'VIDE'
    mult_comp     = 'VIDE'
    nb_vari_exte  = 0
    if (present(kit_comp_)) then
        kit_comp(1:4) = kit_comp_(1:4)
    endif
    if (present(type_matg_)) then
        type_matg = type_matg_
    endif
    if (present(post_iter_)) then
        post_iter = post_iter_
    endif
    if (present(mult_comp_)) then
        mult_comp = mult_comp_
    endif
    if (present(nb_vari_exte_)) then
        nb_vari_exte = nb_vari_exte_
    endif
!
! - Coding composite comportment (Python)
!
    call comp_meca_code(rela_comp, defo_comp   , type_cpla   , kit_comp    , type_matg,&
                        post_iter, comp_elem_py, rela_comp_py, rela_meta_py)
!
! - Get total number of internal variable for RELATION
!
    call lcinfo(rela_comp_py, idummy, nb_vari_rela)
!
! - Get number of internal variables for composite comportment
!
    call lcinfo(comp_elem_py, nume_comp(1), nb_vari)
!
! - Number of internal variables for KIT THM
!
    if (l_kit_thm) then
        rela_thmc = kit_comp(1)
        rela_ther = kit_comp(2)
        rela_hydr = kit_comp(3)
        rela_meca = kit_comp(4)
        call thm_kit_nvar(rela_thmc   , rela_hydr   , rela_meca   , rela_ther, nb_vari_thmc,&
                          nb_vari_hydr, nb_vari_meca, nb_vari_ther)
        nb_vari_comp(1) = nb_vari_thmc
        nb_vari_comp(2) = nb_vari_ther
        nb_vari_comp(3) = nb_vari_hydr
        if (rela_meca.eq.'MFRONT') then
            nb_vari_comp(4) = nb_vari_exte
            l_exte_comp     = .true.
        else
            nb_vari_comp(4) = nb_vari_meca
        endif
    endif
!
! - Number of internal variables for KIT META
!
    if (l_kit_meta) then
        rela_meta = kit_comp(1)
        call lcinfo(rela_meta_py, idummy, nb_vari_meta)
        nb_vari = nb_vari_rela*(nb_vari_meta+1) + 1
        nb_vari_comp(1) = nb_vari_rela
        nb_vari_comp(2) = nb_vari_meta
    endif
!
! - Number of internal variables for KIT DDI
!
    if (l_kit_ddi) then
        rela_flua = kit_comp(1)
        rela_plas = kit_comp(2)
        rela_cpla = kit_comp(3)
        rela_coup = kit_comp(4)
        call ddi_kit_nvar(rela_flua   , rela_plas   , rela_cpla   , rela_coup     , nb_vari_flua,&
                          nb_vari_plas, nb_vari_cpla, nb_vari_coup, nume_comp_plas)
        nb_vari_comp(1) = nb_vari_flua
        nb_vari_comp(2) = nb_vari_plas
        nb_vari_comp(3) = nb_vari_cpla
        nb_vari_comp(4) = nb_vari_coup
        nume_comp(2)    = nume_comp_plas
        nume_comp(3)    = nb_vari_plas
    endif
!
! - Number of internal variables for KIT CG
!
    if (l_kit_cg) then
        rela_comp_cg(1) = kit_comp(1)
        rela_comp_cg(2) = kit_comp(2)
        call cg_kit_nvar(rela_comp_cg, nb_vari_cg)
        nb_vari_comp(1) = nb_vari_cg(1)
        nb_vari_comp(2) = nb_vari_cg(2)
    endif
!
    call lcdiscard(comp_elem_py)
    call lcdiscard(rela_comp_py)
    call lcdiscard(rela_meta_py)
!
! - Exception for number of internal variables
!
    if (l_kit_meta .or. l_cristal .or. l_exte_comp) then
        call comp_meca_exc1(defo_comp  , mult_comp, nb_vari_exte, l_kit_meta, l_cristal,&
                            l_exte_comp, nb_vari)
    endif
!
    if (present(nume_comp_)) then
        nume_comp_(1:4) = nume_comp(1:4)
    endif
    if (present(nb_vari_comp_)) then
        nb_vari_comp_(1:4) = nb_vari_comp(1:4)
    endif
!
end subroutine
