subroutine comp_meca_code(rela_comp_, defo_comp_  , type_cpla_   , kit_comp_   , type_matg_,&
                          post_iter_, comp_code_py, rela_code_py_, meta_code_py_)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/lccree.h"
#include "asterfort/comp_meca_l.h"
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
    character(len=16), optional, intent(in) :: rela_comp_
    character(len=16), optional, intent(in) :: defo_comp_
    character(len=16), optional, intent(in) :: type_cpla_
    character(len=16), optional, intent(in) :: kit_comp_(4)
    character(len=16), optional, intent(in) :: type_matg_
    character(len=16), optional, intent(in) :: post_iter_
    character(len=16), intent(out) :: comp_code_py
    character(len=16), optional, intent(out) :: rela_code_py_
    character(len=16), optional, intent(out) :: meta_code_py_
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Coding composite comportment
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp        : RELATION comportment
! In  defo_comp        : DEFORMATION comportment
! In  type_cpla        : plane stress method
! In  kit_comp         : KIT comportment
! In  type_matg        : type of tangent matrix
! In  post_iter        : type of post_treatment
! Out comp_code_py     : composite coded comportment (coding in Python)
! Out rela_code_py     : coded comportment for RELATION (coding in Python)
! Out meta_code_py     : coded comportment for metallurgy (coding in Python)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_comp_elem, ikit
    character(len=16) :: rela_thmc, rela_hydr, rela_meca, rela_ther
    character(len=16) :: comp_elem(20), rela_meta, meta_code_py, rela_code_py
    aster_logical :: l_kit_meta, l_kit_thm
    character(len=16) :: type_matg, post_iter
    character(len=16) :: rela_comp, defo_comp, kit_comp(4), type_cpla
!
! --------------------------------------------------------------------------------------------------
!
    rela_comp = 'VIDE'
    if (present(rela_comp_)) then
        rela_comp = rela_comp_
    endif
    defo_comp = 'VIDE'
    if (present(defo_comp_)) then
        defo_comp = defo_comp_
    endif
    type_cpla = 'VIDE'
    if (present(type_cpla_)) then
        type_cpla = type_cpla_
    endif
    kit_comp(1:4) = 'VIDE'
    if (present(kit_comp_)) then
        kit_comp(:) = kit_comp_(:)
    endif
    type_matg = 'VIDE'
    if (present(type_matg_)) then
        type_matg = type_matg_
    endif
    post_iter = 'VIDE'
    if (present(post_iter_)) then
        post_iter = post_iter_
    endif
!
    nb_comp_elem    = 0
    comp_elem(1:20) = 'VIDE'
    call comp_meca_l(rela_comp, 'KIT_META', l_kit_meta)
    call comp_meca_l(rela_comp, 'KIT_THM' , l_kit_thm)
!
! - Create composite comportment
!
    nb_comp_elem = nb_comp_elem + 1
    comp_elem(nb_comp_elem) = rela_comp
    nb_comp_elem = nb_comp_elem + 1
    comp_elem(nb_comp_elem) = defo_comp
    nb_comp_elem = nb_comp_elem + 1
    comp_elem(nb_comp_elem) = type_cpla
    do ikit = 1, 4
        nb_comp_elem = nb_comp_elem + 1
        comp_elem(nb_comp_elem) = kit_comp(ikit)
    enddo
    if (type_matg.ne.' ') then
        nb_comp_elem = nb_comp_elem + 1
        comp_elem(nb_comp_elem) = type_matg
    endif
    if (post_iter.ne.' ') then
        nb_comp_elem = nb_comp_elem + 1
        comp_elem(nb_comp_elem) = post_iter
    endif
!
! - Reorder THM behaviours
!
    if (l_kit_thm) then
        rela_thmc = comp_elem(4)
        rela_ther = comp_elem(5)
        rela_hydr = comp_elem(6)
        rela_meca = comp_elem(7)
        comp_elem(4) = rela_meca
        comp_elem(5) = rela_hydr
        comp_elem(6) = rela_ther
        comp_elem(7) = rela_thmc
    endif
!
! - Coding metallurgy comportment
!
    meta_code_py = ' '
    if (l_kit_meta) then
        rela_meta = kit_comp(1)
        call lccree(1, rela_meta, meta_code_py)
    endif
!
! - Coding only RELATION (Python)
!
    call lccree(1, rela_comp, rela_code_py)
!
! - Coding composite comportment (Python)
!
    call lccree(nb_comp_elem, comp_elem, comp_code_py)
!
    if (present(meta_code_py_)) then
        meta_code_py_ = meta_code_py
    endif
    if (present(rela_code_py_)) then
        rela_code_py_ = rela_code_py
    endif
!
end subroutine
