subroutine comp_meca_code(rela_comp, defo_comp   , type_cpla   , kit_comp    , type_matg,&
                          post_iter, comp_elem_py, rela_comp_py, rela_meta_py)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/lccree.h"
#include "asterfort/comp_meca_l.h"
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
    character(len=16), intent(in) :: rela_comp
    character(len=16), intent(in) :: defo_comp
    character(len=16), intent(in) :: type_cpla
    character(len=16), intent(in) :: kit_comp(4)
    character(len=16), intent(in) :: type_matg
    character(len=16), intent(in) :: post_iter
    character(len=16), intent(out) :: comp_elem_py
    character(len=16), intent(out) :: rela_comp_py
    character(len=16), intent(out) :: rela_meta_py
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
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
! Out comp_elem_py     : composite coded comportment (coding in Python)
! Out rela_comp_py     : coded comportment for RELATION (coding in Python)
! Out rela_meta_py     : coded comportment for metallurgy (coding in Python)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_comp_elem, ikit
    character(len=16) :: rela_thmc, rela_hydr, rela_meca, rela_ther
    character(len=16) :: comp_elem(20), rela_meta
    aster_logical :: l_kit_meta, l_kit_thm
!
! --------------------------------------------------------------------------------------------------
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
    rela_meta_py = ' '
    if (l_kit_meta) then
        rela_meta = kit_comp(1)
        call lccree(1, rela_meta, rela_meta_py)
    endif
!
! - Coding only RELATION (Python)
!
    call lccree(1, rela_comp, rela_comp_py)
!
! - Coding composite comportment (Python)
!
    call lccree(nb_comp_elem, comp_elem, comp_elem_py)
!
end subroutine
