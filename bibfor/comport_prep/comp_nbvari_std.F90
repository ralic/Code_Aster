subroutine comp_nbvari_std(rela_comp , defo_comp, type_cpla     , nb_vari   ,&
                           kit_comp_ , type_matg_, post_iter_   , mult_comp_,&
                           l_cristal_, nume_comp_, nb_vari_rela_)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/lcinfo.h"
#include "asterc/lcdiscard.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_code.h"
#include "asterfort/jeveuo.h"
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
    aster_logical, optional, intent(in) :: l_cristal_
    integer, optional, intent(out) :: nb_vari_rela_
    integer, optional, intent(out) :: nume_comp_(4)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Get number of internal variables for standard constitutive laws
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp        : RELATION comportment
! In  defo_comp        : DEFORMATION comportment
! In  type_cpla        : plane stress method
! In  kit_comp         : KIT comportment
! In  type_matg        : type of tangent matrix
! In  post_iter        : type of post_treatment
! In  mult_comp        : multi-comportment
! In  l_cristal        : .true. if *CRISTAL comportment
! Out nb_vari          : number of internal variables
! Out nb_vari_rela     : number of internal variables for RELATION
! Out nume_comp        : number LCxxxx subroutine
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: kit_comp(4), type_matg, post_iter, mult_comp
    aster_logical :: l_cristal
    integer :: nb_vari_rela, nume_comp(4)
    character(len=16) :: comp_elem_py, rela_comp_py
    integer :: idummy
    character(len=8) :: sdcomp
    integer :: nb_vari_cris
    integer, pointer :: v_cpri(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    kit_comp(:) = 'VIDE'
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
    mult_comp = 'VIDE'
    if (present(mult_comp_)) then
        mult_comp = mult_comp_
    endif
    l_cristal = .false.
    if (present(l_cristal_)) then
        l_cristal = l_cristal_
    endif
    nb_vari      = 0
    nb_vari_rela = 0
    nume_comp(:) = 0
!
! - Coding composite comportment (Python)
!
    call comp_meca_code(rela_comp, defo_comp   , type_cpla   , kit_comp, type_matg,&
                        post_iter, comp_elem_py, rela_comp_py)
!
! - Get number of variables
!
    call lcinfo(rela_comp_py, idummy, nb_vari_rela)
    call lcinfo(comp_elem_py, nume_comp(1), nb_vari)
!
! - Special for CRISTAL
!
    if (l_cristal) then
        sdcomp = mult_comp(1:8)
        call jeveuo(sdcomp//'.CPRI', 'L', vi=v_cpri)
        nb_vari_cris = v_cpri(3)
        nb_vari      = nb_vari + nb_vari_cris
        if (defo_comp .eq. 'SIMO_MIEHE') then
            nb_vari = nb_vari + 3 + 9
        endif
    endif
!
! - End of encoding
!
    call lcdiscard(comp_elem_py)
    call lcdiscard(rela_comp_py)
!
! - Copy
!
    if (present(nume_comp_)) then
        nume_comp_(1:4) = nume_comp(1:4)
    endif
    if (present(nb_vari_rela_)) then
        nb_vari_rela_ = nb_vari_rela
    endif
!
end subroutine
