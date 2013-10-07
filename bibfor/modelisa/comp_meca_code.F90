subroutine comp_meca_code(rela_comp   , defo_comp   , type_cpla , kit_comp, comp_code_py,  &
                          rela_code_py, meta_code_py)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterfort/comp_meca_l.h"
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
    character(len=16), intent(in) :: kit_comp(9)
    character(len=16), intent(out) :: comp_code_py
    character(len=16), intent(out) :: rela_code_py
    character(len=16), intent(out) :: meta_code_py
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Coding composite comportment
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp    : RELATION comportment
! In  defo_comp    : DEFORMATION comportment
! In  type_cpla    : plane stress method
! In  kit_comp     : KIT comportment
! In  l_matr_tgsc  : .true. if modified matrix
! In  l_crit_rupt  : .true. if damage post-treatment
! In  l_kit_meta   : .true. if metallurgy
! Out comp_code_py : composite coded comportment (coding in Python)
! Out rela_code_py : coded comportment for RELATION (coding in Python)
! Out meta_code_py : coded comportment for metallurgy (coding in Python)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_comp_elem, ikit, icomp
    character(len=16) :: comp_elem(20), rela_meta
    logical :: l_umat, l_mfront, l_kit_meta
!
! --------------------------------------------------------------------------------------------------
!
    nb_comp_elem = 0
    do icomp = 1, 20
       comp_elem(icomp) = 'VIDE'
    enddo
    call comp_meca_l(rela_comp, 'UMAT'     , l_umat)
    call comp_meca_l(rela_comp, 'MFRONT'   , l_mfront)
    call comp_meca_l(rela_comp, 'KIT_META' , l_kit_meta)
!
! - Create composite comportment
!
    nb_comp_elem = nb_comp_elem + 1
    comp_elem(nb_comp_elem) = rela_comp
    nb_comp_elem = nb_comp_elem + 1
    comp_elem(nb_comp_elem) = defo_comp
    nb_comp_elem = nb_comp_elem + 1
    comp_elem(nb_comp_elem) = type_cpla
!
! - No UMAT/MFRONT with KIT
!
    if (.not.(l_umat.or.l_mfront)) then
        do ikit = 1, 9
            nb_comp_elem = nb_comp_elem + 1
            comp_elem(nb_comp_elem) = kit_comp(ikit)
        enddo
    endif
!
! - Coding metallurgy comportment
!
    if (l_kit_meta) then
        rela_meta = kit_comp(1)
        call lccree(1, rela_meta, meta_code_py)
    endif  
!
! - Coding RELATION comportment
!
    call lccree(1, rela_comp, rela_code_py) 
!
! - Coding composite comportment (Python)
!
    call lccree(nb_comp_elem, comp_elem, comp_code_py)
!
end subroutine
