!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine comp_meca_name(nb_vari     , l_excl      , vari_excl   , l_kit_meta,&
                              defo_comp   ,&
                              comp_code_py, rela_code_py, meta_code_py,&
                              v_vari_name)
        integer, intent(in) :: nb_vari
        aster_logical, intent(in) :: l_excl
        character(len=16), intent(in) :: vari_excl
        aster_logical, intent(in) :: l_kit_meta
        character(len=16), intent(in) :: defo_comp
        character(len=16), intent(in) :: comp_code_py
        character(len=16), intent(in) :: rela_code_py
        character(len=16), intent(in) :: meta_code_py
        character(len=16), pointer, intent(in) :: v_vari_name(:)
    end subroutine comp_meca_name
end interface
