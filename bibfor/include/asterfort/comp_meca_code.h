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
interface
    subroutine comp_meca_code(rela_comp_, defo_comp_  , type_cpla_   , kit_comp_   , type_matg_,&
                              post_iter_, comp_code_py, rela_code_py_, meta_code_py_)
        character(len=16), optional, intent(in) :: rela_comp_
        character(len=16), optional, intent(in) :: defo_comp_
        character(len=16), optional, intent(in) :: type_cpla_
        character(len=16), optional, intent(in) :: kit_comp_(4)
        character(len=16), optional, intent(in) :: type_matg_
        character(len=16), optional, intent(in) :: post_iter_
        character(len=16), intent(out) :: comp_code_py
        character(len=16), optional, intent(out) :: rela_code_py_
        character(len=16), optional, intent(out) :: meta_code_py_
    end subroutine comp_meca_code
end interface
