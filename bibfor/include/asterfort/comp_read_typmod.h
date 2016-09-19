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
    subroutine comp_read_typmod(mesh       , v_model_elem,&
                                keywordfact, i_comp      , rela_comp ,&
                                model_dim  , model_mfront, type_cpla_)
        character(len=8), intent(in) :: mesh
        integer, intent(in), pointer :: v_model_elem(:)
        character(len=16), intent(in) :: keywordfact
        integer, intent(in) :: i_comp
        character(len=16), intent(in) :: rela_comp
        character(len=16), intent(out) :: model_mfront
        integer, intent(out) :: model_dim
        character(len=16), optional, intent(out) :: type_cpla_
    end subroutine comp_read_typmod
end interface
