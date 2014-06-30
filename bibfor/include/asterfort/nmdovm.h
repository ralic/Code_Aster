!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine nmdovm(model, l_affe_all, list_elem_affe, nb_elem_affe, full_elem_s,&
                      rela_comp, rela_comp_py, type_cpla)
        character(len=8), intent(in) :: model
        character(len=24), intent(in) :: list_elem_affe
        logical(kind=1), intent(in) :: l_affe_all
        integer, intent(in) :: nb_elem_affe
        character(len=19), intent(in) :: full_elem_s
        character(len=16), intent(in) :: rela_comp
        character(len=16), intent(in) :: rela_comp_py
        character(len=16), intent(out) :: type_cpla
    end subroutine nmdovm
end interface
