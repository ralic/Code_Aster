!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine diinit(mesh_      , model_, result, mate     , cara_elem,&
                      func_acti  , sddyna, parcri, inst_init, solver   ,&
                      sdcont_defi, sddisc)
        character(len=*), intent(in) :: mesh_
        character(len=*), intent(in) :: model_
        character(len=19), intent(in) :: sddisc
        character(len=19), intent(in) :: sddyna
        character(len=24), intent(in) :: cara_elem
        character(len=24), intent(in) :: mate
        real(kind=8), intent(in) :: inst_init
        real(kind=8), intent(in) :: parcri(*)
        character(len=8), intent(in) :: result
        character(len=19), intent(in) :: solver
        character(len=24), intent(in) :: sdcont_defi
        integer, intent(in) :: func_acti(*)
    end subroutine diinit
end interface
