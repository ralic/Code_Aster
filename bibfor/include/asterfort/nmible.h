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
    subroutine nmible(cont_loop     , model   , mesh  , sdcont_defi, sdcont_solv,&
                      list_func_acti, nume_dof, sdstat, sdtime     , sdimpr)
        integer, intent(inout) :: cont_loop
        character(len=24), intent(in) :: model
        character(len=8), intent(in) :: mesh
        character(len=24), intent(in) :: sdcont_defi
        character(len=24), intent(in) :: sdcont_solv
        integer, intent(in):: list_func_acti(*)
        character(len=24), intent(in) :: nume_dof
        character(len=24), intent(in) :: sdstat
        character(len=24), intent(in) :: sdtime
        character(len=24), intent(in) :: sdimpr
    end subroutine nmible
end interface
