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
    subroutine nmcoun(mesh          , list_func_acti, solver   , nume_dof_ , matr_asse  ,&
                      iter_newt     , time_curr     , hval_incr, hval_algo , hval_veasse,&
                      resi_glob_rela, sdtime        , sdstat   , ds_contact, ctccvg)
        use NonLin_Datastructure_type
        character(len=8), intent(in) :: mesh
        integer, intent(in) :: list_func_acti(*)
        character(len=19), intent(in) :: solver
        character(len=*), intent(in) :: nume_dof_
        character(len=19), intent(in) :: matr_asse
        integer, intent(in) :: iter_newt
        real(kind=8), intent(in) :: time_curr
        character(len=19), intent(in) :: hval_incr(*)
        character(len=19), intent(in) :: hval_algo(*)
        character(len=19), intent(in) :: hval_veasse(*)
        real(kind=8), intent(in) :: resi_glob_rela
        character(len=24), intent(in) :: sdtime 
        character(len=24), intent(in) :: sdstat
        type(NL_DS_Contact), intent(inout) :: ds_contact 
        integer, intent(out) :: ctccvg
    end subroutine nmcoun
end interface
