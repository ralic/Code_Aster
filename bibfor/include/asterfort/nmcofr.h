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
    subroutine nmcofr(mesh      , disp_curr, disp_cumu_inst, disp_iter, solver        ,&
                      nume_dof  , matr_asse, iter_newt     , time_curr, resi_glob_rela,&
                      ds_measure, ds_contact    , ctccvg)
        use NonLin_Datastructure_type
        character(len=8), intent(in) :: mesh
        character(len=19), intent(in) :: disp_curr
        character(len=19), intent(in) :: disp_cumu_inst
        character(len=19), intent(in) :: disp_iter
        character(len=19), intent(in) :: solver
        character(len=14), intent(in) :: nume_dof
        character(len=19), intent(in) :: matr_asse
        integer, intent(in) :: iter_newt
        real(kind=8), intent(in) :: time_curr
        real(kind=8), intent(in) :: resi_glob_rela
        type(NL_DS_Measure), intent(inout) :: ds_measure
        type(NL_DS_Contact), intent(inout) :: ds_contact 
        integer, intent(out) :: ctccvg
    end subroutine nmcofr
end interface
