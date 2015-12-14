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
#include "asterf_types.h"
!
interface
    subroutine cfalgo(mesh          , sdstat    , resi_glob_rela, iter_newt,&
                      solver        , nume_dof  , matr_asse     , disp_iter,&
                      disp_cumu_inst, ds_contact, ctccvg        )
        use NonLin_Datastructure_type
        character(len=8), intent(in) :: mesh
        character(len=24), intent(in) :: sdstat
        real(kind=8), intent(in) :: resi_glob_rela
        integer, intent(in) :: iter_newt
        character(len=19), intent(in) :: solver
        character(len=14), intent(in) :: nume_dof
        character(len=19), intent(in) :: matr_asse
        character(len=19), intent(in) :: disp_iter
        character(len=19), intent(in) :: disp_cumu_inst
        type(NL_DS_Contact), intent(inout) :: ds_contact 
        integer, intent(out) :: ctccvg
    end subroutine cfalgo
end interface
