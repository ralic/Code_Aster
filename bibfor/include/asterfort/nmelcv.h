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
    subroutine nmelcv(phase    , mesh     , model    , mate     , ds_contact    ,&
                      disp_prev, vite_prev, acce_prev, vite_curr, disp_cumu_inst,&
                      vect_elem, time_prev, time_curr, ds_constitutive, list_func_acti)
        use NonLin_Datastructure_type
        character(len=4), intent(in) :: phase
        character(len=8), intent(in) :: mesh
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        type(NL_DS_Contact), intent(in) :: ds_contact
        character(len=19), intent(in) :: disp_prev
        character(len=19), intent(in) :: vite_prev
        character(len=19), intent(in) :: acce_prev
        character(len=19), intent(in) :: vite_curr
        character(len=19), intent(in) :: disp_cumu_inst
        character(len=19), intent(out) :: vect_elem
        character(len=19), intent(in) :: time_prev
        character(len=19), intent(in) :: time_curr
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        integer, intent(in) :: list_func_acti(*)
    end subroutine nmelcv
end interface
