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
    subroutine nmelco_prep(phase    , calc_type,&
                           mesh     , model    , mate     , ds_contact,&
                           disp_prev, vite_prev, acce_prev, vite_curr , disp_cumu_inst,&
                           nbin     , lpain    , lchin    ,&
                           option   , ccohes_  , xcohes_)
        use NonLin_Datastructure_type
        character(len=4), intent(in) :: phase
        character(len=4), intent(in) :: calc_type
        character(len=8), intent(in) :: mesh
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        type(NL_DS_Contact), intent(in) :: ds_contact
        character(len=19), intent(in) :: disp_prev
        character(len=19), intent(in) :: vite_prev
        character(len=19), intent(in) :: acce_prev
        character(len=19), intent(in) :: vite_curr
        character(len=19), intent(in) :: disp_cumu_inst
        integer, intent(in) :: nbin
        character(len=8), intent(out) :: lpain(nbin)
        character(len=19), intent(out) :: lchin(nbin)
        character(len=16), intent(out) :: option
        character(len=19), optional, intent(out) :: ccohes_
        character(len=19), optional, intent(out) :: xcohes_
    end subroutine nmelco_prep
end interface
