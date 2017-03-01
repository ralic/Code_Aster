!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine calcPrepDataMeca(model          , mate          , cara_elem,&
                                disp_prev      , disp_cumu_inst, vari_prev, sigm_prev,&
                                time_prev      , time_curr     ,&
                                ds_constitutive, varc_refe     ,&
                                hval_incr      , hval_algo     ,&
                                merigi         , vediri        , vefint   , veforc   ,&
                                vevarc_prev    , vevarc_curr   )
        use NonLin_Datastructure_type
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=19), intent(in) :: disp_prev
        character(len=19), intent(in) :: disp_cumu_inst
        character(len=19), intent(in) :: vari_prev
        character(len=19), intent(in) :: sigm_prev
        real(kind=8), intent(in) :: time_prev
        real(kind=8), intent(in) :: time_curr
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        character(len=24), intent(out) :: varc_refe
        character(len=19), intent(out) :: hval_incr(:)
        character(len=19), intent(out) :: hval_algo(:)
        character(len=19), intent(out) :: merigi
        character(len=19), intent(out) :: vediri
        character(len=19), intent(out) :: vefint
        character(len=19), intent(out) :: veforc
        character(len=19), intent(out) :: vevarc_prev
        character(len=19), intent(out) :: vevarc_curr
    end subroutine calcPrepDataMeca
end interface
