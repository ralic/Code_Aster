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
! aslint: disable=W1504
interface
    subroutine mmalgo(sd_cont_defi  , sd_cont_solv  , l_loop_cont   , l_frot_zone   , l_vite    , &
                  l_glis_init   , l_coef_adap   , zone_index    , point_index   , indi_cont_init, &
                  indi_cont_eval, indi_frot_eval, dist_cont_curr, vite_cont_curr, pres_cont_curr, &
                  dist_frot_curr, pres_frot_curr, cycl_hist     , cycl_coef     , indi_cont_curr, &
                  indi_frot_curr, ctcsta        , mmcvca        , scotch        )
        character(len=24), intent(in) :: sd_cont_defi
        character(len=24), intent(in) :: sd_cont_solv
        logical(kind=1), intent(in) :: l_loop_cont
        logical(kind=1), intent(in) :: l_frot_zone
        logical(kind=1), intent(in) :: l_vite
        logical(kind=1), intent(in) :: l_glis_init
        logical(kind=1), intent(in) :: l_coef_adap
        integer, intent(in) :: point_index
        integer, intent(in) :: zone_index
        integer, intent(in) :: indi_cont_init
        integer, intent(in) :: indi_cont_eval
        integer, intent(in) :: indi_frot_eval
        real(kind=8), intent(in) :: dist_cont_curr
        real(kind=8), intent(in) :: vite_cont_curr
        real(kind=8), intent(in) :: pres_cont_curr
        real(kind=8), intent(in) :: dist_frot_curr(3)
        real(kind=8), intent(in) :: pres_frot_curr(3)
        real(kind=8), intent(inout) :: cycl_hist(*) 
        real(kind=8), intent(inout) :: cycl_coef(*) 
        integer, intent(out) :: indi_cont_curr
        integer, intent(out) :: indi_frot_curr
        integer, intent(out) :: ctcsta
        logical(kind=1), intent(out) :: mmcvca
        logical(kind=1), intent(out) :: scotch
    end subroutine mmalgo
end interface
