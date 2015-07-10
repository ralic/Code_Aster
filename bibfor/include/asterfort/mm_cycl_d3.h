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
    subroutine mm_cycl_d3(sdcont_defi  , sdcont_solv  , i_cont_poin, &
                          indi_frot_prev, dist_frot_prev, &
                          indi_cont_eval, indi_frot_eval, &
                          dist_frot_curr)
        character(len=24), intent(in) :: sdcont_defi
        character(len=24), intent(in) :: sdcont_solv
        integer, intent(in) :: i_cont_poin
        integer, intent(in) :: indi_frot_prev
        real(kind=8), intent(in) :: dist_frot_prev(3)
        integer, intent(in) :: indi_cont_eval
        integer, intent(in) :: indi_frot_eval
        real(kind=8), intent(in) :: dist_frot_curr(3)
    end subroutine mm_cycl_d3
end interface
