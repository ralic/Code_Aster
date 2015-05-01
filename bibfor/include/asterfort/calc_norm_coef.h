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
    subroutine calc_norm_coef(model       , name_gd     , nb_cmp_max  , nb_cmp_in, norm  , &
                              calc_elem   , list_cmp    , nb_coef_user, coef_user, chcoef, &
                              chcalc      , nb_cmp_act)
        character(len=8), intent(in) :: name_gd
        character(len=8), intent(in) :: model
        integer, intent(in) :: nb_cmp_max
        integer, intent(in) :: nb_cmp_in
        character(len=16) , intent(in) :: norm
        character(len=4) , intent(in) :: calc_elem
        integer, intent(in) :: nb_coef_user
        real(kind=8), intent(in) :: coef_user(*)
        character(len=24), intent(in) :: list_cmp
        character(len=19), intent(in) :: chcoef
        character(len=19), intent(in) :: chcalc
        integer, intent(out) :: nb_cmp_act
    end subroutine calc_norm_coef
end interface
