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
interface
    subroutine meta_vpta_coef(rela_comp, lgpg       , fami     , kpg      , j_mater  ,&
                              l_temp   , temp       , meta_type, nb_phasis, phas_prev,&
                              phas_curr, zalpha_curr, young    ,  deuxmu  , coef     ,&
                              trans)
        character(len=16), intent(in) :: rela_comp
        integer, intent(in) :: lgpg 
        character(len=4), intent(in) :: fami
        integer, intent(in) :: kpg
        integer, intent(in) :: j_mater
        logical, intent(in) :: l_temp
        real(kind=8), intent(in) :: temp
        integer, intent(in) :: meta_type
        integer, intent(in) :: nb_phasis
        real(kind=8), intent(in) :: phas_prev(*)
        real(kind=8), intent(in) :: phas_curr(*)
        real(kind=8), intent(in) :: zalpha_curr
        real(kind=8), intent(in) :: young
        real(kind=8), intent(in) :: deuxmu
        real(kind=8), intent(out) :: coef
        real(kind=8), intent(out) :: trans
    end subroutine meta_vpta_coef
end interface
