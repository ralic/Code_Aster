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
    subroutine get_meta_visc(poum     , fami     , kpg, ksp, j_mater,&
                             meta_type, nb_phasis, eta, n  , unsurn ,&
                             c        , m)
        character(len=1), intent(in) :: poum
        character(len=*), intent(in) :: fami
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        integer, intent(in) :: j_mater
        integer, intent(in) :: meta_type
        integer, intent(in) :: nb_phasis
        real(kind=8), optional, intent(out) :: eta(*)
        real(kind=8), optional, intent(out) :: n(*)
        real(kind=8), optional, intent(out) :: unsurn(*)
        real(kind=8), optional, intent(out) :: c(*)
        real(kind=8), optional, intent(out) :: m(*)
    end subroutine get_meta_visc
end interface
