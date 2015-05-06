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
    subroutine edgmat(fami   , kpg   , ksp   , imat  , c1 ,&
                      zalpha , temp  , dt    , mum   , mu ,&
                      troiskm, troisk, ani   , m     , n  ,&
                      gamma  , zcylin)
        character(len=*), intent(in) :: fami
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        integer, intent(in) :: imat
        character(len=1), intent(in) :: c1
        real(kind=8), intent(in) :: zalpha
        real(kind=8), intent(in) :: temp
        real(kind=8), intent(in) :: dt
        real(kind=8), intent(out) :: mum
        real(kind=8), intent(out) :: mu
        real(kind=8), intent(out) :: troiskm
        real(kind=8), intent(out) :: troisk
        real(kind=8), intent(out) :: ani(6, 6)
        real(kind=8), intent(out) :: m(3)
        real(kind=8), intent(out) :: n(3)
        real(kind=8), intent(out) :: gamma(3)
        logical, intent(out) :: zcylin
    end subroutine edgmat
end interface
