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
    subroutine edgmat(fami, kpg, ksp, imat, c1,&
                      zalpha, temp, dt, mum, mu,&
                      troikm, troisk, alpham, alphap, ani,&
                      m, n, gamma)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: imat
        character(len=1) :: c1
        real(kind=8) :: zalpha
        real(kind=8) :: temp
        real(kind=8) :: dt
        real(kind=8) :: mum
        real(kind=8) :: mu
        real(kind=8) :: troikm
        real(kind=8) :: troisk
        real(kind=8) :: alpham
        real(kind=8) :: alphap
        real(kind=8) :: ani(6, 6)
        real(kind=8) :: m(3)
        real(kind=8) :: n(3)
        real(kind=8) :: gamma(3)
    end subroutine edgmat
end interface
