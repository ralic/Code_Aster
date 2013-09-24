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
    subroutine rslphi(fami, kpg, ksp, loi, imat,&
                      troisk, troimu, depsmo, rigdmo, rieleq,&
                      pi, d, s1, ann, theta,&
                      acc, f, df, sig0, eps0,&
                      mexpo, dt, phi, phip, rigeq,&
                      rigm, p, overfl)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: loi
        integer :: imat
        real(kind=8) :: troisk
        real(kind=8) :: troimu
        real(kind=8) :: depsmo
        real(kind=8) :: rigdmo
        real(kind=8) :: rieleq
        real(kind=8) :: pi
        real(kind=8) :: d
        real(kind=8) :: s1
        real(kind=8) :: ann
        real(kind=8) :: theta
        real(kind=8) :: acc
        real(kind=8) :: f
        real(kind=8) :: df
        real(kind=8) :: sig0
        real(kind=8) :: eps0
        real(kind=8) :: mexpo
        real(kind=8) :: dt
        real(kind=8) :: phi
        real(kind=8) :: phip
        real(kind=8) :: rigeq
        real(kind=8) :: rigm
        real(kind=8) :: p
        logical :: overfl
    end subroutine rslphi
end interface
