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
    subroutine nmcpel(fami, kpg, ksp, poum, ndim,&
                      typmod, angmas, imate, compor, crit,&
                      option, eps, sig, vi, dsidep,&
                      codret)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=*) :: poum
        integer :: ndim
        character(len=8) :: typmod(*)
        real(kind=8) :: angmas(3)
        integer :: imate
        character(len=16) :: compor(*)
        real(kind=8) :: crit(3)
        character(len=16) :: option
        real(kind=8) :: eps(6)
        real(kind=8) :: sig(6)
        real(kind=8) :: vi(*)
        real(kind=8) :: dsidep(6, 6)
        integer :: codret
    end subroutine nmcpel
end interface
