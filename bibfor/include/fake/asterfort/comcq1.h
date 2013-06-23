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
    subroutine comcq1(fami, kpg, ksp, mod, imate,&
                      compor, carcri, instm, instp, eps,&
                      deps, tempm, tempp, sigm, vim,&
                      option, angmas, sigp, vip, dsde,&
                      codret)
        character(*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: mod
        integer :: imate
        character(len=16) :: compor(*)
        real(kind=8) :: carcri(*)
        real(kind=8) :: instm
        real(kind=8) :: instp
        real(kind=8) :: eps(4)
        real(kind=8) :: deps(4)
        real(kind=8) :: tempm
        real(kind=8) :: tempp
        real(kind=8) :: sigm(4)
        real(kind=8) :: vim(*)
        character(len=16) :: option
        real(kind=8) :: angmas(3)
        real(kind=8) :: sigp(4)
        real(kind=8) :: vip(*)
        real(kind=8) :: dsde(6, 6)
        integer :: codret
    end subroutine comcq1
end interface
