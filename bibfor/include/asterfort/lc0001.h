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
    subroutine lc0001(fami, kpg, ksp, ndim, imate,&
                      neps, deps, nsig, sigm, option,&
                      angmas, sigp, vip, typmod, ndsde,&
                      dsidep, codret)
        integer :: ndsde
        integer :: nsig
        integer :: neps
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: ndim
        integer :: imate
        real(kind=8) :: deps(neps)
        real(kind=8) :: sigm(nsig)
        character(len=16) :: option
        real(kind=8) :: angmas(3)
        real(kind=8) :: sigp(nsig)
        real(kind=8) :: vip(1)
        character(len=8) :: typmod(*)
        real(kind=8) :: dsidep(ndsde)
        integer :: codret
    end subroutine lc0001
end interface
