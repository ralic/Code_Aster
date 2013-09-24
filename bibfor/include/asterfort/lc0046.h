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
    subroutine lc0046(fami, kpg, ksp, ndim, imate,&
                      compor, crit, instam, instap, neps,&
                      epsm, deps, nsig, sigm, vim,&
                      option, angmas, sigp, vip, nwkin,&
                      wkin, typmod, icomp, nvi, ndsde,&
                      dsidep, nwkout, wkout, codret)
        integer :: nwkout
        integer :: ndsde
        integer :: nvi
        integer :: nwkin
        integer :: nsig
        integer :: neps
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: ndim
        integer :: imate
        character(len=16) :: compor(*)
        real(kind=8) :: crit(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: epsm(neps)
        real(kind=8) :: deps(neps)
        real(kind=8) :: sigm(nsig)
        real(kind=8) :: vim(nvi)
        character(len=16) :: option
        real(kind=8) :: angmas(3)
        real(kind=8) :: sigp(nsig)
        real(kind=8) :: vip(nvi)
        real(kind=8) :: wkin(nwkin)
        character(len=8) :: typmod(*)
        integer :: icomp
        real(kind=8) :: dsidep(ndsde)
        real(kind=8) :: wkout(nwkout)
        integer :: codret
    end subroutine lc0046
end interface
