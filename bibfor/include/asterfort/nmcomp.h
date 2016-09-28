!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine nmcomp(fami, kpg, ksp, ndim, typmod,&
                      imate, compor, carcri, instam, instap,&
                      neps, epsm, deps, nsig, sigm,&
                      vim, option, angmas, nwkin, wkin,&
                      sigp, vip, ndsde, dsidep, nwkout,&
                      wkout, codret, mult_comp_)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: ndim
        character(len=8) :: typmod(*)
        integer :: imate
        character(len=16) :: compor(*)
        real(kind=8) :: carcri(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        integer :: neps
        real(kind=8) :: epsm(*)
        real(kind=8) :: deps(*)
        integer :: nsig
        real(kind=8) :: sigm(*)
        real(kind=8) :: vim(*)
        character(len=16) :: option
        character(len=16), optional, intent(in) :: mult_comp_
        real(kind=8) :: angmas(*)
        integer :: nwkin
        real(kind=8) :: wkin(nwkin)
        real(kind=8) :: sigp(*)
        real(kind=8) :: vip(*)
        integer :: ndsde
        real(kind=8) :: dsidep(*)
        integer :: nwkout
        real(kind=8) :: wkout(nwkout)
        integer :: codret
    end subroutine nmcomp
end interface
