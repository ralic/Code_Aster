!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
!
interface
    subroutine lc5006(fami, kpg, ksp, ndim, imate,&
                      compor, carcri, instam, instap, neps,&
                      epsm, deps, nsig, sigm, vim,&
                      option, angmas, sigp, vip, nwkin,&
                      wkin, typmod, icomp, nvi, ndsde,&
                      dsidep, nwkout, wkout, codret)
        character(len=*), intent(in) :: fami
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        integer, intent(in) :: ndim
        integer, intent(in) :: imate
        character(len=16), intent(in) :: compor(*)
        real(kind=8), intent(in) :: carcri(*)
        real(kind=8), intent(in) :: instam
        real(kind=8), intent(in) :: instap
        integer, intent(in) :: neps
        real(kind=8), intent(in) :: epsm(*)
        real(kind=8), intent(in) :: deps(*)
        integer, intent(in) :: nsig
        real(kind=8), intent(in) :: sigm(*)
        real(kind=8), intent(in) :: vim(*)
        character(len=16), intent(in) :: option
        real(kind=8), intent(in) :: angmas(*)
        real(kind=8), intent(out) :: sigp(*)
        real(kind=8), intent(out) :: vip(*)
        integer, intent(in) :: nwkin
        real(kind=8), intent(in) :: wkin(nwkin)
        character(len=8), intent(in) :: typmod(*)
        integer, intent(in) :: nwkout
        real(kind=8), intent(out) :: wkout(nwkout)
        integer, intent(in) :: icomp
        integer, intent(in) :: nvi
        integer, intent(in) :: ndsde
        real(kind=8), intent(out) :: dsidep(*)
        integer, intent(out) :: codret
    end subroutine lc5006
end interface
