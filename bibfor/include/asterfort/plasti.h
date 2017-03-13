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
    subroutine plasti(fami, kpg, ksp, typmod, imate,&
                      compor, carcri, instam, instap, &
                      epsdt, depst, sigm,&
                      vim, option, angmas, sigp, vip,&
                      dsidep, icomp, nvi, codret, mult_compor_, wkin_)
        character(len=*), intent(in) :: fami
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        integer, intent(in) :: imate
        character(len=16), intent(in) :: compor(*)
        real(kind=8), intent(in) :: carcri(*)
        real(kind=8), intent(in) :: instam
        real(kind=8), intent(in) :: instap
        real(kind=8), intent(in) :: epsdt(9)
        real(kind=8), intent(in) :: depst(9)
        real(kind=8), intent(in) :: sigm(6)
        real(kind=8), intent(in) :: vim(*)
        character(len=16), intent(in) :: option
        real(kind=8), intent(in) :: angmas(3)
        real(kind=8), intent(out) :: sigp(6)
        real(kind=8), intent(out) :: vip(*)
        character(len=8), intent(in) :: typmod(*)
        integer, intent(in) :: icomp
        integer, intent(in) :: nvi
        real(kind=8), intent(out) :: dsidep(6, *)
        integer, intent(out) :: codret
        character(len=16), optional, intent(in) :: mult_compor_
        real(kind=8), optional, intent(in) :: wkin_(9)
    end subroutine plasti
end interface
