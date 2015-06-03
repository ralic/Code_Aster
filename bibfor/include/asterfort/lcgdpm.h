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
    subroutine lcgdpm(fami, kpg, ksp, ndim, imat,&
                      compor, crit, instam, instap, fm,&
                      df, sigm, vim, option, sigp,&
                      vip, dsigdf, iret)
        character(len=*), intent(in) :: fami
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        integer, intent(in) :: ndim
        integer, intent(in) :: imat
        character(len=16), intent(in) :: compor(*)
        real(kind=8), intent(in) :: crit(*)
        real(kind=8), intent(in) :: instam
        real(kind=8), intent(in) :: instap
        real(kind=8), intent(in) :: fm(3, 3)
        real(kind=8), intent(in) :: df(3, 3)
        real(kind=8), intent(in) :: sigm(*)
        real(kind=8), intent(in) :: vim(8)
        character(len=16), intent(in) :: option
        real(kind=8), intent(out) :: sigp(*)
        real(kind=8), intent(out) :: vip(8)
        real(kind=8), intent(out) :: dsigdf(6, 3, 3)
        integer, intent(out) :: iret
    end subroutine lcgdpm
end interface
