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
    subroutine lcumfe(fami, kpg, ksp, ndim, typmod,&
                      imate, tinstm, tinstp, epstm, depst,&
                      sigm, vim, option, rela_plas, sigp,&
                      vip, dsidpt, proj)
        integer, intent(in) :: ndim
        integer, intent(in) :: imate
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        character(len=8), intent(in) :: typmod(*)
        character(len=16), intent(in) :: rela_plas
        character(len=16), intent(in) :: option
        character(len=*), intent(in) :: fami
        real(kind=8) :: tinstm
        real(kind=8) :: tinstp
        real(kind=8) :: epstm(12)
        real(kind=8) :: depst(12)
        real(kind=8) :: sigm(6)
        real(kind=8) :: vim(25)
        real(kind=8) :: sigp(6)
        real(kind=8) :: vip(25)
        real(kind=8) :: dsidpt(6, 6, 2)
        real(kind=8) :: proj(6, 6)
    end subroutine lcumfe
end interface
