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
    subroutine nm1vil(fami, kpg, ksp, icdmat, materi,&
                      crit, instam, instap, tm, tp,&
                      tref, deps, sigm, vim, option,&
                      defam, defap, angmas, sigp, vip,&
                      dsidep, iret, compo, nbvalc)
        integer :: nbvalc
        character(*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: icdmat
        character(len=8) :: materi
        real(kind=8) :: crit(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: tm
        real(kind=8) :: tp
        real(kind=8) :: tref
        real(kind=8) :: deps
        real(kind=8) :: sigm
        real(kind=8) :: vim(nbvalc)
        character(len=16) :: option
        real(kind=8) :: defam
        real(kind=8) :: defap
        real(kind=8) :: angmas(3)
        real(kind=8) :: sigp
        real(kind=8) :: vip(nbvalc)
        real(kind=8) :: dsidep
        integer :: iret
        character(len=16) :: compo
    end subroutine nm1vil
end interface
