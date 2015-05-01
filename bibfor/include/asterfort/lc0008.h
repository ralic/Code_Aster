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
    subroutine lc0008(fami, kpg, ksp, ndim, imate,&
                      compor, crit, instam, instap, epsm,&
                      deps, sigm, vim, option, angmas,&
                      sigp, vip, tampon, typmod, icomp,&
                      nvi, dsidep, codret)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: ndim
        integer :: imate
        character(len=16) :: compor(6)
        real(kind=8) :: crit(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: epsm(*)
        real(kind=8) :: deps(*)
        real(kind=8) :: sigm(*)
        real(kind=8) :: vim(*)
        character(len=16) :: option
        real(kind=8) :: angmas(*)
        real(kind=8) :: sigp(*)
        real(kind=8) :: vip(*)
        real(kind=8) :: tampon(*)
        character(len=8) :: typmod(2)
        integer :: icomp
        integer :: nvi
        real(kind=8) :: dsidep(*)
        integer :: codret
    end subroutine lc0008
end interface
