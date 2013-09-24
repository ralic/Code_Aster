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
    subroutine lcedga(fami, kpg, ksp, ndim, imat,&
                      crit, typmod, instam, instap, coord,&
                      deps2, sigm2, vim, option, sigp,&
                      vip, dsidep, iret)
        integer :: ndim
        character(*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: imat
        real(kind=8) :: crit(3)
        character(len=8) :: typmod(2)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: coord(3)
        real(kind=8) :: deps2(6)
        real(kind=8) :: sigm2(6)
        real(kind=8) :: vim(2)
        character(len=16) :: option
        real(kind=8) :: sigp(6)
        real(kind=8) :: vip(2)
        real(kind=8) :: dsidep(6, 6)
        integer :: iret
    end subroutine lcedga
end interface
