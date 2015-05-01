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
    subroutine comp1d(fami, kpg, ksp, option, sigx,&
                      epsx, depx, angmas, vim, vip,&
                      sigxp, etan, codret)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: option
        real(kind=8) :: sigx
        real(kind=8) :: epsx
        real(kind=8) :: depx
        real(kind=8) :: angmas(3)
        real(kind=8) :: vim(*)
        real(kind=8) :: vip(*)
        real(kind=8) :: sigxp
        real(kind=8) :: etan
        integer :: codret
    end subroutine comp1d
end interface
