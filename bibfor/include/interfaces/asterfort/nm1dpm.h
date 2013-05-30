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
    subroutine nm1dpm(fami, kpg, ksp, imate, option,&
                      nvar, ncstpm, cstpm, sigm, vim,&
                      deps, vip, sigp, dsde)
        integer :: ncstpm
        integer :: nvar
        character(*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: imate
        character(*) :: option
        real(kind=8) :: cstpm(ncstpm)
        real(kind=8) :: sigm
        real(kind=8) :: vim(nvar)
        real(kind=8) :: deps
        real(kind=8) :: vip(nvar)
        real(kind=8) :: sigp
        real(kind=8) :: dsde
    end subroutine nm1dpm
end interface
