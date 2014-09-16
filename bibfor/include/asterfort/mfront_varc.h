! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
interface
    subroutine mfront_varc(fami, kpg, ksp, imate, ifm, niv, idbg, &
                           lvarc, nbvarc, nwkin, wkin, temp, dtemp, &
                           predef, dpred, neps, epsth, depsth )
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: imate
        integer :: ifm
        integer :: niv
        integer :: idbg, nkwin
        character(len=8)  :: lvarc(8)
        integer :: nbvarc, nwkin
        real(kind=8) :: temp
        real(kind=8) :: dtemp, wkin(nwkin)
        real(kind=8) :: predef(8)
        real(kind=8) :: dpred(8)
        integer :: neps
        real(kind=8) :: epsth(neps)
        real(kind=8) :: depsth(neps)
    end subroutine mfront_varc
end interface
