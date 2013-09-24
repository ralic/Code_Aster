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
    subroutine paeldt(kpg, ksp, fami, poum, icdmat,&
                      materi, em, ep, nup, depsth,&
                      tmoins, tplus, trefer)
        integer :: kpg
        integer :: ksp
        character(len=4) :: fami
        character(len=1) :: poum
        integer :: icdmat
        character(len=8) :: materi
        real(kind=8) :: em
        real(kind=8) :: ep
        real(kind=8) :: nup
        real(kind=8) ::depsth
        real(kind=8), intent(out), optional :: tmoins
        real(kind=8), intent(out), optional :: tplus
        real(kind=8), intent(out), optional :: trefer
    end subroutine paeldt
end interface
