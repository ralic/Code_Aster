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
    subroutine verift(fami, kpg, ksp, poum, imate,&
                      materi, compor, iret, ndim, epsth,&
                      vepsth, tmoins, tplus, trefer)

        character(len=*), intent(in) :: fami
        character(len=*), intent(in) :: poum
        character(len=*), intent(in) :: compor
        character(len=8), intent(in) :: materi
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        integer, intent(in) :: imate
        integer, optional, intent(in) :: ndim
        integer, intent(out) :: iret
        real(kind=8), optional, intent(out) :: epsth
        real(kind=8), optional, intent(out) :: vepsth(*)
        real(kind=8), optional, intent(out) :: tmoins
        real(kind=8), optional, intent(out) :: tplus
        real(kind=8), optional, intent(out) :: trefer
    end subroutine verift
end interface
