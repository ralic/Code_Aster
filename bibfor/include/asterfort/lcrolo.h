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
    subroutine lcrolo(fami, kpg, ksp, mate, option,&
                      carcri, fm, df, vim, vip,&
                      taup, dtaudf, iret)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: mate
        character(len=16) :: option
        real(kind=8) :: carcri(3)
        real(kind=8) :: fm(3, 3)
        real(kind=8) :: df(3, 3)
        real(kind=8) :: vim(9)
        real(kind=8) :: vip(9)
        real(kind=8) :: taup(6)
        real(kind=8) :: dtaudf(6, 3, 3)
        integer :: iret
    end subroutine lcrolo
end interface
