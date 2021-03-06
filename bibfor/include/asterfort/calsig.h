!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine calsig(fami, kpg, ksp, ein, mod,&
                      rela_comp, vini, x, dtime, epsd,&
                      detot, nmat, coel, sigi)
        integer :: nmat
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        real(kind=8) :: ein(6)
        character(len=8) :: mod
        character(len=16) :: rela_comp
        real(kind=8) :: vini(*)
        real(kind=8) :: x
        real(kind=8) :: dtime
        real(kind=8) :: epsd(6)
        real(kind=8) :: detot(6)
        real(kind=8) :: coel(nmat)
        real(kind=8) :: sigi(6)
    end subroutine calsig
end interface
