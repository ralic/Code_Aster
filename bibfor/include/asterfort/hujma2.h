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
    subroutine hujma2(fami, kpg, ksp, mod, imat,&
                      nmat, tempf, angmas, sigd, vind,&
                      materd, materf, ndt, ndi, nvi,&
                      nr, matcst)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: nmat
        character(len=8) :: mod
        integer :: imat
        real(kind=8) :: tempf
        real(kind=8) :: angmas(3)
        real(kind=8) :: sigd(6)
        real(kind=8) :: vind(50)
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        integer :: ndt
        integer :: ndi
        integer :: nvi
        integer :: nr
        character(len=3) :: matcst
    end subroutine hujma2
end interface
