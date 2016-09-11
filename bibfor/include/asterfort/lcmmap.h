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
    subroutine lcmmap(fami, kpg, ksp, mult_comp, mod,&
                      imat, nmat, angmas, pgl, materd,&
                      materf, matcst, nbcomm, cpmono, ndt,&
                      ndi, nr, nvi, nfs, nsg,&
                      nhsr, numhsr, hsr)
        integer :: nhsr
        integer :: nsg
        integer :: nmat
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: mult_comp
        character(len=8) :: mod
        integer :: imat
        real(kind=8) :: angmas(3)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        character(len=3) :: matcst
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        integer :: ndt
        integer :: ndi
        integer :: nr
        integer :: nvi
        integer :: nfs
        integer :: numhsr(nhsr)
        real(kind=8) :: hsr(nsg, nsg, nhsr)
    end subroutine lcmmap
end interface
