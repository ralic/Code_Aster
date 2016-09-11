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
    subroutine lcmate(fami, kpg, ksp, comp, mod,&
                      imat, nmat, tempd, tempf, impexp,&
                      typma, hsr, materd, materf, matcst,&
                      nbcomm, cpmono, angmas, pgl, itmax,&
                      toler, ndt, ndi, nr, crit,&
                      nvi, vind, nfs, nsg, toutms,&
                      nhsr, numhsr, sigd, mult_comp_)
        integer :: nmat
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: comp(*)
        character(len=8) :: mod
        integer :: imat
        real(kind=8) :: tempd
        real(kind=8) :: tempf
        integer :: impexp
        character(len=8) :: typma
        real(kind=8) :: hsr(*)
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        character(len=3) :: matcst
        integer :: nbcomm(*)
        character(len=24) :: cpmono(*)
        real(kind=8) :: angmas(3)
        real(kind=8) :: pgl(3, 3)
        integer :: itmax
        real(kind=8) :: toler
        integer :: ndt
        integer :: ndi
        integer :: nr
        real(kind=8) :: crit(*)
        integer :: nvi
        real(kind=8) :: vind(*)
        integer :: nfs
        integer :: nsg
        real(kind=8) :: toutms(*)
        integer :: nhsr
        integer :: numhsr(*)
        real(kind=8) :: sigd(6)
        character(len=16), optional, intent(in) :: mult_comp_
    end subroutine lcmate
end interface
