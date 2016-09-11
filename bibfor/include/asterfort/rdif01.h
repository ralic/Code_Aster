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
    subroutine rdif01(fami, kpg, ksp, rela_comp, mod,&
                      imat, matcst, nbcomm, cpmono, nfs,&
                      nsg, toutms, nvi, nmat, vini,&
                      cothe, coeff, dcothe, dcoeff, pgl,&
                      nbphas, coel, x, dtime, neps,&
                      epsd, detot, dvin, nhsr, numhsr,&
                      hsr, itmax, toler, iret)
        integer :: nhsr
        integer :: nmat
        integer :: nvi
        integer :: nsg
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: rela_comp
        character(len=8) :: mod
        integer :: imat
        character(len=3) :: matcst
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        integer :: nfs
        real(kind=8) :: toutms(*)
        real(kind=8) :: vini(nvi)
        real(kind=8) :: cothe(nmat)
        real(kind=8) :: coeff(nmat)
        real(kind=8) :: dcothe(nmat)
        real(kind=8) :: dcoeff(nmat)
        real(kind=8) :: pgl(3, 3)
        integer :: nbphas
        real(kind=8) :: coel(nmat)
        real(kind=8) :: x
        real(kind=8) :: dtime
        integer :: neps
        real(kind=8) :: epsd(6)
        real(kind=8) :: detot(6)
        real(kind=8) :: dvin(nvi)
        integer :: numhsr(*)
        real(kind=8) :: hsr(nsg, nsg, nhsr)
        integer :: itmax
        real(kind=8) :: toler
        integer :: iret
    end subroutine rdif01
end interface
