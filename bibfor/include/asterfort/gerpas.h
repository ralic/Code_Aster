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
    subroutine gerpas(fami, kpg, ksp, comp, mod,&
                      imat, matcst, nbcomm, cpmono, nbphas,&
                      nvi, nmat, y, pas, itmax,&
                      eps, toly, cothe, coeff, dcothe,&
                      dcoeff, coel, pgl, angmas, neps,&
                      epsd, detot, x, nfs, nsg,&
                      nhsr, numhsr, hsr, iret)
        integer :: nhsr
        integer :: nsg
        integer :: nfs
        integer :: neps
        integer :: nmat
        integer :: nvi
        integer :: nbphas
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: comp(*)
        character(len=8) :: mod
        integer :: imat
        character(len=3) :: matcst
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        real(kind=8) :: y(nvi)
        real(kind=8) :: pas
        integer :: itmax
        real(kind=8) :: eps
        real(kind=8) :: toly
        real(kind=8) :: cothe(nmat)
        real(kind=8) :: coeff(nmat)
        real(kind=8) :: dcothe(nmat)
        real(kind=8) :: dcoeff(nmat)
        real(kind=8) :: coel(nmat)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: angmas(3)
        real(kind=8) :: epsd(neps)
        real(kind=8) :: detot(neps)
        real(kind=8) :: x
        integer :: numhsr(*)
        real(kind=8) :: hsr(nsg, nsg, nhsr)
        integer :: iret
    end subroutine gerpas
end interface
