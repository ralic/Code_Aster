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
    subroutine lcjacp(fami, kpg, ksp, rela_comp, toler,&
                      itmax, mod, imat, nmat, materd,&
                      materf, nr, nvi, timed, timef,&
                      deps, epsd, vind, vinf, yd,&
                      nbcomm, cpmono, pgl, nfs,&
                      nsg, toutms, hsr, dy, r,&
                      drdy, verjac, drdyb, iret, crit,&
                      indi)
        integer :: nsg
        integer :: nfs
        integer :: nvi
        integer :: nr
        integer :: nmat
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: rela_comp
        real(kind=8) :: toler
        integer :: itmax
        character(len=8) :: mod
        integer :: imat
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        real(kind=8) :: timed
        real(kind=8) :: timef
        real(kind=8) :: deps(6)
        real(kind=8) :: epsd(6)
        real(kind=8) :: vind(nvi)
        real(kind=8) :: vinf(nvi)
        real(kind=8) :: yd(nr)
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: toutms(nfs, nsg, 6)
        real(kind=8) :: hsr(nsg, nsg)
        real(kind=8) :: dy(nr)
        real(kind=8) :: r(nr)
        real(kind=8) :: drdy(nr, nr)
        integer :: verjac
        real(kind=8) :: drdyb(nr, nr)
        integer :: iret
        real(kind=8) :: crit(*)
        integer :: indi(7)
    end subroutine lcjacp
end interface
