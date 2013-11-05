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
    subroutine lcjacb(fami, kpg, ksp, loi, mod,&
                      nmat, materd, materf, timed, timef,&
                      yf, deps, itmax, toler, nbcomm,&
                      cpmono, pgl, nfs, nsg, toutms,&
                      hsr, nr, comp, nvi, vind,&
                      vinf, epsd, yd, dy, ye,&
                      crit, indi, vind1, bnews, mtrac,&
                      drdy, iret)
        integer :: nvi
        integer :: nr
        integer :: nsg
        integer :: nfs
        integer :: nmat
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: loi
        character(len=8) :: mod
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        real(kind=8) :: timed
        real(kind=8) :: timef
        real(kind=8) :: yf(nr)
        real(kind=8) :: deps(*)
        integer :: itmax
        real(kind=8) :: toler
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: toutms(nfs, nsg, 6)
        real(kind=8) :: hsr(nsg, nsg)
        character(len=16) :: comp(*)
        real(kind=8) :: vind(*)
        real(kind=8) :: vinf(*)
        real(kind=8) :: epsd(*)
        real(kind=8) :: yd(nr)
        real(kind=8) :: dy(nr)
        real(kind=8) :: ye(nr)
        real(kind=8) :: crit(*)
        integer :: indi(7)
        real(kind=8) :: vind1(nvi)
        logical :: bnews(3)
        logical :: mtrac
        real(kind=8) :: drdy(nr, nr)
        integer :: iret
    end subroutine lcjacb
end interface
