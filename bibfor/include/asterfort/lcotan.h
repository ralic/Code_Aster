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
    subroutine lcotan(opt, angmas, etatd, etatf, fami,&
                      kpg, ksp, loi, mod, imat,&
                      nmat, materd, materf, epsd, deps,&
                      sigd, sigf, nvi, vind, vinf,&
                      drdy, vp, vecp, theta, dt,&
                      devg, devgii, timed, timef, comp,&
                      nbcomm, cpmono, pgl, nfs, nsg,&
                      toutms, hsr, nr, itmax, toler,&
                      typma, dsde, codret)
        integer :: nsg
        integer :: nfs
        integer :: nmat
        character(len=16) :: opt
        real(kind=8) :: angmas(3)
        character(len=7) :: etatd
        character(len=7) :: etatf
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        character(len=16) :: loi
        character(len=8) :: mod
        integer :: imat
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        real(kind=8) :: epsd(9)
        real(kind=8) :: deps(9)
        real(kind=8) :: sigd(6)
        real(kind=8) :: sigf(6)
        integer :: nvi
        real(kind=8) :: vind(*)
        real(kind=8) :: vinf(*)
        real(kind=8) :: drdy(*)
        real(kind=8) :: vp(3)
        real(kind=8) :: vecp(3, 3)
        real(kind=8) :: theta
        real(kind=8) :: dt
        real(kind=8) :: devg(6)
        real(kind=8) :: devgii
        real(kind=8) :: timed
        real(kind=8) :: timef
        character(len=16) :: comp(*)
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: toutms(nfs, nsg, 6)
        real(kind=8) :: hsr(nsg, nsg)
        integer :: nr
        integer :: itmax
        real(kind=8) :: toler
        character(len=8) :: typma
        real(kind=8) :: dsde(6, *)
        integer :: codret
    end subroutine lcotan
end interface
