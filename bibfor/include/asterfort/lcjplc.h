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
    subroutine lcjplc(loi, mod, angmas, imat, nmat,&
                      mater, timed, timef, comp, nbcomm,&
                      cpmono, pgl, nfs, nsg, toutms,&
                      hsr, nr, nvi, epsd, deps,&
                      itmax, toler, sigf, vinf, sigd,&
                      vind, dsde, drdy, option, iret)
        integer :: nr
        integer :: nsg
        integer :: nfs
        integer :: nmat
        character(len=16) :: loi
        character(len=8) :: mod
        real(kind=8) :: angmas(3)
        integer :: imat
        real(kind=8) :: mater(nmat, 2)
        real(kind=8) :: timed
        real(kind=8) :: timef
        character(len=16) :: comp(*)
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: toutms(nfs, nsg, 6)
        real(kind=8) :: hsr(nsg, nsg)
        integer :: nvi
        real(kind=8) :: epsd(*)
        real(kind=8) :: deps(*)
        integer :: itmax
        real(kind=8) :: toler
        real(kind=8) :: sigf(*)
        real(kind=8) :: vinf(*)
        real(kind=8) :: sigd(*)
        real(kind=8) :: vind(*)
        real(kind=8) :: dsde(6, 6)
        real(kind=8) :: drdy(nr, nr)
        character(len=16) :: option
        integer :: iret
    end subroutine lcjplc
end interface
