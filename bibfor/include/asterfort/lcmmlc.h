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
    subroutine lcmmlc(nmat, nbcomm, cpmono, nfs, nsg,&
                      hsr, nsfv, nsfa, ifa, nbsys,&
                      is, dt, nvi, vind, yd,&
                      dy, itmax, toler, materf, expbp,&
                      taus, dalpha, dgamma, dp, crit,&
                      sgns, rp, iret)
        integer :: nvi
        integer :: nsg
        integer :: nmat
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        integer :: nfs
        real(kind=8) :: hsr(nsg, nsg)
        integer :: nsfv
        integer :: nsfa
        integer :: ifa
        integer :: nbsys
        integer :: is
        real(kind=8) :: dt
        real(kind=8) :: vind(nvi)
        real(kind=8) :: yd(*)
        real(kind=8) :: dy(*)
        integer :: itmax
        real(kind=8) :: toler
        real(kind=8) :: materf(nmat*2)
        real(kind=8) :: expbp(nsg)
        real(kind=8) :: taus
        real(kind=8) :: dalpha
        real(kind=8) :: dgamma
        real(kind=8) :: dp
        real(kind=8) :: crit
        real(kind=8) :: sgns
        real(kind=8) :: rp
        integer :: iret
    end subroutine lcmmlc
end interface
