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
    subroutine lcmmfe(taus, coeft, materf, ifa, nmat,&
                      nbcomm, necoul, is, nbsys, vind,&
                      dy, rp, alphap, gammap, dt,&
                      dalpha, dgamma, dp, crit, sgns,&
                      nfs, nsg, hsr, iret)
        integer :: nsg
        integer :: nmat
        real(kind=8) :: taus
        real(kind=8) :: coeft(nmat)
        real(kind=8) :: materf(nmat)
        integer :: ifa
        integer :: nbcomm(nmat, 3)
        character(len=16) :: necoul
        integer :: is
        integer :: nbsys
        real(kind=8) :: vind(*)
        real(kind=8) :: dy(*)
        real(kind=8) :: rp
        real(kind=8) :: alphap
        real(kind=8) :: gammap
        real(kind=8) :: dt
        real(kind=8) :: dalpha
        real(kind=8) :: dgamma
        real(kind=8) :: dp
        real(kind=8) :: crit
        real(kind=8) :: sgns
        integer :: nfs
        real(kind=8) :: hsr(nsg, nsg)
        integer :: iret
    end subroutine lcmmfe
end interface
