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
    subroutine lcmmj1(taur, materf, cpmono, ifa, nmat,&
                      nbcomm, dt, nsfv, nsfa, ir,&
                      is, nbsys, nfs, nsg, hsr,&
                      vind, dy, iexp, expbp, itmax,&
                      toler, dgsdts, dksdts, dgrdbs, dkrdbs,&
                      iret)
        integer :: nsg
        integer :: nmat
        real(kind=8) :: taur
        real(kind=8) :: materf(nmat*2)
        character(len=24) :: cpmono(5*nmat+1)
        integer :: ifa
        integer :: nbcomm(nmat, 3)
        real(kind=8) :: dt
        integer :: nsfv
        integer :: nsfa
        integer :: ir
        integer :: is
        integer :: nbsys
        integer :: nfs
        real(kind=8) :: hsr(nsg, nsg)
        real(kind=8) :: vind(*)
        real(kind=8) :: dy(*)
        integer :: iexp
        real(kind=8) :: expbp(nsg)
        integer :: itmax
        real(kind=8) :: toler
        real(kind=8) :: dgsdts
        real(kind=8) :: dksdts
        real(kind=8) :: dgrdbs
        real(kind=8) :: dkrdbs
        integer :: iret
    end subroutine lcmmj1
end interface
