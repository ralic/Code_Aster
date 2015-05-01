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
    subroutine lcmmjd(taur, materf, ifa, nmat, nbcomm,&
                      dt, ir, is, nbsys, nfs,&
                      nsg, hsr, vind, dy, dpdtau,&
                      dprdas, dhrdas, hr, dpr, sgnr,&
                      iret)
        integer :: nsg
        integer :: nmat
        real(kind=8) :: taur
        real(kind=8) :: materf(nmat*2)
        integer :: ifa
        integer :: nbcomm(nmat, 3)
        real(kind=8) :: dt
        integer :: ir
        integer :: is
        integer :: nbsys
        integer :: nfs
        real(kind=8) :: hsr(nsg, nsg)
        real(kind=8) :: vind(36)
        real(kind=8) :: dy(12)
        real(kind=8) :: dpdtau
        real(kind=8) :: dprdas
        real(kind=8) :: dhrdas
        real(kind=8) :: hr
        real(kind=8) :: dpr
        real(kind=8) :: sgnr
        integer :: iret
    end subroutine lcmmjd
end interface
