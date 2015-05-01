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
    subroutine lcmmfi(coeft, ifa, nmat, nbcomm, necris,&
                      is, nbsys, vind, nsfv, dy,&
                      nfs, nsg, hsr, iexp, expbp,&
                      rp)
        integer :: nsg
        integer :: nmat
        real(kind=8) :: coeft(nmat)
        integer :: ifa
        integer :: nbcomm(nmat, 3)
        character(len=16) :: necris
        integer :: is
        integer :: nbsys
        real(kind=8) :: vind(*)
        integer :: nsfv
        real(kind=8) :: dy(*)
        integer :: nfs
        real(kind=8) :: hsr(nsg, nsg)
        integer :: iexp
        real(kind=8) :: expbp(*)
        real(kind=8) :: rp
    end subroutine lcmmfi
end interface
