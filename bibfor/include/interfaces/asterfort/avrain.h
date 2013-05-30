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
    subroutine avrain(nbvec, nbordr, itrv, npic, pic,&
                      opic, fatsoc, ncycl, vmin, vmax,&
                      omin, omax)
        integer :: nbordr
        integer :: nbvec
        integer :: itrv(2*(nbordr+2))
        integer :: npic(nbvec)
        real(kind=8) :: pic(nbvec*(nbordr+2))
        integer :: opic(nbvec*(nbordr+2))
        real(kind=8) :: fatsoc
        integer :: ncycl(nbvec)
        real(kind=8) :: vmin(nbvec*(nbordr+2))
        real(kind=8) :: vmax(nbvec*(nbordr+2))
        integer :: omin(nbvec*(nbordr+2))
        integer :: omax(nbvec*(nbordr+2))
    end subroutine avrain
end interface
