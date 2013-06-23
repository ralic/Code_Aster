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
    subroutine prep2(ndim, npg, g, rpa, etdpn1,&
                     sigm, jm, fda, rp, rpat,&
                     etdm, etdv, sigmam, rpt, epsm,&
                     epsmm)
        integer :: npg
        integer :: ndim
        integer :: g
        real(kind=8) :: rpa(3, 3)
        real(kind=8) :: etdpn1(3, 3)
        real(kind=8) :: sigm(2*ndim, npg)
        real(kind=8) :: jm
        real(kind=8) :: fda(3, 3)
        real(kind=8) :: rp(3, 3)
        real(kind=8) :: rpat(3, 3)
        real(kind=8) :: etdm(3, 3)
        real(kind=8) :: etdv(6)
        real(kind=8) :: sigmam(6)
        real(kind=8) :: rpt(3, 3)
        real(kind=8) :: epsm(6)
        real(kind=8) :: epsmm(6)
    end subroutine prep2
end interface
