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
    subroutine dicor4(k0, sim, sip, pi, ui,&
                      ti, dxu1, dxu2, dryu1, dryu2,&
                      nu1, nu2, mu1, mu2, feq1,&
                      c1, dbar2, uu, tt, dur,&
                      dryr, p2, utot, ttot, dnsdu,&
                      dmsdt, dnsdt, dnsdu2, dmsdt2, dnsdt2)
        real(kind=8) :: k0(78)
        real(kind=8) :: sim(12)
        real(kind=8) :: sip(12)
        real(kind=8) :: pi
        real(kind=8) :: ui
        real(kind=8) :: ti
        real(kind=8) :: dxu1
        real(kind=8) :: dxu2
        real(kind=8) :: dryu1
        real(kind=8) :: dryu2
        real(kind=8) :: nu1
        real(kind=8) :: nu2
        real(kind=8) :: mu1
        real(kind=8) :: mu2
        real(kind=8) :: feq1
        real(kind=8) :: c1
        real(kind=8) :: dbar2
        real(kind=8) :: uu
        real(kind=8) :: tt
        real(kind=8) :: dur
        real(kind=8) :: dryr
        real(kind=8) :: p2
        real(kind=8) :: utot
        real(kind=8) :: ttot
        real(kind=8) :: dnsdu
        real(kind=8) :: dmsdt
        real(kind=8) :: dnsdt
        real(kind=8) :: dnsdu2
        real(kind=8) :: dmsdt2
        real(kind=8) :: dnsdt2
    end subroutine dicor4
end interface
