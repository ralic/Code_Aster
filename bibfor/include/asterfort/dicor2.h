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
    subroutine dicor2(k0, p1, p2, dur, dryr,&
                      dxu, dryu, feq, nu, mu,&
                      uu, tt, si1, dnsdu, dmsdt,&
                      dnsdt, varip1, varip2, si2)
        real(kind=8) :: k0(78)
        real(kind=8) :: p1
        real(kind=8) :: p2
        real(kind=8) :: dur
        real(kind=8) :: dryr
        real(kind=8) :: dxu
        real(kind=8) :: dryu
        real(kind=8) :: feq
        real(kind=8) :: nu
        real(kind=8) :: mu
        real(kind=8) :: uu
        real(kind=8) :: tt
        real(kind=8) :: si1(12)
        real(kind=8) :: dnsdu
        real(kind=8) :: dmsdt
        real(kind=8) :: dnsdt
        real(kind=8) :: varip1
        real(kind=8) :: varip2
        real(kind=8) :: si2(12)
    end subroutine dicor2
end interface
