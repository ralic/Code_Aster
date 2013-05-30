subroutine dicor2(k0, p1, p2, dur, dryr,&
                  dxu, dryu, feq, nu, mu,&
                  uu, tt, si1, dnsdu, dmsdt,&
                  dnsdt, varip1, varip2, si2)
! ----------------------------------------------------------------------
    implicit none
    real(kind=8) :: k0(78), p1, p2, dur, dryr, dxu, dryu, feq, nu, mu, uu, tt
    real(kind=8) :: si1(12)
    real(kind=8) :: dnsdu, dmsdt, dnsdt, varip1, varip2, si2(12)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     UTILITAIRE POUR LE COMPORTEMENT CORNIERE.
!
! ----------------------------------------------------------------------
!
! IN  : K0     : COEFFICIENTS DE RAIDEUR TANGENTE
!       P1     : VARIABLE INTERNE
!       P2     : VARIABLE INTERNE
!       DUR    : INCREMENT DE DEPLACEMENT
!       DRYR   : INCREMENT DE ROTATION
!       DXU    :
!       DRYU   :
!       FEQ    : FORCE EQUIVALENTE
!       NU     : EFFORT LIMITE ULTIME
!       MU     : MOMENT LIMITE ULTIME
!       UU     :
!       TT     :
!       SI1    : EFFORTS GENERALISES PRECEDENTS
!
! OUT : DNSDU  :
!       DMSDT  :
!       DNSDT  :
!       VARIP1 : VARIABLE INTERNE
!       VARIP2 : VARIABLE INTERNE
!       SI2    : EFFORTS GENERALISES COURANTS
!
! ----------------------------------------------------------------------
    varip1 = p1
    varip2 = 1.d0
    dnsdu = feq*nu/dxu/p2
    if (dur .eq. 0.d0) dnsdu = k0(1)
    dmsdt = feq*mu/dryu/p2
    if (dryr .eq. 0.d0) dmsdt = k0(15)
    dnsdt = 0.d0
    si2(7) = dnsdu*uu
    si2(11) = dmsdt*tt
    si2(1) = -si1(7)
    si2(5) = -si1(11)
! ----------------------------------------------------------------------
!
end subroutine
