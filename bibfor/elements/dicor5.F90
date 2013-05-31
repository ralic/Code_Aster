subroutine dicor5(k0, sim, p1, pi, ui,&
                  ti, dxu1, dxu2, dryu1, dryu2,&
                  nu1, nu2, mu1, mu2, c1,&
                  dbar2, uu, tt, dur, dryr,&
                  dnsdu, dmsdt, dnsdt, dnsdu2, dmsdt2,&
                  dnsdt2, si, varip2, varip3)
! ----------------------------------------------------------------------
! aslint: disable=W1504
    implicit none
    include 'asterfort/dicor3.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: k0(78), sim(12), p1, pi, ui, ti, dxu1, dxu2, dryu1, dryu2
    real(kind=8) :: nu1, nu2, mu1, mu2, c1, dbar2, uu, tt, dur, dryr
    real(kind=8) :: dnsdu, dmsdt, dnsdt, dnsdu2, dmsdt2, dnsdt2
    real(kind=8) :: si(12), varip2, varip3
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       SIM    : EFFORTS GENERALISES PRECEDENTS
!       P1     : VARIABLE INTERNE COURANTE MECANISME 1
!       PI     :
!       UI     :
!       TI     :
!       DXU$   : DEPLACEMENT ULTIME POUR LE MECANISME $ (1 OU 2)
!       DRYU$  : ROTATION ULTIME POUR LE MECANISME $
!       NU$    : EFFORT ULTIME POUR LE MECANISME $
!       MU$    : MOMENT ULTIME POUR LE MECANISME $
!       C1     : PARAMETRE DE NON-LINEARITE DU MECANISME 1
!       DBAR$  : COEFFICIENT DE LA RELATION DU MECANISME $
!       UU     :
!       TT     :
!       DUR    : INCREMENT DE DEPLACEMENT
!       DRYR   : INCREMENT DE ROTATION
!
! OUT : DNSDU  :
!       DMSDT  :
!       DNSDT  :
!       DNSDU2 :
!       DMSDT2 :
!       DNSDT2 :
!       SI     : EFFORTS GENERALISES COURANTS
!       VARIP2 : VARIABLE INTERNE ACTUALISEE
!       VARIP3 : VARIABLE INTERNE ACTUALISEE
!
!**************** DECLARATION DES VARIABLES LOCALES ********************
!
    real(kind=8) :: g2, rg2
    real(kind=8) :: ubr1, ubr2, tbr1, tbr2, ub1, ub2, tb1, tb2, znb1, znb2, zmb1
    real(kind=8) :: zmb2
    real(kind=8) :: feq2, p2, p2b, ur2, tr2, u2, t2, feq1, upi, tpi
!
!************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
! ICI UI=UI-VARIM4 ET TI=TI-VARIM5
!C ON REMPLACE APRES UI PAR UU-DUR ET TI PAR TT-DRYR
    call u2mess('I', 'ELEMENTS_25')
    if (pi .ne. 0.d0) then
        ubr1 = ui/dxu1/pi
        tbr1 = ti/dryu1/pi
    else
        ubr1 = (ui+dur)/dxu1/p1
        tbr1 = (ti+dryr)/dryu1/p1
    endif
    feq1 = sqrt(ubr1**2+tbr1**2)
    ubr1 = ubr1/feq1
    tbr1 = tbr1/feq1
    ub1 = ubr1*dxu1
    tb1 = tbr1*dryu1
    upi = ub1*pi
    tpi = tb1*pi
    znb1 = c1*ubr1
    zmb1 = c1*tbr1
!
    znb2 = znb1*nu1/nu2
    zmb2 = zmb1*mu1/mu2
    feq2 = sqrt(znb2**2+zmb2**2)
    p2b = feq2**2/(1.d0-feq2)/dbar2
    ubr2 = p2b*znb2/feq2
    tbr2 = p2b*zmb2/feq2
    ub2 = ubr2*dxu2
    tb2 = tbr2*dryu2
    u2 = dur-ub1+ub2+upi
    t2 = dryr-tb1+tb2+tpi
    ur2 = u2/dxu2
    tr2 = t2/dryu2
    p2 = sqrt(ur2**2+tr2**2)
    g2 = dbar2*p2
    rg2 = 0.5d0*(-g2+sqrt(g2**2 + 4.d0*g2))
!      RGP2 = (1.D0-RG2)**2/RG2/(2.D0-RG2)
!
    varip2 = p2
    varip3 = 2.d0
!
    si(7) = u2*rg2*nu2/dxu2/p2
    si(11) = t2*rg2*mu2/dryu2/p2
    si(1) = -si(7)
    si(5) = -si(11)
!
    call dicor3(k0, dur, dryr, sim, si,&
                dnsdu, dmsdt, dnsdt)
!
    dnsdu2 = rg2*nu2/dxu2/p2
    if (dur .eq. 0.d0) dnsdu2 = k0(1)
    dmsdt2 = rg2*mu2/dryu2/p2
    if (dryr .eq. 0.d0) dmsdt2 = k0(15)
    dnsdt2 = 0.d0
! ----------------------------------------------------------------------
!
end subroutine
