subroutine dicor4(k0, sim, sip, pi, ui,&
                  ti, dxu1, dxu2, dryu1, dryu2,&
                  nu1, nu2, mu1, mu2, feq1,&
                  c1, dbar2, uu, tt, dur,&
                  dryr, p2, utot, ttot, dnsdu,&
                  dmsdt, dnsdt, dnsdu2, dmsdt2, dnsdt2)
! ----------------------------------------------------------------------
! aslint: disable=W1504
    implicit none
#include "asterfort/dicor3.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: k0(78), sim(12), sip(12), pi, ui, ti, dxu1, dxu2, dryu1
    real(kind=8) :: dryu2
    real(kind=8) :: nu1, nu2, mu1, mu2, feq1, c1, dbar2, uu, tt, dur, dryr
    real(kind=8) :: p2, utot, ttot, dnsdu, dmsdt, dnsdt, dnsdu2, dmsdt2, dnsdt2
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
!       SIM    : EFFORTS GENERALISES PRECEDENTS
!       SIP    : EFFORTS GENERALISES COURANTS
!       PI     :
!       UI     :
!       TI     :
!       DXU$   : DEPLACEMENT ULTIME POUR LE MECANISME $ (1 OU 2)
!       DRYU$  : ROTATION ULTIME POUR LE MECANISME $
!       NU$    : EFFORT ULTIME POUR LE MECANISME $
!       MU$    : MOMENT ULTIME POUR LE MECANISME $
!       FEQ1   : FORCE EQUIVALENTE POUR LE MECANISME 1
!       C1     : PARAMETRE DE NON-LINEARITE DU MECANISME 1
!       DBAR2  : COEFFICIENT DE LA RELATION DU MECANISME 2
!       UU     :
!       TT     :
!       DUR    : INCREMENT DE DEPLACEMENT
!       DRYR   : INCREMENT DE ROTATION
!
! OUT : SIP    : EFFORTS GENERALISES COURANTS
!       P2     :
!       UTOT   :
!       TTOT   :
!       DNSDU  :
!       DMSDT  :
!       DNSDT  :
!       DNSDU2 :
!       DMSDT2 :
!       DNSDT2 :
!
!**************** DECLARATION DES VARIABLES LOCALES ********************
!
    real(kind=8) :: ubr1, ubr2, tbr1, tbr2, ub1, ub2, tb1, tb2, znb1, znb2, zmb1
    real(kind=8) :: zmb2
    real(kind=8) :: feq2, p2b, ur2, tr2, u2, t2, upi, tpi
!
!************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
    call u2mess('I', 'ELEMENTS_25')
! ICI UI=UI-VARIM4 ET TI=TI-VARIM5
!C ON REMPLACE APRES UI PAR UU-DUR ET TI PAR TT-DRYR
    if (pi .ne. 0.d0) then
        ubr1 = ui/dxu1/pi
        tbr1 = ti/dryu1/pi
    else
        ubr1 = sip(7)/nu1/feq1
        tbr1 = sip(11)/mu1/feq1
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
    feq2 = sqrt ( (sip(7)/nu2)**2 + (sip(11)/mu2)**2 )
    p2 = feq2**2/(1.d0-feq2)/dbar2
    ur2 = p2*sip(7)/nu2/feq2
    tr2 = p2*sip(11)/mu2/feq2
    u2 = ur2*dxu2
    t2 = tr2*dryu2
!
    utot = u2+ub1-ub2+uu-dur-upi
    ttot = t2+tb1-tb2+tt-dryr-tpi
!
    if (dur .ne. 0.d0) dnsdu2 = sip(7)/utot
    if (dur .eq. 0.d0) dnsdu2 = k0(1)
    if (dryr .ne. 0.d0) dmsdt2 = sip(11)/ttot
    if (dryr .eq. 0.d0) dmsdt2 = k0(15)
    dnsdt2 = 0.d0
!
    sip(7) = dnsdu2*uu
    sip(11) = dmsdt2*tt
    sip(1) = -sip(7)
    sip(5) = -sip(11)
!
    call dicor3(k0, dur, dryr, sim, sip,&
                dnsdu, dmsdt, dnsdt)
! ----------------------------------------------------------------------
!
end subroutine
