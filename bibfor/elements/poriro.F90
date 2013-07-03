subroutine poriro(itype, m, rho, omega, e,&
                  a1, a2, xl, xiy1, xiy2,&
                  xiz1, xiz2, g, alfay1, alfay2,&
                  alfaz1, alfaz2)
    implicit none
#include "asterc/r8gaem.h"
#include "asterfort/fun1.h"
#include "asterfort/u2mess.h"
    integer :: itype
    real(kind=8) :: e, rho, a1, a2, xl, xiy1, xiy2, xiz1, xiz2, g
    real(kind=8) :: m(*), alfay1, alfay2, alfaz1, alfaz2
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     CALCUL DE LA MATRICE DE RAIDEUR CENTRIFUGE DES ELEMENTS DE POUTRE
!          - DROIT A SECTION CONSTANTE
!          - DROIT A SECTION VARIABLE
!     ------------------------------------------------------------------
! IN  ITYPE      - TYPE DE VARIATION SECTION DROITE
! IN  RHO        - MASSE VOLUMIQUE DU MATERIAU
! IN  OMEGA      - VECTEUR ROTATION (EN REPERE LOCAL)
! IN  E          - MODULE D ELASTICITE MATERIAU
! IN  A1         - AIRE DE LA SECTION DROITE INITIALE
! IN  A2         - AIRE DE LA SECTION DROITE FINALE
! IN  XL         - LONGUEUR DE L ELEMENT
! IN  XIY1       - MOMENT D INERTIE / Y PRINCIPAL  SECTION INITIALE
! IN  XIY2       - MOMENT D INERTIE / Y PRINCIPAL  SECTION FINALE
! IN  XIZ1       - MOMENT D INERTIE / Z PRINCIPAL  SECTION INITIALE
! IN  XIZ2       - MOMENT D INERTIE / Z PRINCIPAL  SECTION FINALE
! IN  G          - MODULE DE CISAILLEMENT DU MATERIAU
! IN  ALFAY1     - COEFFICIENT DE CISAILLEMENT AXE Y SECTION INITIALE
! IN  ALFAY2     - COEFFICIENT DE CISAILLEMENT AXE Y SECTION FINALE
! IN  ALFAZ1     - COEFFICIENT DE CISAILLEMENT AXE Z SECTION INITIALE
! IN  ALFAZ2     - COEFFICIENT DE CISAILLEMENT AXE Z SECTION FINALE
!
! OUT M          -(78) MATRICE DE RAIDEUR CENTRIFUGE ELEMENTAIRE
! ======================================================================
!     SOUS - PROGRAMMES UTILISES
!
!BBL  FUN1     - AIRES ET CONSTANTE DE TORSION EQUIVALENTES
! ======================================================================
!
    real(kind=8) :: zaire1, zaire2, zaire, zcont, xl2
    real(kind=8) :: asz, asy, phiy, phiz, phiy2, phiz2, omega(3)
    real(kind=8) :: omx, omy, omz, omxy, omxz, omyz, omx2y2, omx2z2, omy2z2
    real(kind=8) :: c, xiy, xiz, zero
    real(kind=8) :: c001, c002, c003, c004, c005, c006, c007, c009
    real(kind=8) :: c010, c011, c012, c013, c020, c021, c024, c035
    real(kind=8) :: c040, c060, c070, c105, c120, c140, c210, c420
    real(kind=8) :: c240
!
    integer :: ip(13), i
!-----------------------------------------------------------------------
    real(kind=8) :: phiqy, phiqz, phis, phiyz, qy, qz
!-----------------------------------------------------------------------
    data    ip/0,1,3,6,10,15,21,28,36,45,55,66,78/
!
!     INITIALISATION
    zero = 0.0d0
    do 10 i = 1, 78
        m(i) = zero
10  end do
!
!     -- SI G  ET E SONT NULS : ON FAIT G=1.
    if (abs(g) .lt. 1.0d0/r8gaem()) then
        if (abs(e) .lt. 1.0d0/r8gaem()) then
            g = 1.0d0
        else
            call u2mess('F', 'ELEMENTS2_54')
        endif
    endif
!
    c001 = 1.0d0
    c002 = 2.0d0
    c003 = 3.0d0
    c004 = 4.0d0
    c005 = 5.0d0
    c006 = 6.0d0
    c007 = 7.0d0
    c009 = 9.0d0
    c010 = 10.0d0
    c011 = 11.0d0
    c012 = 12.0d0
    c013 = 13.0d0
    c020 = 20.0d0
    c021 = 21.0d0
    c024 = 24.0d0
    c035 = 35.0d0
    c040 = 40.0d0
    c060 = 60.0d0
    c070 = 70.0d0
    c105 = 105.0d0
    c120 = 120.0d0
    c140 = 140.0d0
    c210 = 210.0d0
    c240 = 240.0d0
    c420 = 420.0d0
!
    if (itype .eq. 2) then
        zaire = ( a1 + a2 + sqrt(a1*a2) ) / c003
    else
        zaire = ( a1 + a2) / c002
    endif
    zcont = - rho * zaire * xl
!
    xl2 = xl * xl
    xiy = ( xiy1 + xiy2 ) / c002
    xiz = ( xiz1 + xiz2 ) / c002
!
!        CALCUL DES COEFFICIENTS INDUIT PAR LE CISAILLEMENT
    if (alfaz1 .ne. zero .and. alfaz2 .ne. zero .and. alfay1 .ne. zero .and. alfay2 .ne.&
        zero) then
!                    1/ AIRE REDUITE EN Y
        zaire1 = a1 / alfaz1
        zaire2 = a2 / alfaz2
        call fun1(asy, zaire1, zaire2, itype)
!                    2/ AIRE REDUITE EN Z
        zaire1 = a1 / alfay1
        zaire2 = a2 / alfay2
        call fun1(asz, zaire1, zaire2, itype)
        phiy = ( c012 * e * xiz ) / (g * asz * xl2 )
        phiz = ( c012 * e * xiy ) / (g * asy * xl2 )
    else
!              EULER SANS INERTIE DE ROTATION
        phiy = zero
        phiz = zero
        xiy = zero
        xiz = zero
    endif
    omx = omega(1)
    omy = omega(2)
    omz = omega(3)
    omxy = omx*omy
    omxz = omx*omz
    omyz = omz*omy
    omx2y2 = omx*omx + omy*omy
    omy2z2 = omy*omy + omz*omz
    omx2z2 = omx*omx + omz*omz
!
    phiy2 = phiy * phiy
    phiz2 = phiz * phiz
!
    m(ip(1)+ 1) = zcont * omy2z2/ c003
    m(ip(7)+ 1) = m(ip(1)+ 1) / c002
    m(ip(7)+ 7) = m(ip(1)+ 1)
!
!  TERMES DEDUITS DE LA MATRICE DE MASSE :
!
    c = zcont / ( c001 + phiy )**2
    m(ip( 2)+ 2) = omx2z2 * c * ( c013 / c035 + phiy * c007 / c010 + phiy2 / c003)
    m(ip( 6)+ 2) = - omx2z2 * c * xl *( c011 / c210 + phiy * c011 / c120 + phiy2 / c024)
    m(ip( 8)+ 2) = omx2z2 * c * ( c009 / c070 + phiy * c003 / c010 + phiy2 / c006)
    m(ip(12)+ 2) = - omx2z2 * c * xl *( - c013 / c420 - phiy*c003 / c040 - phiy2 / c024)
    m(ip( 6)+ 6) = omx2z2 * c * xl * (c001 / c105 + phiy / c060 + phiy2 / c120)
    m(ip( 8)+ 6) = - m(ip(12)+ 2)
    m(ip(12)+ 6) = omx2z2 * c * xl2 * ( - c001 / c140 - phiy / c060 - phiy2 / c120)
    m(ip( 8)+ 8) = m(ip(2)+ 2)
    m(ip(12)+ 8) = - m(ip(6)+ 2)
    m(ip(12)+12) = m(ip(6)+ 6)
!
    c = zcont / ( c001 + phiz )**2
    m(ip( 3)+ 3) = omx2y2 * c * ( c013 / c035 + phiz * c007 / c010 + phiz2 / c003)
    m(ip( 5)+ 3) = - omx2y2 * c * xl * ( c011 / c210 + phiz * c011 / c120 + phiz2 / c024)
    m(ip( 9)+ 3) = omx2y2 * c * ( c009 / c070 + phiz * c003 / c010 + phiz2 / c006)
    m(ip(11)+ 3) = - omx2y2 * c * xl * ( - c013 / c420 - phiz * c003 / c040 - phiz2 / c024)
    m(ip( 5)+ 5) = omx2y2 * c * xl * (c001 / c105 + phiz / c060 + phiz2 / c120)
    m(ip( 9)+ 5) = - m(ip(11)+ 3)
    m(ip(11)+ 5) = omx2y2 * c * xl2 * ( - c001 / c140 - phiz / c060 - phiz2 / c120)
    m(ip( 9)+ 9) = m(ip(3)+ 3)
    m(ip(11)+ 9) = - m(ip(5)+ 3)
    m(ip(11)+11) = m(ip(5)+ 5)
!
!  TERMES EN PLUS PAR RAPPORT A LA MATRICE DE MASSE :
!
    qy = c001/(c001 + phiy)
    qz = c001/(c001 + phiz)
    phiqy = phiy*qy
    phiqz = phiz*qz
    phiyz = phiy * phiz
    phis = phiy + phiz
!
    c = zcont
    m(ip( 2)+ 1) = - omxy * c * (c009 + phiqy)/c060
    m(ip( 3)+ 1) = - omxz * c * (c009 + phiqz)/c060
    m(ip( 5)+ 1) = - omxz * c * xl * (-c004 - phiqz)/c120
    m(ip( 6)+ 1) = omxy * c * xl * (-c004 - phiqy)/c120
    m(ip( 8)+ 1) = - omxy * c * (c020 + qy)/c060
    m(ip( 9)+ 1) = - omxz * c * (c020 + qz)/c060
    m(ip( 11)+ 1) = - omxz * c * xl * (c005 + qz)/c120
    m(ip( 12)+ 1) = omxy * c * xl * (c005 + qy)/c120
!
! ======================================================================
    m(ip( 3)+ 2) = - omyz * c * qy * qz * ( c013/c035 + phis * c007/c020 + phiyz/c003)
    m(ip( 5)+ 2) = omyz * c * xl * qy * qz * (&
                   c011/c210 + (phiy * c012 + phiz * c010)/c240 + phiyz/c024)
    m(ip( 7)+ 2) = - omxy * c * (c021- phiqy) / c060
    m(ip( 9)+ 2) = - omyz * c * qy * qz * ( c009/c070 + phis * c003/c020 + phiyz/c006)
    m(ip( 11)+ 2) = omyz * c * xl * qy * qz * (&
                    c013/c420 + (phiy * c004 + phiz * c005)/c120 + phiyz/c024)
    m(ip( 6)+ 3) = - omyz * c * xl * qy * qz * (&
                   c011/c210 + (phiy * c010 + phiz * c012)/c240 + phiyz/c024)
    m(ip( 7)+ 3) = - omxz * c * (c021 - phiqz) / c060
    m(ip( 8)+ 3) = m(ip( 9)+ 2)
    m(ip( 12)+ 3) = omyz * c * xl * qy * qz * (&
                    c013/c420 + (phiy * c005 + phiz * c004)/c120 + phiyz/c024)
    m(ip( 6)+ 5) = omyz * c * xl * qy * qz * (c001/c105 + (phis + phiyz) /c120)
    m(ip( 7)+ 5) = omxz * c * xl * (c006 - phiqz) / c120
    m(ip( 8)+ 5) = m(ip( 12)+ 3)
    m(ip( 12)+ 5) = - omyz * c * xl2 * qy * qz * ( c001/c140 + (phis + phiyz) /c120)
    m(ip( 7)+ 6) = - omxy * c * xl * (c006 - phiqy) / c120
    m(ip( 9)+ 6) = m(ip( 11)+ 2)
    m(ip( 11)+ 6) = m(ip( 12)+ 5)
!
!
    m(ip( 8)+ 7) = - omxy * c * (c010 - qy)/c060
    m(ip( 9)+ 7) = - omxz * c * (c010 - qz)/c060
    m(ip( 11)+ 7) = - omxz * c * xl * (c005 - qz)/c120
    m(ip( 12)+ 7) = omxz * c * xl * (c005 - qy)/c120
    m(ip( 9)+ 8) = m(ip( 3)+ 2)
    m(ip( 11)+ 8) = - m(ip( 5)+ 2)
    m(ip( 12)+ 9) = - m(ip( 6)+ 3)
    m(ip( 12)+ 11)=    m(ip( 6)+ 5)
end subroutine
