subroutine ptma01(kanl, itype, m, ist, rho,&
                  e, a1, a2, xl, xiy1,&
                  xiy2, xiz1, xiz2, g, alfay1,&
                  alfay2, alfaz1, alfaz2, ey, ez)
    implicit none
    include 'asterc/r8gaem.h'
    include 'asterfort/fun1.h'
    include 'asterfort/u2mess.h'
    integer :: kanl, ist, itype
    real(kind=8) :: e, rho, a1, a2, xl, xiy1, xiy2, xiz1, xiz2, g
    real(kind=8) :: m(*), alfay1, alfay2, alfaz1, alfaz2, ey, ez
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     CALCUL DE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE
!          - DROIT A SECTION CONSTANTE
!          - DROIT A SECTION VARIABLE
!     PAR LA METHODE
!          - DES MASSES CONCENTREES
!          - DES MASSES EQUIVALENTES
!     ------------------------------------------------------------------
! IN  KANL       - TYPE DE MODELISATION DES MASSES
! IN               KANL = 0 MASSES CONCENTREES FORMULATION S.D.R.C.
! IN               KANL = 1 MASSES COHERENTE
! IN  ITYPE      - TYPE DE VARIATION SECTION DROITE
! OUT M          -(78) MATRICE DE MASSE ELEMENT
! IN  E          - MODULE D ELASTICITE MATERIAU
! IN  RHO        - MASSE VOLUMIQUE DU MATERIAU
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
! IN  EY         - COMPOSANTE TG SUR Y PRINCIPAL
! IN  EZ         - COMPOSANTE TG SUR Z PRINCIPAL
! IN  IST        - TYPE DE STRUCTURE
!
! ======================================================================
!     SOUS - PROGRAMMES UTILISES
!
!BBL  FUN1     - AIRES ET CONSTANTE DE TORSION EQUIVALENTES
! ======================================================================
!
    real(kind=8) :: zaire1, zaire2, zinex1, zinex2
    real(kind=8) :: zaire, zcont, zial, yial, xl2
    real(kind=8) :: asz, asy, phiy, phiz, phiy2, phiz2
    real(kind=8) :: c, xiy, xiz, xjx
    real(kind=8) :: zero
    real(kind=8) :: c001, c002, c003, c004, c005, c006
    real(kind=8) :: c007, c008, c009, c010, c011
    real(kind=8) :: c012, c013, c015, c020, c024
    real(kind=8) :: c030, c035, c040, c048, c060
    real(kind=8) :: c070, c105, c120, c140
    real(kind=8) :: c210, c420
!
    integer :: ip(12), i
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data    ip/0,1,3,6,10,15,21,28,36,45,55,66/
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
    c008 = 8.0d0
    c009 = 9.0d0
    c010 = 10.0d0
    c011 = 11.0d0
    c012 = 12.0d0
    c013 = 13.0d0
    c015 = 15.0d0
    c020 = 20.0d0
    c024 = 24.0d0
    c030 = 30.0d0
    c035 = 35.0d0
    c040 = 40.0d0
    c048 = 48.0d0
    c060 = 60.0d0
    c070 = 70.0d0
    c105 = 105.0d0
    c120 = 120.0d0
    c140 = 140.0d0
    c210 = 210.0d0
    c420 = 420.0d0
!
    if (kanl .eq. 0) then
!     ------------------------------------------------------------------
!               MASSES CONCENTREES FORMULATION S.D.R.C. KANL =0
!     ------------------------------------------------------------------
        if (itype .le. 1) then
            zaire1 = rho*a1*xl / c002
            zaire2 = zaire1
            zinex1 = rho*xl*(xiy1+xiz1)/c002
            zinex2 = zinex1
        else if (itype .eq. 1) then
            zaire1 = rho*(c003*a1+a2)*xl/c008
            zaire2 = rho*(c003*a2+a1)*xl/c008
            zinex1 = (xiy1+xiy2+xiz1+xiz2)*xl*rho / c004
            zinex2 = zinex1
        else
            zaire1 = rho*(c005*a1+a2)*xl/c012
            zaire2 = rho*(c005*a2+a1)*xl/c012
            zinex1 = (xiy1+xiy2+xiz1+xiz2)*xl*rho / c004
            zinex2 = zinex1
        endif
        c = (zaire1 + zaire2 )*xl
        c = min(c*xl/c105,c/c048)
        m(ip( 1)+ 1) = zaire1
        m(ip( 2)+ 2) = zaire1
        m(ip( 3)+ 3) = zaire1
        m(ip( 4)+ 4) = zinex1
        m(ip( 5)+ 5) = c + rho*(xiy1+xiy2)*xl/c015
        m(ip( 6)+ 6) = c + rho*(xiz1+xiz2)*xl/c015
        m(ip( 7)+ 7) = zaire2
        m(ip( 8)+ 8) = zaire2
        m(ip( 9)+ 9) = zaire2
        m(ip(10)+10) = zinex2
        m(ip(11)+11) = m(ip( 5)+ 5)
        m(ip(12)+12) = m(ip( 6)+ 6)
    else
!     ------------------------------------------------------------------
!                       MASSES COHERENTES   KANL = 1
!     ------------------------------------------------------------------
        if (itype .eq. 2) then
            zaire = ( a1 + a2 + sqrt(a1*a2) ) / c003
        else
            zaire = ( a1 + a2) / c002
        endif
        zcont = rho * zaire * xl
        m(ip(1)+ 1) = zcont / c003
        m(ip(7)+ 1) = m(ip(1)+ 1) / c002
        m(ip(7)+ 7) = m(ip(1)+ 1)
!
        if (ist .ne. 2 .and. ist .ne. 5) then
            xl2 = xl * xl
            xiy = ( xiy1 + xiy2 ) / c002
            xiz = ( xiz1 + xiz2 ) / c002
            xjx = xiy + xiz
!
!              CALCUL DES COEFFICIENTS INDUIT PAR LE CISAILLEMENT
            if (alfaz1 .ne. zero .and. alfaz2 .ne. zero .and. alfay1 .ne. zero .and. alfay2&
                .ne. zero) then
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
            phiy2 = phiy * phiy
            phiz2 = phiz * phiz
!
            c = zcont / ( c001 + phiy )**2
            zial = xiz / ( zaire * xl2 )
            m(ip( 2)+ 2) = c * (&
                           c013 / c035 + phiy * c007 / c010 + phiy2 / c003 + zial * c006 / c005)
            m(ip( 6)+ 2) = c * xl *(&
                           c011 / c210 + phiy * c011 / c120 + phiy2 / c024 + zial / c010 - phiy *&
                           & zial / c002&
                           )
            m(ip( 8)+ 2) = c * (&
                           c009 / c070 + phiy * c003 / c010 + phiy2 / c006 - zial * c006 / c005)
            m(ip(12)+ 2) = c * xl *(&
                           - c013 / c420 - phiy*c003 / c040 - phiy2 / c024 + zial / c010 - phiy *&
                           & zial / c002&
                           )
            m(ip( 6)+ 6) = c * xl2 * (&
                           c001 / c105 + phiy / c060 + phiy2 / c120 + zial * (&
                           c002 / c015 + phiy / c006 + phiy2 / c003)&
                           )
            m(ip( 8)+ 6) = - m(ip(12)+ 2)
            m(ip(12)+ 6) = c * xl2 * (&
                           - c001 / c140 - phiy / c060 - phiy2 / c120 - zial * (&
                           c001 / c030 + phiy / c006 - phiy2 / c006)&
                           )
            m(ip( 8)+ 8) = m(ip(2)+ 2)
            m(ip(12)+ 8) = - m(ip(6)+ 2)
            m(ip(12)+12) = m(ip(6)+ 6)
!
            if (ist .ne. 3 .and. ist .ne. 6) then
                c = zcont / ( c001 + phiz )**2
                yial = xiy / ( zaire * xl2 )
                m(ip( 3)+ 3) = c * (&
                               c013 / c035 + phiz * c007 / c010 + phiz2 / c003 + yial * c006 / c0&
                               &05&
                               )
                m(ip( 5)+ 3) = - c * xl * (&
                               c011 / c210 + phiz * c011 / c120 + phiz2 / c024 + yial / c010 - ph&
                               &iz * yial / c002&
                               )
                m(ip( 9)+ 3) = c * (&
                               c009 / c070 + phiz * c003 / c010 + phiz2 / c006 - yial * c006 / c0&
                               &05&
                               )
                m(ip(11)+ 3) = - c * xl * (&
                               - c013 / c420 - phiz * c003 / c040 - phiz2 / c024 + yial / c010 - &
                               &phiz * yial / c002&
                               )
                m(ip( 4)+ 4) = zcont * xjx / ( c003 * zaire)
                m(ip(10)+ 4) = m(ip(4)+ 4) / c002
                m(ip( 5)+ 5) = c * xl2 * (&
                               c001 / c105 + phiz / c060 + phiz2 / c120 + yial * (&
                               c002 / c015 + phiz / c006 + phiz2 / c003)&
                               )
                m(ip( 9)+ 5) = - m(ip(11)+ 3)
                m(ip(11)+ 5) = c * xl2 * (&
                               - c001 / c140 - phiz / c060 - phiz2 / c120 - yial * (&
                               c001 / c030 + phiz / c006 - phiz2 / c006)&
                               )
                m(ip( 9)+ 9) = m(ip(3)+ 3)
                m(ip(11)+ 9) = - m(ip(5)+ 3)
                m(ip(10)+10) = m(ip(4)+ 4)
                m(ip(11)+11) = m(ip(5)+ 5)
!
                if (ez .ne. zero .or. ey .ne. zero) then
!                 --- DANS LE REPERE LIE AU C D TORSION ---
                    m(ip( 4)+ 4) = m(ip( 4)+ 4) + (ez*ez+ey*ey)*zcont/ c003
                    m(ip(10)+10) = m(ip( 4)+ 4)
                    m(ip(10)+ 4) = m(ip(10)+ 4) + (ez*ez+ey*ey)*zcont/ c006
! V TX
                    m(ip( 4)+ 2) = - ez * zcont * c007/c020
                    m(ip(10)+ 8) = m(ip( 4)+ 2)
                    m(ip(10)+ 2) = - ez * zcont * c003/c020
                    m(ip( 8)+ 4) = m(ip(10)+ 2)
! TX TZ
                    m(ip( 6)+ 4) = - ez * zcont *xl/c020
                    m(ip(12)+10) = - m(ip( 6)+ 4)
                    m(ip(12)+ 4) = + ez * zcont *xl/c030
                    m(ip(10)+ 6) = - m(ip(12)+ 4)
! W TX
                    m(ip( 4)+ 3) = - ey * zcont * c007/c020
                    m(ip(10)+ 9) = m(ip( 4)+ 3)
                    m(ip(10)+ 3) = - ey * zcont * c003/c020
                    m(ip( 9)+ 4) = m(ip(10)+ 3)
! TX TY
                    m(ip( 5)+ 4) = + ey * zcont *xl/c020
                    m(ip(11)+10) = - m(ip( 5)+ 4)
                    m(ip(11)+ 4) = - ey * zcont *xl/c030
                    m(ip(10)+ 5) = - m(ip(11)+ 4)
!
!                 --- REPASSAGE DANS LE REPERE LIE AU CDG ---
                    m(ip( 4)+ 4) = m(&
                                   ip( 4)+ 4) + ez * ez * m(ip( 2)+ 2) + ey * ey * m(ip( 3)+ 3) -&
                                   &2 * ez * m(ip( 4)+ 2) +2 * ey * m(ip( 4)+ 3&
                                   )
                    m(ip(10)+ 4) = m(&
                                   ip(10)+ 4) + ez * ez * m(ip( 8)+ 2) + ey * ey * m(ip( 9)+ 3) -&
                                   & ez * m(ip(10)+ 2) + ey * m(ip(10)+ 3) - ez * m(ip( 8)+ 4) + &
                                   &ey * m( ip( 9)+ 4&
                                   )
                    m(ip(10)+10) = m(&
                                   ip(10)+10) + ez * ez * m(ip( 8)+ 8) + ey * ey * m(ip( 9)+ 9) -&
                                   &2 * ez * m(ip(10)+ 8) +2 * ey * m(ip(10)+ 9&
                                   )
                    m(ip( 4)+ 2) = - ez * m(ip( 2)+ 2) + m(ip( 4)+ 2)
                    m(ip(10)+ 2) = - ez * m(ip( 8)+ 2) + m(ip(10)+ 2)
                    m(ip( 4)+ 3) = ey * m(ip( 3)+ 3) + m(ip( 4)+ 3)
                    m(ip(10)+ 3) = ey * m(ip( 9)+ 3) + m(ip(10)+ 3)
                    m(ip( 5)+ 4) = ey * m(ip( 5)+ 3) + m(ip( 5)+ 4)
                    m(ip( 6)+ 4) = - ez * m(ip( 6)+ 2) + m(ip( 6)+ 4)
                    m(ip( 8)+ 4) = - ez * m(ip( 8)+ 2) + m(ip( 8)+ 4)
                    m(ip( 9)+ 4) = ey * m(ip( 9)+ 3) + m(ip( 9)+ 4)
                    m(ip(11)+ 4) = ey * m(ip(11)+ 3) + m(ip(11)+ 4)
                    m(ip(12)+ 4) = - ez * m(ip(12)+ 2) + m(ip(12)+ 4)
                    m(ip(10)+ 5) = ey * m(ip( 9)+ 5) + m(ip(10)+ 5)
                    m(ip(10)+ 6) = - ez * m(ip( 8)+ 6) + m(ip(10)+ 6)
                    m(ip(10)+ 8) = - ez * m(ip( 8)+ 8) + m(ip(10)+ 8)
                    m(ip(10)+ 9) = ey * m(ip( 9)+ 9) + m(ip(10)+ 9)
                    m(ip(11)+10) = ey * m(ip(11)+ 9) + m(ip(11)+10)
                    m(ip(12)+10) = - ez * m(ip(12)+ 8) + m(ip(12)+10)
                endif
            endif
        endif
    endif
end subroutine
