subroutine pmfm21(kanl, m, casrho, casece, a, &
                  xl, xiy, xiz, g, alfay,&
                  alfaz, ey, ez )
    implicit none
#include "asterc/r8gaem.h"
#include "asterfort/utmess.h"
    integer :: kanl
    real(kind=8) :: casece(6),  a, xl, xiy, xiz, g
    real(kind=8) :: m(*), alfay, alfaz,  ey, ez, casrho(6)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCUL DE LA MATRICE DE MASSE DES ELEMENTS POU_D_TGM
!          - DROIT A SECTION CONSTANTE
!     PAR LA METHODE
!          - DES MASSES CONCENTREES
!          - DES MASSES EQUIVALENTES
!     ------------------------------------------------------------------
! IN  KANL       - TYPE DE MODELISATION DES MASSES
! IN               KANL = 0 MASSES CONCENTREES FORMULATION S.D.R.C.
! IN               KANL = 1 MASSES COHERENTE
! OUT M          -(78) MATRICE DE MASSE ELEMENT
! IN  E          - MODULE D ELASTICITE MATERIAU
! IN  RHO        - MASSE VOLUMIQUE DU MATERIAU
! IN  A         - AIRE DE LA SECTION DROITE
! IN  XL         - LONGUEUR DE L ELEMENT
! IN  XIY       - MOMENT D INERTIE / Y PRINCIPAL
! IN  XIZ       - MOMENT D INERTIE / Z PRINCIPAL
! IN  G          - MODULE DE CISAILLEMENT DU MATERIAU
! IN  ALFAY     - COEFFICIENT DE CISAILLEMENT AXE Y
! IN  ALFAZ     - COEFFICIENT DE CISAILLEMENT AXE Z
! IN  EY         - COMPOSANTE TG SUR Y PRINCIPAL
! IN  EZ         - COMPOSANTE TG SUR Z PRINCIPAL
!
!
    real(kind=8) :: zaire, zinex
    real(kind=8) :: zcont, zial, yial, xl2
    real(kind=8) :: asz, asy, phiy, phiz, phiy2, phiz2
    real(kind=8) :: c, xjx
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
    do i = 1, 78
        m(i) = zero
    end do
!
!     -- SI G  ET E SONT NULS : ON FAIT G=1.
    if (abs(g) .lt. 1.0d0/r8gaem()) then
        if (abs(casece(1)) .lt. 1.0d0/r8gaem()) then
            g = 1.0d0
        else
            call utmess('F', 'ELEMENTS2_54')
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
        zaire = casrho(1)*xl
        zinex = (casrho(4)+casrho(5))*xl/c002

        c = zaire*xl
        c = min(c*xl/c105,c/c048)
        m(ip( 1)+ 1) = zaire/c002
        m(ip( 2)+ 2) = zaire/c002
        m(ip( 3)+ 3) = zaire/c002
        m(ip( 4)+ 4) = zinex
        m(ip( 5)+ 5) = c + (casrho(4)+casrho(5))*xl/c015
        m(ip( 6)+ 6) = c + (casrho(4)+casrho(5))*xl/c015
        m(ip( 7)+ 7) = zaire/c002
        m(ip( 8)+ 8) = zaire/c002
        m(ip( 9)+ 9) = zaire/c002
        m(ip(10)+10) = zinex
        m(ip(11)+11) = m(ip( 5)+ 5)
        m(ip(12)+12) = m(ip( 6)+ 6)
    else
!     ------------------------------------------------------------------
!                       MASSES COHERENTES   KANL = 1
!     ------------------------------------------------------------------
        zaire = a
        zcont = casrho(1) * xl
        m(ip(1)+ 1) = zcont / c003
        m(ip(7)+ 1) = m(ip(1)+ 1) / c002
        m(ip(7)+ 7) = m(ip(1)+ 1)
!
        xl2 = xl * xl
        xjx = xiy + xiz
!
!       CALCUL DES COEFFICIENTS INDUIT PAR LE CISAILLEMENT
!       1/ AIRE REDUITE EN Y
        asy = a / alfaz
!       2/ AIRE REDUITE EN Z
        asz = a / alfay
        phiy = ( c012 * casece(4) ) / (g * asz * xl2 )
        phiz = ( c012 * casece(5) ) / (g * asy * xl2 )
!
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
!       --- DANS LE REPERE LIE AU C D TORSION ---
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
end subroutine
