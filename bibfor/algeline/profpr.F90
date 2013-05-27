subroutine profpr(icoq, rki, r1, r2, coepr1,&
                  coepr2, wpr)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! CALCUL DE COEFFICIENTS PONDERATEURS QUAND ON PREND EN COMPTE UN PROFIL
! RADIAL NON UNIFORME POUR LA PRESSION
! APPELANT : BIJSOM, PBFLUI
!-----------------------------------------------------------------------
!  IN : ICOQ   : INDICE CARACTERISANT LA COQUE EN MOUVEMENT POUR LE MODE
!                CONSIDERE (ICOQ=1 COQUE INTERNE  ICOQ=2 COQUE EXTERNE)
!  IN : RKI    : ORDRE DE COQUE DU MODE CONSIDERE
!  IN : R1     : RAYON REPERANT LA SURFACE DE LA STRUCTURE INTERNE
!  IN : R2     : RAYON REPERANT LA SURFACE DE LA STRUCTURE EXTERNE
! OUT : COEPR1 : COEFFICIENT PONDERATEUR POUR LA PRESSION EN R1
! OUT : COEPR2 : COEFFICIENT PONDERATEUR POUR LA PRESSION EN R2
! OUT : WPR    : VALEUR MOYENNE DU PROFIL DE PRESSION
!-----------------------------------------------------------------------
    integer :: icoq
    real(kind=8) :: rki, r1, r2, coepr1, coepr2, wpr
!-----------------------------------------------------------------------
!
! --- 1.INITIALISATIONS
!
!-----------------------------------------------------------------------
    integer :: ki
    real(kind=8) :: a, h0, r0, rc, t, x, y
    real(kind=8) :: z, z1, z2
!-----------------------------------------------------------------------
    r0 = (r1+r2)/2.d0
    h0 = r2 - r1
!
    rc = r2
    if (icoq .eq. 2) rc = r1
!
    ki = int(rki)
!
! --- 2.CALCUL DES COEFFICIENTS PONDERATEURS
!
    if (ki .eq. 1) then
        x = r0 + rc*rc*dble(log(r2/r1))/h0
        a = 1.d0/x
    else
        x = r2**(rki+1.d0) - r1**(rki+1.d0)
        x = x/(rki+1.d0)
        y = rc**(rki)
        y = y*y/(rki-1.d0)
        z1= r1**(rki-1.d0)
        z2= r2**(rki-1.d0)
        z = 1.d0/z1 - 1.d0/z2
        a = h0/(x+y*z)
    endif
!
    if (icoq .eq. 1) then
        t = (r2/r1)**(rki)
        coepr1 = a*(1.d0+t*t)*r1**(rki)
        coepr2 = 2.d0*a*r2**(rki)
    else
        t = (r1/r2)**(rki)
        coepr1 = 2.d0*a*r1**(rki)
        coepr2 = a*(1.d0+t*t)*r2**(rki)
    endif
!
! --- 3.CALCUL DE LA VALEUR MOYENNE
!
    if (ki .eq. 2) then
        x = (r1*r1+r2*r2)/2.d0
        y = rc*rc
        y = y*y*dble(log(r2/r1))/(r0*h0)
        wpr = a*(x+y)
    else
        x = r2**(rki+2.d0) - r1**(rki+2.d0)
        x = x/(rki+2.d0)
        y = rc**(rki)
        y = y*y/(rki-2.d0)
        z1= r1**(rki-2.d0)
        z2= r2**(rki-2.d0)
        z = 1.d0/z1 - 1.d0/z2
        wpr = a*(x+y*z)/(r0*h0)
    endif
!
end subroutine
