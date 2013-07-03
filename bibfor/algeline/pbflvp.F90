subroutine pbflvp(umoy, hmoy, rmoy, cf0, mcf0,&
                  rkip, s1, s2, lambda)
    implicit none
!-----------------------------------------------------------------------
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
! RESOLUTION DU PROBLEME FLUIDE INSTATIONNAIRE : DETERMINATION DES
! VALEURS PROPRES DE L'OPERATEUR DIFFERENTIEL
! APPELANT : PBFLUI
!-----------------------------------------------------------------------
!  IN : UMOY   : VITESSE DE L'ECOULEMENT MOYEN
!  IN : HMOY   : JEU ANNULAIRE MOYEN
!  IN : RMOY   : RAYON MOYEN
!  IN : CF0    : COEFFICIENT DE FROTTEMENT VISQUEUX
!  IN : MCF0   : EXPOSANT VIS-A-VIS DU NOMBRE DE REYNOLDS
!  IN : RKIP   : ORDRE DE COQUE DU MODE CONSIDERE, PONDERE PAR LA VALEUR
!                MOYENNE DU PROFIL DE PRESSION
!  IN : S1     : PARTIE REELLE     DE LA FREQUENCE COMPLEXE
!  IN : S2     : PARTIE IMAGINAIRE DE LA FREQUENCE COMPLEXE
! OUT : LAMBDA : VALEURS PROPRES DE L'OPERATEUR DIFFERENTIEL
!-----------------------------------------------------------------------
!
#include "asterc/r8pi.h"
#include "asterfort/dcabs2.h"
#include "asterfort/dcargu.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: umoy, hmoy, rmoy, cf0, mcf0, rkip, s1, s2
    complex(kind=8) :: lambda(3)
!
    real(kind=8) :: arg, modul, pi, eps
    complex(kind=8) :: a, b, c, z1, z2, z3, p, q, r, s, delta, alpha, beta, n3
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    pi = r8pi()
    eps = 1.d-4
    s = dcmplx(s1,s2)
!
    a = s/umoy + dcmplx(cf0/hmoy)
    b = dcmplx(-1.d0*((rkip/rmoy)**2))
    c = s/umoy + (mcf0+2.d0) * dcmplx(cf0/hmoy)
    c = -1.d0*((rkip/rmoy)**2)*c
    p = b - (a**2)/3.d0
    q = 2.d0*(a**3)/27.d0 - a*b/3.d0 + c
    r = (q/2.d0)**2 + (p/3.d0)**3
    r = r**0.5d0
    if (dcabs2(r) .lt. eps) then
        call u2mess('A', 'ALGELINE3_19')
    endif
!
    delta = ((3.d0*q/p)**2) + 4.d0*p/3.d0
    delta = delta**(0.5d0)
    alpha = 0.5d0*(-3.d0*q/p + delta)
    beta = 0.5d0*(-3.d0*q/p - delta)
    n3 = alpha/beta
!
    modul = (dcabs2(n3))**(1.d0/3.d0)
    arg = dcargu(n3)/3.d0
    z1 = modul * dcmplx(dble(cos(arg)),dble(sin(arg)))
    arg = dcargu(n3)/3.d0 + 2.d0*pi/3.d0
    z2 = modul * dcmplx(dble(cos(arg)),dble(sin(arg)))
    arg = dcargu(n3)/3.d0 + 4.d0*pi/3.d0
    z3 = modul * dcmplx(dble(cos(arg)),dble(sin(arg)))
!
    lambda(1) = ((beta*z1-alpha)/(z1-dcmplx(1.d0)))-a/3.d0
    lambda(2) = ((beta*z2-alpha)/(z2-dcmplx(1.d0)))-a/3.d0
    lambda(3) = ((beta*z3-alpha)/(z3-dcmplx(1.d0)))-a/3.d0
!
    do 10 i = 1, 3
        if (dcabs2(lambda(i)) .lt. eps) then
            write(k1bid,'(I1)') i
            call u2mesk('A', 'ALGELINE3_20', 1, k1bid)
        endif
10  end do
!
end subroutine
