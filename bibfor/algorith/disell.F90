subroutine disell(pz, az, bz, h)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
    implicit none
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
    real(kind=8) :: pz(2), az, bz, h
!
!      CALCUL DE H : DISTANCE SIGNEE ENTRE LE POINT P ET L'ELLIPSE
!      DE CENTRE (0,0), DE DEMI-GRAND AXE A ET DE DEMI-PETIT AXE B
!
! IN  PZ     : POINT DU PLAN DONT ON CHERCHE LA DISTANCE A L'ELLIPSE
! IN  AZ     : DEMI-GRAND AXE DE L'ELLIPSE (SUIVANT L'AXE X)
! IN  BZ     : DEMI-PETIT AXE DE L'ELLIPSE (SUIVANT L'AXE Y)
! OUT H      : DISTANCE SIGNEE ENTRE LE POINT P ET L'ELLIPSE
!
!
!
!
    integer :: iter, nitmx
    real(kind=8) :: a, b, p(2)
    real(kind=8) :: eps, epsc, ba, r, z, cosx, sinx, t, a0, a1, a2, a3, a4, k
    real(kind=8) :: phi, qq, rr, dd
    real(kind=8) :: tt, phit, theta, dphi, dr, dz, rac, temp
    parameter    (eps  = 1.d-6)
    parameter    (epsc = 1.d-6)
    parameter    (nitmx=100)
    logical :: linsid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     COPIE LOCALE DES ARGUMENTS D'ENTREE
!     SINON ON RISQUE DE LES MODIFIER
    a=az
    b=bz
    p(1)=pz(1)
    p(2)=pz(2)
!
!     ON NOTE R ET Z LES COORDONNES DANS LE PLAN POUR ETRE EN
!     CONFORMITE AVEC LES NOTATIONS DU PAPIER DE RÉFÉRENCE
!     QUE L'ON PEUT TROUVER EN PIECE JOINTE DE LA FICHE 10385
!
!     CHOIX DU SIGNE DE LA DISTANCE H :
!     H EST NEGATIF A L'INTERIEUR DE L'ELLIPSE
!     H EST POSITIF A L'EXTERIEUR DE L'ELLIPSE
!
!     VERIFICATIONS
    ASSERT(a.gt.0.d0 .and. b.gt.0.d0)
    if (a .lt. b) then
!       SI A EST PLUS PETIT QUE B, ON INVERSE A ET B
!       ET AUSSI LES COORDONNÉES DU POINT P
        temp = a
        a = b
        b = temp
        temp = p(1)
        p(1) = p(2)
        p(2) = temp
    endif
!
!     DEFINITION DE QUELQUES VARIABLES UTILES
!     ---------------------------------------
!
!     ABSCISSE (TOUJOURS POSITIVE) ET ORDONNEE DU POINT P
    r=abs(p(1))
    z=p(2)
!
!     RAPPORT B/A = (1-F)
    ba=b/a
    ASSERT(ba.le.1.d0)
!
!     TRAITEMENT DU CAS PARTICULIER : POINT = CENTRE O
    if (sqrt(r**2+z**2) .lt. eps*a) then
        h = -b
        goto 9999
    endif
!
!
!     ITERATION 0 (= PREDICTION)
!     --------------------------
!
!     TEST SI LE POINT EST A L'INTERIEUR DE L'ELLIPSE
    linsid = ba**2*(r**2-a**2)+z**2.le.0.d0
    cosx = r/sqrt(r**2+z**2)
    sinx = z/sqrt(r**2+z**2)
    t = z/(r+sqrt(r**2+z**2))
!
!     DISTANCE A L'ELLIPSE LE LONG DE LA LIGNE COURRANTE :
!     PLUS PETITE RACINE DU TRINOME A2.K²-2A1.K+A0=0
    a2 = ba**2*cosx**2+sinx**2
    a1 = ba**2*r*cosx+z*sinx
    a0 = ba**2*(r**2-a**2)+z**2
    k = a0/(a1+sqrt(a1**2-a2*a0))
    phi = atan2(z-k*sinx,ba**2*(r-k*cosx))
!
!     SI LA PREDICTION EST LA SOLUTION (CAS DU CERCLE PAR EX), ON SORT
    if (abs(k) .lt. eps*sqrt(r**2+z**2)) then
        h = k
        goto 9999
    endif
!
!
!     BOUCLE PRINCIPALE
!     -----------------
!
    do 10 iter = 1, nitmx
!
!       POLYNOME NORMALISE D'ORDRE 4 DECRIVANT LES INTERSECTIONS
!       ENTRE LE CERCLE ET L'ELLIPSE
!       TAU^4+A3.TAU^3+A2.TAU^2+A1.TAU+A0 = 0
        a4 = ba**2*((r+k)**2-a**2)+z**2
        a3 = -4.d0*k*z/a4
        a2 = 2.d0*(ba**2*(r**2-k**2-a**2)+2.d0*k**2+z**2)/a4
        a1 = a3
!
!       REDUCTION DU POLYNOME EN DEGRE 3 PAR SUBSTITUTION DE LA
!       RACINE CONNUE (T)
        a3 = a3 + t
        a2 = a2 + t*a3
        a1 = a1 + t*a2
!
!       RECHERCHE DES AUTRES RACINES REELLES
        qq = (3.d0*a2-a3**2)/9.d0
        rr = (a3*(9.d0*a2-2.d0*a3**2)-27.d0*a1)/54.d0
        dd = qq**3+rr**2
!
        if (dd .ge. 0.d0) then
!
            tt = sign(&
                 1.d0, rr+sqrt(dd))*(abs(rr+sqrt(dd))**(1.d0/3.d0) ) +sign(1.d0,&
                 rr-sqrt(dd))*(abs(rr-sqrt(dd))**(1.d0/3.d0)&
                 ) -a3/3.d0
            phit = atan2( z*(1.d0+tt**2)-2.d0*k*tt, ba**2*(r*(1.d0+tt** 2) -k*(1.d0-tt**2)) )
        else
            qq = -qq
            theta = trigom('ACOS', rr/(qq*sqrt(qq)))
            tt = 2.d0*sqrt(qq)*cos(theta/3.d0)-a3/3.d0
            phit = atan2( z*(1.d0+tt**2)-2.d0*k*tt, ba**2*(r*(1.d0+tt** 2) -k*(1.d0-tt**2)) )
            if (phit*phi .lt. 0.d0) then
                tt = 2.d0*sqrt(qq)*cos((theta+r8depi())/3.d0)-a3/3.d0
                phit = atan2( z*(1.d0+tt**2)-2.d0*k*tt, ba**2*(r*(1.d0+ tt**2) -k*(1.d0-tt**2)) )
                if (phit*phi .lt. 0.d0) then
                    tt = 2.d0*sqrt(qq)*cos((theta+2.d0*r8depi())/3.d0) -a3/3.d0
                    phit = atan2(z*(1.d0+tt**2)-2.d0*k*tt, ba**2* (r*(1.d0+tt**2)-k*(1.d0-tt**2))&
                           )
                endif
            endif
        endif
!
!       POINT DONT L'ANGLE EST AU MILIEU DE PHI ET PHIT
        dphi = abs(phit-phi)/2.d0
        phi = (phi+phit)/2.d0
!
        rac = sqrt(1.d0-(1.d0-ba)*(1.d0+ba)*(sin(phi))**2)
!
!       SI LA DIFFERENCE D'ANGLE ENTRE DEUX ITERATIONS SUCCESSIVES EST
!       INFERIEURE A LA PRECISION, ON A CONVERGE ET ON SORT
        if (dphi .lt. epsc) then
            h = r*cos(phi)+z*sin(phi)-a*rac
            goto 9999
        endif
!
        dr = r-a*cos(phi)/rac
        dz = z-a*ba**2*sin(phi)/rac
        k = sqrt(dr**2+dz**2)
!
        if (linsid) k = -k
        t = dz/(dr+k)
!
10  end do
!
!     NOMBRE D'ITERATIONS MAXI ATTEINT
    call utmess('F', 'XFEM_2')
!
!
9999  continue
!
    call jedema()
end subroutine
