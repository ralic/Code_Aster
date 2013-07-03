subroutine pbflso(umoy, rmoy, long, icoq, imod,&
                  nbm, rkip, tcoef, harm, lambda,&
                  kcalcu, passag, condit, gamma, d,&
                  ysol)
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
! RESOLUTION DU PROBLEME FLUIDE INSTATIONNAIRE : CALCUL DU VECTEUR
! SOLUTION T(UI*,VI*,PI*) LE LONG DES STRUCTURES
! APPELANT : PBFLUI
!-----------------------------------------------------------------------
!  IN : UMOY   : VITESSE DE L'ECOULEMENT MOYEN
!  IN : RMOY   : RAYON MOYEN
!  IN : LONG   : LONGUEUR DU DOMAINE DE RECOUVREMENT DES DEUX COQUES
!  IN : ICOQ   : INDICE CARACTERISANT LA COQUE SUR LAQUELLE ON TRAVAILLE
!                ICOQ=1 COQUE INTERNE  ICOQ=2 COQUE EXTERNE
!  IN : IMOD   : INDICE DU MODE CONSIDERE
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : RKIP   : ORDRE DE COQUE DU MODE CONSIDERE, PONDERE PAR LA VALEUR
!                MOYENNE DU PROFIL DE PRESSION
!  IN : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
!  IN : HARM   : VECTEUR DE TRAVAIL
!  IN : LAMBDA : VALEURS PROPRES DE L'OPERATEUR DIFFERENTIEL
!  IN : KCALCU : MATRICE RECTANGULAIRE A COEFFICIENTS CONSTANTS
!                PERMETTANT DE CALCULER UNE SOLUTION PARTICULIERE DU
!                PROBLEME FLUIDE INSTATIONNAIRE, LORSQUE UMOY > 0
!  IN : PASSAG : MATRICE DONT LES COLONNES SONT LES VECTEURS PROPRES DE
!                L'OPERATEUR DIFFERENTIEL
!  IN : CONDIT : COEFFICIENTS DE PRECONDITIONNEMENT
!  IN : GAMMA  : COEFFICIENTS DE LA COMBINAISON LINEAIRE DONNANT LA
!                SOLUTION GENERALE DU PROBLEME FLUIDE INSTATIONNAIRE
!                (DECOMPOSITION SUR UNE FAMILLE D'EXPONENTIELLES)
!                LORSQUE UMOY > 0
!  IN : D      : COEFFICIENTS DE LA COMBINAISON LINEAIRE DONNANT LA
!                PRESSION PERTURBEE (DECOMPOSITION SUR UNE FAMILLE
!                DE FONCTIONS EXPONENTIELLES REELLES ET COMPLEXES)
!                LORSQUE UMOY = 0
! OUT : YSOL   : TABLEAU SOLUTION (VECTEUR T(UI*,VI*,PI*) TABULE EN Z)
!-----------------------------------------------------------------------
!
#include "asterfort/pbflkz.h"
    real(kind=8) :: umoy, rmoy, long
    integer :: icoq, imod, nbm
    real(kind=8) :: rkip, tcoef(10, nbm), harm(6)
    complex(kind=8) :: lambda(3), kcalcu(3, 4), passag(3, 3)
    real(kind=8) :: condit(3)
    complex(kind=8) :: gamma(3)
    real(kind=8) :: d(6)
    complex(kind=8) :: ysol(3, 101)
!
    real(kind=8) :: ln
    complex(kind=8) :: somm1, somm2
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: itab, k, m1, m2
    real(kind=8) :: dz, u, v, z
!-----------------------------------------------------------------------
    itab = 0
    if (icoq .eq. 2) itab = 5
    ln = tcoef(1+itab,imod)
    dz = long/100.d0
!
    if (umoy .lt. 1.d-5) then
!
        do 10 k = 1, 101
            z = dble(k-1)*dz
            somm1 = dcmplx(0.d0,0.d0)
            u = (ln/long)*z
            harm(1) = dble(cos(u))
            harm(2) = dble(sin(u))
            harm(3) = dble(cosh(u))
            harm(4) = dble(sinh(u))
            v = -1.d0*(rkip/rmoy)*(long-z)
            harm(5) = dble(exp(v))
            v = -1.d0*(rkip/rmoy)*z
            harm(6) = dble(exp(v))
            do 11 m1 = 1, 6
                somm1 = somm1 + d(m1)*harm(m1)
11          continue
            ysol(3,k) = somm1
10      continue
!
    else
!
        do 20 k = 1, 101
            z = dble(k-1)*dz
            do 21 m1 = 1, 3
                somm2 = dcmplx(0.d0,0.d0)
                do 22 m2 = 1, 3
                    somm2 = somm2 + passag(m1,m2) * gamma(m2) * dcmplx(exp(lambda(m2)*(z-condit(m&
                            &2)*long)))
22              continue
                ysol(m1,k) = pbflkz(m1,z,long,ln,kcalcu) + somm2
21          continue
20      continue
!
    endif
!
end subroutine
