subroutine bijsom(umoy, rhof, r1, r2, long,&
                  cf0, icoq, jcoq, jmod, nbm,&
                  rki, thetai, thetaj, tcoef, ysol,&
                  bij)
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
! CALCUL DU TERME (I,J) DE LA MATRICE DE TRANSFERT DES FORCES
! FLUIDELASTIQUES PROJETEE SUR LA BASE MODALE DES STRUCTURES
! CALCUL DE L'INTEGRALE DONNANT BIJ APRES RESOLUTION DU PROBLEME FLUIDE
! INSTATIONNAIRE
! APPELANT : BIJMOC, BMOCCA
!-----------------------------------------------------------------------
!  IN : UMOY   : VITESSE DE L'ECOULEMENT MOYEN
!  IN : RHOF   : MASSE VOLUMIQUE DU FLUIDE
!  IN : R1     : RAYON REPERANT LA SURFACE DE LA STRUCTURE INTERNE
!  IN : R2     : RAYON REPERANT LA SURFACE DE LA STRUCTURE EXTERNE
!  IN : LONG   : LONGUEUR DU DOMAINE DE RECOUVREMENT DES DEUX COQUES
!  IN : CF0    : COEFFICIENT DE FROTTEMENT VISQUEUX
!  IN : ICOQ   : INDICE CARACTERISTIQUE DE LA COQUE EN MOUVEMENT POUR LE
!                MODE I, MOUVEMENT POUR LEQUEL ON A RESOLU LE PROBLEME
!                FLUIDE (ICOQ=1 COQUE INTERNE, ICOQ=2 COQUE EXTERNE)
!  IN : JCOQ   : INDICE CARACTERISTIQUE DE LA COQUE EN MOUVEMENT POUR LE
!                MODE J, COQUE SUR LAQUELLE ON INTEGRE LA FORCE FLUIDE
!                SURFACIQUE (JCOQ=1 COQUE INTERNE, JCOQ=2 COQUE EXTERNE)
!  IN : JMOD   : INDICE DU MODE J
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : RKI    : ORDRE DE COQUE COMMUN AUX DEUX MODES CONSIDERES
!  IN : THETAI : DEPHASAGE POUR LE MODE I
!  IN : THETAJ : DEPHASAGE POUR LE MODE J
!  IN : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
!  IN : YSOL   : TABLEAU SOLUTION (VECTEUR T(UI*,VI*,PI*) TABULE EN Z)
!                DU PROBLEME FLUIDE INSTATIONNAIRE, RESOLU EN
!                CONSIDERANT UN MOUVEMENT DE L'UNE DES COQUES SUIVANT
!                LE MODE I
! OUT : BIJ    : TERME (I,J) DE LA MATRICE B, COMPLEXE
!-----------------------------------------------------------------------
!
#include "asterc/r8pi.h"
#include "asterfort/defaxe.h"
#include "asterfort/profpr.h"
    real(kind=8) :: umoy, rhof, r1, r2, long, cf0
    integer :: icoq, jcoq, jmod, nbm
    real(kind=8) :: rki, thetai, thetaj, tcoef(10, nbm)
    complex(kind=8) :: ysol(3, 101), bij
!
    complex(kind=8) :: p, v, som
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: k
    real(kind=8) :: coep, coepr1, coepr2, dz, pi, rayon
    real(kind=8) :: wj, wpr, z
!-----------------------------------------------------------------------
    pi = r8pi()
    dz = long/100.d0
    som = dcmplx(0.d0,0.d0)
!
    rayon = r1
    if (jcoq .eq. 2) rayon = r2
!
    call profpr(icoq, rki, r1, r2, coepr1,&
                coepr2, wpr)
    if (jcoq .eq. 1) then
        coep = coepr1
    else
        coep = -1.d0*coepr2
    endif
!
!-----1.SI UMOY NULLE (STRUCTURES EN EAU AU REPOS)
!
    if (umoy .lt. 1.d-5) then
!
!-----1.1.CONTRIBUTION DU PREMIER POINT DANS LE CALCUL DE L INTEGRALE
!
        p = ysol(3,1)
        wj = defaxe(jcoq,jmod,0.d0,long,nbm,tcoef)
        som = som + 0.5d0*p*coep*wj
!
!-----1.2.CONTRIBUTIONS DES POINTS INTERMEDIAIRES
!
        do 10 k = 2, 100
            p = ysol(3,k)
            z = dble(k-1)*dz
            wj = defaxe(jcoq,jmod,z,long,nbm,tcoef)
            som = som + p*coep*wj
10      continue
!
!-----1.3.CONTRIBUTION DU DERNIER POINT
!
        p = ysol(3,101)
        wj = defaxe(jcoq,jmod,long,long,nbm,tcoef)
        som = som + 0.5d0*p*coep*wj
!
!-----2.SINON (STRUCTURES SOUS ECOULEMENT)
!
    else
!
!-----2.1.CONTRIBUTION DU PREMIER POINT
!
        p = ysol(3,1)
        v = ysol(2,1)
        wj = defaxe(jcoq,jmod,0.d0,long,nbm,tcoef)
        som = som + 0.5d0*(p*coep+0.5d0*rhof*cf0*umoy*v)*wj
!
!-----2.2.CONTRIBUTIONS DES POINTS INTERMEDIAIRES
!
        do 20 k = 2, 100
            p = ysol(3,k)
            v = ysol(2,k)
            z = dble(k-1)*dz
            wj = defaxe(jcoq,jmod,z,long,nbm,tcoef)
            som = som + (p*coep+0.5d0*rhof*cf0*umoy*v)*wj
20      continue
!
!-----2.3.CONTRIBUTION DU DERNIER POINT
!
        p = ysol(3,101)
        v = ysol(2,101)
        wj = defaxe(jcoq,jmod,long,long,nbm,tcoef)
        som = som + 0.5d0*(p*coep+0.5d0*rhof*cf0*umoy*v)*wj
!
    endif
!
!-----3.DEDUCTION DE B(I,J)
!
    bij = -1.d0*pi*rayon*dble(cos(rki*(thetai-thetaj))) *dz*som/(rki*rki)
!
end subroutine
