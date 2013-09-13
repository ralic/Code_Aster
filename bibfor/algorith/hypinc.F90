subroutine hypinc(fami, kpg, ksp, poum, ndim,&
                  typmod, imate, compor, crit, epsm,&
                  deps, sigm, sigp, dsidep, codret)
!
! ======================================================================
! COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/hyp3ci.h"
#include "asterfort/hyp3cv.h"
#include "asterfort/hyp3di.h"
#include "asterfort/hyp3dv.h"
#include "asterfort/hypcpc.h"
#include "asterfort/hypcpd.h"
#include "asterfort/hypmat.h"
#include "asterfort/matini.h"
#include "asterfort/utmess.h"
#include "blas/dscal.h"
    integer :: kpg, ksp, ndim
    character(len=*) :: fami, poum
    character(len=8) :: typmod(*)
    integer :: imate
    character(len=16) :: compor(*)
    real(kind=8) :: crit(*)
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), sigp(6)
    real(kind=8) :: dsidep(6, 6)
    integer :: codret
!
! ----------------------------------------------------------------------
!
!     LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
!
!     C10 (I1-3) + C01 (I2-3)+ C20 (I1-3)^2 + K/2(J-1)²
!
!     POUR LES ELEMENTS ISOPARAMETRIQUES 3D, CP, ET DP
!
!     CONTRAINTES ET MATRICE TANGENTE EN COMP_INCR
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  TYPMOD : TYPE DE MODELISATION
! IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
!                             (1) = NB ITERATIONS MAXI A CONVERGENCE
!                                   (ITER_INTE_MAXI == ITECREL)
!                             (2) = TYPE DE JACOBIEN A T+DT
!                                   (TYPE_MATR_COMP == MACOMP)
!                                   0 = EN VITESSE     >SYMETRIQUE
!                                   1 = EN INCREMENTAL >NON-SYMETRIQUE
!                             (3) = VALEUR TOLERANCE DE CONVERGENCE
!                                    (RESI_INTE_RELA == RESCREL)
! IN  IMATE  : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT  (1) = TYPE DE RELATION COMPORTEMENT
!                             (2) = NB VARIABLES INTERNES / PG
!                             (3) = HYPOTHESE SUR LES DEFORMATIONS
!                             (4) = COMP_ELAS (OU COMP_INCR)
! IN  FAMI   : FAMILLE DE POINTS DE GAUSS
! IN  KPG    : NUMERO DU POINT DE GAUSS
! IN  KSP    : NUMERO DU SOUS-POINT DE GAUSS
! IN  EPSM   : DEFORMATION A L'INSTANT MOINS
!               (SI C_PLAN EPS(3) EST EN FAIT CALCULE)
! IN  DEPS   : INCREMENT DE DEFORMATION
! IN  POUM   : '-' POUR VARIABLES DE COMMANDE
! IN  SIGM   : CONTRAINTES A L'INSTANT MOINS
! OUT SIGP   : CONTRAINTES LAGRANGIENNES CALCULEES
! OUT DSIDEP : MATRICE DE RIGIDITE TANGENTE
! OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
!
! ----------------------------------------------------------------------
!
    integer :: i, j, l, m
    real(kind=8) :: c11, c22, c12, c33, c13, c23
    real(kind=8) :: eps(6), epstot(6)
    real(kind=8) :: cvol(6, 6), ciso(6, 6)
    real(kind=8) :: svol(6), siso(6)
    real(kind=8) :: c10, c01, c20, k
    integer :: nitmax
    real(kind=8) :: epsi
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    call matini(6, 6, 0.d0, dsidep)
    nitmax = int(crit(1))
    epsi = crit(3)
    ASSERT(compor(4).eq.'COMP_INCR')
!
! --- LECTURE DES CARACTERISTIQUES MATERIAU
!
    if ((compor(1)(1:10).eq. 'ELAS_HYPER')) then
        call hypmat(fami, kpg, ksp, poum, imate,&
                    c10, c01, c20, k)
    else
        ASSERT(.false.)
    endif
!
! --- A PRIORI ON A CONVERGE
!
    codret = 0
!
! --- CALCUL TENSEUR DEFORMATION CAUCHY-GREEN DROIT
!
    eps(1)=epsm(1)+deps(1)
    eps(2)=epsm(2)+deps(2)
    eps(3)=epsm(3)+deps(3)
    eps(4)=epsm(4)+deps(4)
    eps(5)=epsm(5)+deps(5)
    eps(6)=epsm(6)+deps(6)
!
! --- PRE-TRAITEMENT DES DEFORMATIONS (PAS DE NOTATION DE VOIGT)
!
    do 10 i = 1, 3
        epstot( i)=eps( i)
        epstot(3+i)=eps(3+i)/sqrt(2.d0)
10  end do
!
! --- CALCUL CONTRAINTES ET MATRICE TANGENTE
!
    if (typmod(1) .eq. '3D' .or. typmod(1) .eq. '3D_SI' .or. typmod(1)(1:6) .eq. 'D_PLAN') then
! --- CALCUL DES ELONGATIONS
        c11 = 2.d0*epstot(1)+1.d0
        c12 = 2.d0*epstot(4)
        c22 = 2.d0*epstot(2)+1.d0
        c33 = 2.d0*epstot(3)+1.d0
        c13 = 2.d0*epstot(5)
        c23 = 2.d0*epstot(6)
! --- CALCUL DES CONTRAINTES ISOTROPIQUES
        call hyp3ci(c11, c22, c33, c12, c13,&
                    c23, c10, c01, c20, siso,&
                    codret)
        if (codret .eq. 1) then
            goto 99
        endif
! --- CALCUL DES CONTRAINTES VOLUMIQUES
        call hyp3cv(c11, c22, c33, c12, c13,&
                    c23, k, svol, codret)
        if (codret .eq. 1) then
            goto 99
        endif
! --- CALCUL DE LA MATRICE TANGENTE (PARTIE ISOTROPIQUE)
        call hyp3di(c11, c22, c33, c12, c13,&
                    c23, c10, c01, c20, ciso,&
                    codret)
        if (codret .eq. 1) then
            goto 99
        endif
! --- CALCUL DE LA MATRICE TANGENTE (PARTIE VOLUMIQUE)
        call hyp3dv(c11, c22, c33, c12, c13,&
                    c23, k, cvol, codret)
        if (codret .eq. 1) then
            goto 99
        endif
! --- ASSEMBLAGE VOLUMIQUE/ISOTROPIQUE
! --- ON CORRIGE A CE NIVEAU LES TERMES LIES AU CISAILLEMENT
! --- A TERME IL FAUDRA RE-ECRIRE LES ROUTINES D'INTEGRATION
        do 30 i = 1, 3
            sigp( i) = siso( i)+svol( i)
            sigp(3+i) = (siso(3+i)+svol(3+i))/2.d0
            do 20 j = 1, 3
                dsidep( i, j) = ciso( i, j)+cvol( i, j)
                dsidep(3+i, j) = (ciso(3+i, j)+cvol(3+i, j))/sqrt( 2.d0)
                dsidep( i,3+j) = (ciso( i,3+j)+cvol( i,3+j))/sqrt( 2.d0)
                dsidep(3+i,3+j) = (ciso(3+i,3+j)+cvol(3+i,3+j))/ 2.d0
20          continue
30      continue
    else if (typmod(1)(1:6) .eq. 'C_PLAN') then
! --- CALCUL DES ELONGATIONS
        c11 = 2.d0*epstot(1)+1.d0
        c12 = 2.d0*epstot(4)
        c22 = 2.d0*epstot(2)+1.d0
        c33 = 1.d0
! --- CALCUL DES CONTRAINTES
        call hypcpc(c11, c22, c33, c12, k,&
                    c10, c01, c20, nitmax, epsi,&
                    sigp, codret)
        if (codret .eq. 1) then
            goto 99
        endif
! --- CALCUL DE LA MATRICE TANGENTE
        call hypcpd(c11, c22, c33, c12, k,&
                    c10, c01, c20, dsidep, codret)
        if (codret .eq. 1) then
            goto 99
        endif
! --- ON CORRIGE A CE NIVEAU LES TERMES LIES AU CISAILLEMENT
! --- A TERME IL FAUDRA RE-ECRIRE LES ROUTINES D'INTEGRATION
        do 50 i = 1, 3
            sigp(3+i) = sigp(3+i)/2.d0
            do 40 j = 1, 3
                dsidep(3+i, j) = dsidep(3+i, j)/sqrt(2.d0)
                dsidep( i,3+j) = dsidep( i,3+j)/sqrt(2.d0)
                dsidep(3+i,3+j) = dsidep(3+i,3+j)/ 2.d0
40          continue
50      continue
! --- PRISE EN COMPTE DE L'HYPOTHESE DE CONTRAINTES PLANES DANS DSIDEP
        do 70 m = 1, 2*ndim
            if (m .eq. 3) goto 70
            do 60 l = 1, 2*ndim
                if (l .eq. 3) goto 60
                dsidep(m,l )= dsidep(m,l) - 1.d0/dsidep(3,3)*dsidep(m,&
                3)*dsidep(3,l)
60          continue
70      continue
    else
        call utmess('F', 'ELASHYPER_97', sk=typmod(1))
    endif
!
! --- POST-TRAITEMENT DES CONTRAINTES (PAS DE NOTATION DE VOIGT)
!
    call dscal(3, sqrt(2.d0), sigp(4), 1)
!
99  continue
end subroutine
