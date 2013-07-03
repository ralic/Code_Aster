subroutine pipepe(pilo, ndim, nno, npg, ipoids,&
                  ivf, idfde, geom, typmod, mate,&
                  compor, lgpg, deplm, sigm, vim,&
                  ddepl, depl0, depl1, copilo, dfdi,&
                  elgeom, iborne, ictau)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/matfpe.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/pidefo.h"
#include "asterfort/pielas.h"
#include "asterfort/pipdef.h"
#include "asterfort/r8inir.h"
#include "blas/dcopy.h"
    integer :: ndim, nno, npg
    integer :: mate, ipoids, ivf, idfde
    integer :: lgpg, iborne, ictau
    character(len=8) :: typmod(*)
    character(len=16) :: pilo, compor(*)
    real(kind=8) :: geom(ndim, *), deplm(*), ddepl(*)
    real(kind=8) :: sigm(2*ndim, npg), vim(lgpg, npg)
    real(kind=8) :: depl0(*), depl1(*)
    real(kind=8) :: copilo(5, npg), dfdi(*), elgeom(10, *)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE)
!
! CALCUL  DES COEFFICIENTS DE PILOTAGE POUR PRED_ELAS/DEFORMATION
!
! ----------------------------------------------------------------------
!
!
! IN  PILO   : MODE DE PILOTAGE: DEFORMATION, PRED_ELAS
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG    : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS : POIDS DES POINTS DE GAUSS
! IN  IVF    : VALEUR DES FONCTIONS DE FORME
! IN  IDFDE  : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM   : COORDONEES DES NOEUDS
! IN  TYPMOD : TYPE DE MODELISATION
! IN  MATE   : MATERIAU CODE
! IN  COMPOR : COMPORTEMENT
! IN  LGPG   : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!             CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  DEPLM  : DEPLACEMENT EN T-
! IN  DDEPL  : INCREMENT DE DEPLACEMENT A L'ITERATION NEWTON COURANTE
! IN  SIGM   : CONTRAINTES DE CAUCHY EN T-
! IN  VIM    : VARIABLES INTERNES EN T-
! IN  DEPL0  : CORRECTION DE DEPLACEMENT POUR FORCES FIXES
! IN  DEPL1  : CORRECTION DE DEPLACEMENT POUR FORCES PILOTEES
! IN  DFDI   : DERIVEE DES FONCTIONS DE FORME
! IN  ELGEOM : TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX LOIS
!              DE COMPORTEMENT (DIMENSION MAXIMALE FIXEE EN DUR, EN
!              FONCTION DU NOMBRE MAXIMAL DE POINT DE GAUSS)
! IN  IBORNE : ADRESSE JEVEUX POUR BORNES PILOTAGE
! IN  ICTAU  : ADRESSE JEVEUX POUR PARAMETRE PILOTAGE
! OUT COPILO : COEFFICIENTS A0 ET A1 POUR CHAQUE POINT DE GAUSS
!
!
!
!
!
    integer :: kpg, k, ndimsi
    real(kind=8) :: fm(3, 3), epsm(6), epsp(6), epsd(6)
    real(kind=8) :: rac2
    real(kind=8) :: etamin, etamax, tau, sigma(6)
!
! ----------------------------------------------------------------------
!
    call matfpe(-1)
!
! --- INITIALISATIONS
!
    rac2 = sqrt(2.d0)
    ndimsi = 2*ndim
!
    call r8inir(6, 0.d0, sigma, 1)
    call r8inir(npg*5, r8vide(), copilo, 1)
!
! --- TRAITEMENT DE CHAQUE POINT DE GAUSS
!
    do 10 kpg = 1, npg
!
! --- CALCUL DES DEFORMATIONS
!
        call pipdef(ndim, nno, kpg, ipoids, ivf,&
                    idfde, geom, typmod, compor, deplm,&
                    ddepl, depl0, depl1, dfdi, fm,&
                    epsm, epsp, epsd)
!
! --- PILOTAGE PAR L'INCREMENT DE DEFORMATION
!
        if (pilo .eq. 'DEFORMATION') then
!
            call pidefo(ndim, npg, kpg, compor, fm,&
                        epsm, epsp, epsd, copilo)
!
!
! --- PILOTAGE PAR LA PREDICTION ELASTIQUE
!
        else if (pilo .eq. 'PRED_ELAS') then
            tau = zr(ictau)
            etamin = zr(iborne+1)
            etamax = zr(iborne)
!
            call dcopy(ndimsi, sigm(1, kpg), 1, sigma, 1)
            do 70 k = 4, ndimsi
                sigma(k) = sigma(k)*rac2
70          continue
!
            call pielas(ndim, npg, kpg, compor, typmod,&
                        mate, elgeom, lgpg, vim, epsm,&
                        epsp, epsd, sigma, etamin, etamax,&
                        tau, copilo)
        else
            call assert(.false.)
        endif
!
10  end do
!
    call matfpe(1)
!
end subroutine
