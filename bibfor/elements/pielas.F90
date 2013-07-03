subroutine pielas(ndim, npg, kpg, compor, typmod,&
                  mate, elgeom, lgpg, vim, epsm,&
                  epsp, epsd, sigma, etamin, etamax,&
                  tau, copilo)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/pipedo.h"
#include "asterfort/pipedp.h"
#include "asterfort/pipeds.h"
#include "asterfort/pipeef.h"
#include "asterfort/pipepl.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "blas/daxpy.h"
    integer :: ndim, kpg, npg
    integer :: mate
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*)
    integer :: lgpg
    real(kind=8) :: vim(lgpg, npg)
    real(kind=8) :: epsm(6), epsp(6), epsd(6)
    real(kind=8) :: copilo(5, npg)
    real(kind=8) :: etamin, etamax, tau
    real(kind=8) :: sigma(6)
    real(kind=8) :: elgeom(10, *)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS/DEFORMATION)
!
! PILOTAGE PAR PREDICTION ELASTIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NPG    : NOMBRE DE POINTS DE GAUSS
! IN  KPG    : NUMERO DU POINT DE GAUSS
! IN  TYPMOD : TYPE DE MODELISATION
! IN  MATE   : MATERIAU CODE
! IN  ELGEOM : TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX LOIS
!              DE COMPORTEMENT (DIMENSION MAXIMALE FIXEE EN DUR, EN
!              FONCTION DU NOMBRE MAXIMAL DE POINT DE GAUSS)
! IN  COMPOR : COMPORTEMENT
! IN  LGPG   : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!             CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  VIM    : VARIABLES INTERNES EN T-
! IN  EPSM   : DEFORMATIONS AU TEMPS MOINS
! IN  EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! IN  EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
! IN  SIGMA  : CONTRAINTES AVEC SQRT(2)
! IN  ETAMIN : BORNE INF. PILOTAGE
! IN  ETAMAX : BORNE SUP. PILOTAGE
! IN  TAU    : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
! OUT COPILO : COEFFICIENTS A0 ET A1 POUR CHAQUE POINT DE GAUSS
!
!
!
!
    integer :: ndimsi
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    ndimsi = 2*ndim
!
! --- CALCUL SUIVANT COMPORTEMENT
!
    if (compor(1) .eq. 'ENDO_FRAGILE') then
        call daxpy(ndimsi, 1.d0, epsm, 1, epsp,&
                   1)
        call pipeef(ndim, typmod, tau, mate, vim(1, kpg),&
                    epsp, epsd, copilo(1, kpg), copilo(2, kpg), copilo(3, kpg),&
                    copilo(4, kpg), copilo(5, kpg))
!
        else if (compor(1).eq.'VMIS_ISOT_TRAC' .or. compor(1)&
    .eq.'VMIS_ISOT_LINE') then
        call pipepl(ndim, compor(1), typmod, tau, mate,&
                    sigma, vim(1, kpg), epsp, epsd, copilo(1, kpg),&
                    copilo(2, kpg), copilo(3, kpg), copilo(4, kpg), copilo(5, kpg))
!
    else if (compor(1).eq.'ENDO_ISOT_BETON') then
        call daxpy(ndimsi, 1.d0, epsm, 1, epsp,&
                   1)
!
        if (etamin .eq. -r8gaem() .or. etamax .eq. r8gaem()) call u2mesg('F', 'MECANONLINE_60',&
                                                                         1, compor(1), 0, 0, 0,&
                                                                         0.d0)
!
        call pipeds(ndim, typmod, tau, mate, vim(1, kpg),&
                    epsm, epsp, epsd, etamin, etamax,&
                    copilo(1, kpg), copilo(2, kpg), copilo(3, kpg), copilo(4, kpg),&
                    copilo(5, kpg))
!
    else if (compor(1).eq.'ENDO_ORTH_BETON') then
        call daxpy(ndimsi, 1.d0, epsm, 1, epsp,&
                   1)
!
        if (etamin .eq. -r8gaem() .or. etamax .eq. r8gaem()) call u2mesg('F', 'MECANONLINE_60',&
                                                                         1, compor(1), 0, 0, 0,&
                                                                         0.d0)
!
        call pipedo(ndim, typmod, tau, mate, vim(1, kpg),&
                    epsm, epsp, epsd, etamin, etamax,&
                    copilo(1, kpg), copilo(2, kpg), copilo(3, kpg), copilo(4, kpg),&
                    copilo(5, kpg))
    else if (compor(1).eq.'BETON_DOUBLE_DP') then
        call daxpy(ndimsi, 1.d0, epsm, 1, epsp,&
                   1)
!
        call pipedp(kpg, 1, ndim, typmod, mate,&
                    epsm, sigma, vim(1, kpg), epsp, epsd,&
                    elgeom(1, kpg), copilo(1, kpg), copilo(2, kpg))
!
    else
        call u2mesk('F', 'PILOTAGE_88', 1, compor(1))
    endif
end subroutine
