subroutine nmgr2d(fami, nno, npg, ipoids, ivf,&
                  vff, idfde, geomi, typmod, option,&
                  imate, compor, lgpg, crit, instam,&
                  instap, deplm, deplp, angmas, sigm,&
                  vim, matsym, dfdi, pff, def,&
                  sigp, vip, matuu, vectu, codret)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
!
! aslint: disable=W1504
    implicit none
!
#include "asterf_types.h"
#include "asterfort/codere.h"
#include "asterfort/lcdetf.h"
#include "asterfort/lcegeo.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
#include "asterfort/nmgrtg.h"
#include "asterfort/pk2sig.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    integer :: nno, npg, imate, lgpg, codret, cod(9)
    integer :: ipoids, ivf, idfde
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
!
    real(kind=8) :: instam, instap, angmas(3), detfm
    real(kind=8) :: geomi(2, nno), crit(3)
    real(kind=8) :: dfdi(nno, 2), deplm(2*nno), deplp(2*nno)
    real(kind=8) :: pff(4, nno, nno), def(4, nno, 2), vff(*)
    real(kind=8) :: sigm(4, npg), sigp(4, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    real(kind=8) :: matuu(*), vectu(2*nno)
    aster_logical :: matsym
!
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN GRANDE ROTATION ET PETITE DEFORMATION EN 2D
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  POIDSG  : POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOMI   : COORDONEES DES NOEUDS SUR CONFIG INITIALE
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  DEPLP   : DEPLACEMENT A L'INSTANT COURANT
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! IN  MATSYM  : VRAI SI LA MATRICE DE RIGIDITE EST SYMETRIQUE
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!.......................................................................
!
!
    aster_logical :: grand, axi, cplan
!
    integer :: kpg, j
!
    real(kind=8) :: dsidep(6, 6), f(3, 3), fm(3, 3), epsm(6), epsp(6), deps(6)
    real(kind=8) :: r, sigma(6), sigmn(6), detf, poids, maxeps
    real(kind=8) :: elgeom(10, 9), r8bid(1), rac2
!
!     INITIALISATION
!
    rac2 = sqrt(2.d0)
    grand = .true.
    axi = typmod(1) .eq. 'AXIS'
    cplan = typmod(1) .eq. 'C_PLAN'
!
!     CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES AU COMPORTEMENT
!
    call lcegeo(nno, npg, ipoids, ivf, idfde,&
                geomi, typmod, compor, 2, dfdi,&
                deplm, deplp, elgeom)
!
!     INITIALISATION CODES RETOURS
!
    do 1955 kpg = 1, npg
        cod(kpg)=0
1955 end do
!
!     CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 800 kpg = 1, npg
!
!        CALCUL DES ELEMENTS GEOMETRIQUES
!
!        CALCUL DE EPSM EN T- POUR LDC
!
        call r8inir(6, 0.d0, epsm, 1)
        call r8inir(6, 0.d0, epsp, 1)
        call nmgeom(2, nno, axi, grand, geomi,&
                    kpg, ipoids, ivf, idfde, deplm,&
                    .true._1, poids, dfdi, fm, epsm,&
                    r)
!
!        CALCUL DE F, EPSP, DFDI, R ET POIDS EN T+
!
        call nmgeom(2, nno, axi, grand, geomi,&
                    kpg, ipoids, ivf, idfde, deplp,&
                    .true._1, poids, dfdi, f, epsp,&
                    r)
!
!        CALCUL DE DEPS POUR LDC
!
        maxeps=0.d0
        do 25 j = 1, 6
            deps (j)=epsp(j)-epsm(j)
            maxeps=max(maxeps,abs(epsp(j)))
 25     continue
!
!        VERIFICATION QUE EPS RESTE PETIT
        if (maxeps .gt. 0.05d0) then
            if (compor(1)(1:4) .ne. 'ELAS') then
                call utmess('A', 'COMPOR2_9', sr=maxeps)
            endif
        endif
!
!        LOI DE COMPORTEMENT
!        CONTRAINTE CAUCHY -> CONTRAINTE LAGRANGE POUR LDC EN T-
!
        if (cplan) fm(3,3) = sqrt(abs(2.d0*epsm(3)+1.d0))
        call lcdetf(2, fm, detfm)
        call pk2sig(2, fm, detfm, sigmn, sigm(1, kpg),&
                    -1)
!
        sigmn(4) = sigmn(4)*rac2
!
!        INTEGRATION
!
        call nmcomp(fami, kpg, 1, 2, typmod,&
                    imate, compor, crit, instam, instap,&
                    6, epsm, deps, 6, sigmn,&
                    vim(1, kpg), option, angmas, 10, elgeom(1, kpg),&
                    sigma, vip(1, kpg), 36, dsidep, 1,&
                    r8bid, cod(kpg))
!
!
        if (cod(kpg) .eq. 1) then
            goto 1956
        endif
!
!        CALCUL DE LA MATRICE DE RIGIDITE ET DES FORCES INTERIEURES
!
        call nmgrtg(2, nno, poids, kpg, vff,&
                    dfdi, def, pff, option, axi,&
                    r, fm, f, dsidep, sigmn,&
                    sigma, matsym, matuu, vectu)
!
!
!        CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION LAGRANGE -> CAUCHY
!
        if (option(1:4) .eq. 'RAPH' .or. option(1:4) .eq. 'FULL') then
            if (cplan) f(3,3) = sqrt(abs(2.d0*epsp(3)+1.d0))
            call lcdetf(2, f, detf)
            call pk2sig(2, f, detf, sigma, sigp(1, kpg),&
                        1)
        endif
!
800 end do
!
1956 continue
!
! - SYNTHESE DES CODES RETOURS
!
    call codere(cod, npg, codret)
end subroutine
