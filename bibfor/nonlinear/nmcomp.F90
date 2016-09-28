subroutine nmcomp(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, carcri, instam, instap,&
                  neps, epsm, deps, nsig, sigm,&
                  vim, option, angmas, nwkin, wkin,&
                  sigp, vip, ndsde, dsidep, nwkout,&
                  wkout, codret, mult_comp_)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/lcvali.h"
#include "asterfort/nmcoup.h"
#include "asterfort/nmcpl1.h"
#include "asterfort/nmcpl2.h"
#include "asterfort/nmcpl3.h"
#include "asterfort/redece.h"
#include "asterfort/lcidbg.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
!
    integer :: kpg, ksp, ndim, imate, codret, icp, numlc
    integer :: neps, nsig, nwkin, nwkout, ndsde
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=16) :: compor(*), option
    real(kind=8) :: carcri(*), instam, instap
    real(kind=8) :: epsm(*), deps(*), dsidep(*)
    real(kind=8) :: sigm(*), vim(*), sigp(*), vip(*)
    real(kind=8) :: wkin(nwkin), wkout(nwkout)
    real(kind=8) :: angmas(*)
    character(len=16), optional, intent(in) :: mult_comp_
! ----------------------------------------------------------------------
!     INTEGRATION DES LOIS DE COMPORTEMENT NON LINEAIRE POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES OU GRANDES DEFORMATIONS
!
! IN  FAMI,KPG,KSP  : FAMILLE ET NUMERO DU (SOUS)POINT DE GAUSS
!     NDIM    : DIMENSION DE L'ESPACE
!               3 : 3D , 2 : D_PLAN ,AXIS OU  C_PLAN
!     TYPMOD(2): MODELISATION ex: 1:3D, 2:INCO
!     IMATE   : ADRESSE DU MATERIAU CODE
!     COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
!                               (2) = NB VARIABLES INTERNES / PG
!                               (3) = HYPOTHESE SUR LES DEFORMATIONS
!                               (4) etc... (voir grandeur COMPOR)
!     CRIT    : CRITERES DE CONVERGENCE LOCAUX (voir grandeur CARCRI)
!     INSTAM  : INSTANT DU CALCUL PRECEDENT
!     INSTAP  : INSTANT DU CALCUL
!     NEPS    : NOMBRE DE CMP DE EPSM ET DEPS (SUIVANT MODELISATION)
!     EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
!     DEPS    : INCREMENT DE DEFORMATION TOTALE :
!                DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
!     NSIG    : NOMBRE DE CMP DE SIGM ET SIGP (SUIVANT MODELISATION)
!     SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
!     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
!     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
!     ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM),
!               + UN REEL QUI VAUT 0 SI NAUTIQUIES OU 2 SI EULER
!               + LES 3 ANGLES D'EULER
!     NWKIN   : DIMENSION DE WKIN
!     WKIN    : TABLEAU DE TRAVAIL EN ENTREE(SUIVANT MODELISATION)
!
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! VAR VIP     : VARIABLES INTERNES
!                IN  : ESTIMATION (ITERATION PRECEDENTE OU LAG. AUGM.)
!                OUT : EN T+
!     NDSDE   : DIMENSION DE DSIDEP
!     DSIDEP  : OPERATEUR TANGENT DSIG/DEPS OU DSIG/DF
!     NWKOUT  : DIMENSION DE WKOUT
!     WKOUT   : TABLEAU DE TRAVAIL EN SORTIE (SUIVANT MODELISATION)
!     CODRET  : CODE RETOUR LOI DE COMPORMENT :
!               CODRET=0 : TOUT VA BIEN
!               CODRET=1 : ECHEC DANS L'INTEGRATION DE LA LOI
!               CODRET=3 : SIZZ NON NUL (CONTRAINTES PLANES DEBORST)
!
! PRECISIONS :
! -----------
!  LES TENSEURS ET MATRICES SONT RANGES DANS L'ORDRE :
!         XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ
!
! -SI DEFORMATION = SIMO_MIEHE
!   EPSM(3,3)    GRADIENT DE LA TRANSFORMATION EN T-
!   DEPS(3,3)    GRADIENT DE LA TRANSFORMATION DE T- A T+
!
!  OUTPUT SI RESI (RAPH_MECA, FULL_MECA_*)
!   VIP      VARIABLES INTERNES EN T+
!   SIGP(6)  CONTRAINTE DE KIRCHHOFF EN T+ RANGES DANS L'ORDRE
!         XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ
!
!  OUTPUT SI RIGI (RIGI_MECA_*, FULL_MECA_*)
!   DSIDEP(6,3,3) MATRICE TANGENTE D(TAU)/D(FD) * (FD)T
!                 (AVEC LES RACINES DE 2)
!
! -SINON (DEFORMATION = PETIT OU PETIT_REAC OU GDEF_...)
!   EPSM(6), DEPS(6)  SONT LES DEFORMATIONS (LINEARISEES OU GREEN OU ..)
!
! ----------------------------------------------------------------------
!
!    POUR LES UTILITAIRES DE CALCUL TENSORIEL
    integer :: ndt, ndi
    common /tdim/ ndt,ndi
!
    character(len=16) :: optio2, mult_comp
    aster_logical :: cp, convcp
    integer :: cpl, nvv, ncpmax
    real(kind=8) :: r8bid
!
    codret = 0
    r8bid=r8vide()
!
!     CONTRAINTES PLANES
    call nmcpl1(compor, typmod, option, vip, deps,&
                optio2, cpl, nvv)
    cp=(cpl.ne.0)
!
!     DIMENSIONNEMENT POUR LE CALCUL TENSORIEL
    ndt = 2*ndim
    ndi = ndim
!
    if (cp) then
        convcp = .false.
        ncpmax = nint(carcri(9))
    else
        convcp = .true.
        ncpmax = 1
    endif
!
    mult_comp = ' '
    if (present(mult_comp_)) then
        mult_comp = mult_comp_
    endif
!
!     RECUP NUMLC
    read (compor(6),'(I16)') numlc
!
!     BOUCLE POUR ETABLIR LES CONTRAINTES PLANES
    do icp = 1, ncpmax
!
        if (compor(1) .eq. 'KIT_DDI') then
!        POUR EVITER LA RECURSIVITE. PETITES DEFORMATIONS
            call nmcoup(fami, kpg, ksp, ndim, typmod,&
                        imate, compor, mult_comp, cp, carcri, instam,&
                        instap, neps, epsm, deps, nsig,&
                        sigm, vim, option, angmas, nwkin,&
                        wkin, sigp, vip, ndsde, dsidep,&
                        nwkout, wkout, codret)
        else
            call redece(fami, kpg, ksp, ndim, typmod,&
                        imate, compor, mult_comp, carcri, instam, instap,&
                        neps, epsm, deps, nsig, sigm,&
                        vim, option, angmas, nwkin, wkin,&
                        cp, numlc, r8bid, r8bid, r8bid,&
                        sigp, vip, ndsde, dsidep, nwkout,&
                        wkout, codret)
        endif
!
!       VERIFIER LA CONVERGENCE DES CONTRAINTES PLANES ET
!       SORTIR DE LA BOUCLE SI NECESSAIRE
        if (cp) call nmcpl3(compor, option, carcri, deps, dsidep,&
                            ndim, sigp, vip, cpl, icp,&
                            convcp)
!
        if (convcp) then
            exit
        endif
!
    end do
!
!     CONTRAINTES PLANES METHODE DE BORST
    if (cp) then
        if (codret .eq. 0) then
            call nmcpl2(compor, typmod, option, optio2, cpl,&
                        nvv, carcri, deps, dsidep, ndim,&
                        sigp, vip, codret)
        else
            option=optio2
        endif
    endif
!     EXAMEN DU DOMAINE DE VALIDITE
    if (codret .eq. 0) then
        call lcvali(fami, kpg, ksp, imate, compor,&
                    ndim, epsm, deps, instam, instap,&
                    codret)
    else if (codret .eq. 1) then
        call lcidbg(fami, kpg, ksp, typmod, compor,&
                    carcri, instam, instap, neps, epsm,&
                    deps, nsig, sigm, vim, option)
    endif
end subroutine
