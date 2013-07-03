subroutine redece(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, instam, instap,&
                  neps, epsdt, depst, nsig, sigd,&
                  vind, option, angmas, nwkin, wkin,&
                  cp, numlc, tempd, tempf, tref,&
                  sigf, vinf, ndsde, dsde, nwkout,&
                  wkout, codret)
! aslint: disable=W1306,W1504
    implicit none
!       ================================================================
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
! person_in_charge: jean-michel.proix at edf.fr
!
! ======================================================================
!     INTEGRATION DES LOIS DE COMPORTEMENT NON LINEAIRE POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES OU GRANDES DEFORMATIONS
! ======================================================================
! ROUTINE DE REDECOUPAGE LOCAL DU PAS dE TEMPS
! ----------------------------------------------------------------------
!       - SI CRIT(5) = -N  EN CAS DE NON-CONVERGENCE LOCALE ON EFFECTUE
!                          UN REDECOUPAGE DU PAS DE TEMPS EN N PALIERS
!                          L ORDRE D EXECUTION ETANT REMONTE EN ARGUMENT
!                          DANS REDECE, APPELE PAR NMCOMPOR AVANT PLASTI
!       - SI CRIT(5) = -1,0,1  PAS DE REDECOUPAGE DU PAS DE TEMPS
!       - SI CRIT(5) = +N  ON EFFECTUE UN REDECOUPAGE DU PAS DE TEMPS
!                          EN N PALIERS A CHAQUE APPEL DE REDECE
!                          LE PREMIER APPEL DE PLASTI SERT A
!                          L'INITIALISATION DE NVI
!       SI APRES REDECOUPAGE ON ABOUTIT A UN CAS DE NON CONVERGENCE, ON
!       REDECOUPE A NOUVEAU LE PAS DE TEMPS, EN 2*N PALIERS
!
!    ATTENTION  VIND    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
!
! ======================================================================
!     ARGUMENTS
! ======================================================================
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
!     CP      : LOGIQUE = VRAI EN CONTRAINTES PLANES DEBORST
!     NUMLC   : NUMERO DE LOI DE COMPORTEMENT ISSUE DU CATALOGUE DE LC
!     TEMPD,TEMPF,TREF : TEMPERATURES SI L'APPEL PROVIENT DE CALCME
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
#include "asterfort/lc0000.h"
#include "asterfort/lceqve.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcsoma.h"
#include "asterfort/lcsove.h"
#include "asterfort/r8inir.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: imate, ndim, ndt, ndi, nvi, kpg, ksp, numlc
    integer :: neps, nsig, nwkin, nwkout, ndsde
!
    real(kind=8) :: crit(*), angmas(*)
    real(kind=8) :: instam, instap, tempd, tempf, tref
    real(kind=8) :: wkin(nwkin), wkout(nwkout)
    real(kind=8) :: epsdt(neps), depst(neps)
    real(kind=8) :: sigd(nsig), sigf(nsig)
    real(kind=8) :: vind(*), vinf(*)
    real(kind=8) :: dsde(ndsde)
!
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    logical :: cp
!
!       ----------------------------------------------------------------
!       VARIABLES LOCALES POUR LE REDECOUPAGE DU PAS DE TEMPS
!               TD      INSTANT T
!               TF      INSTANT T+DT
!               TEMD    TEMPERATURE A T
!               TEMF    TEMPERATURE A T+DT
!               EPS     DEFORMATION TOTALE A T
!               DEPS    INCREMENT DE DEFORMATION TOTALE
!               SD      CONTRAINTE A T
!               VD      VARIABLES INTERNES A T    + INDICATEUR ETAT T
!               DSDELO MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!               NPAL            NOMBRE DE PALIER POUR LE REDECOUPAGE
!               ICOMP           COMPORTEUR POUR LE REDECOUPAGE DU PAS DE
!                                    TEMPS
!               RETURN1 EN CAS DE NON CONVERGENCE LOCALE
!       ----------------------------------------------------------------
!
    integer :: icomp, npal, ipal, codret, k
    real(kind=8) :: eps(neps), deps(neps), sd(nsig)
    real(kind=8) :: dsdelo(ndsde)
    real(kind=8) :: deltat, td, tf
!       ----------------------------------------------------------------
!       COMMONS POUR VARIABLES DE COMMANDE : CAII17 ET CARR01
    integer :: nfpgmx
    parameter (nfpgmx=10)
    integer :: nfpg, jfpgl, decala(nfpgmx), km, kp, kr, iredec
    common /caii17/nfpg,jfpgl,decala,km,kp,kr,iredec
    real(kind=8) :: timed1, timef1, td1, tf1
    common /carr01/timed1,timef1,td1,tf1
!       ----------------------------------------------------------------
!       ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!       ----------------------------------------------------------------
!       ----------------------------------------------------------------
!       -- POUR LES VARIABLES DE COMMANDE :
    iredec=1
    timed1=instam
    timef1=instap
    td1=instam
    tf1=instap
!
!
    ipal = int(crit(5))
    codret=0
!
!       CORRECTION JMP : POURQUOI REDECOUPER POUR RIGI_MECA_TANG ?
    if (option(1:9) .eq. 'RIGI_MECA') ipal=0
!
    read (compor(2),'(I16)') nvi
!
! IPAL = 0  1 OU -1 -> PAS DE REDECOUPAGE DU PAS DE TEMPS
!
    if (ipal .eq. 0 .or. ipal .eq. 1 .or. ipal .eq. -1) then
        ipal = 0
        npal = 0
        icomp = 2
!
! IPAL < -1 -> REDECOUPAGE DU PAS DE TEMPS EN CAS DE NON CONVERGENCE
!
    else if (ipal .lt. -1) then
        npal = -ipal
        icomp = 0
!
! IPAL > 1 -> REDECOUPAGE IMPOSE DU PAS DE TEMPS
!
    else if (ipal .gt. 1) then
        npal = ipal
        icomp = -1
    endif
!
    call lc0000(fami, kpg, ksp, ndim, typmod,&
                imate, compor, crit, instam, instap,&
                neps, epsdt, depst, nsig, sigd,&
                vind, option, angmas, nwkin, wkin,&
                cp, numlc, tempd, tempf, tref,&
                sigf, vinf, ndsde, dsde, icomp,&
                nvi, nwkout, wkout, codret)
!
    if (codret .gt. 0) goto (1,2), codret
!
! -->   IPAL > 0 --> REDECOUPAGE IMPOSE DU PAS DE TEMPS
! -->   REDECOUPAGE IMPOSE ==>  RETURN DANS PLASTI APRES RECHERCHE
!       DES CARACTERISTIQUES DU MATERIAU A (T) ET (T+DT)
    if (ipal .le. 0) goto 9999
    if (icomp .eq. -1) icomp = 0
!
! --    CAS DE NON CONVERGENCE LOCALE / REDECOUPAGE DU PAS DE TEMPS
!
 1  continue
!
    if (npal .eq. 0) then
        goto 2
    else
        if ((typmod(2).eq.'GRADEPSI') .or. (typmod(2).eq.'GRADVARI')) then
            call u2mesk('A', 'COMPOR2_10', 1, typmod(2))
        endif
    endif
!
    if (icomp .gt. 3) then
        call u2mess('A', 'ALGORITH10_35')
        goto 2
    endif
!
    if (icomp .ge. 1) npal = 2 * npal
    icomp = icomp + 1
!
    do 124 k = 1, npal
! --       INITIALISATION DES VARIABLES POUR LE REDECOUPAGE DU PAS
        if (k .eq. 1) then
            td = instam
            td1 = td
            deltat = (instap - instam) / npal
            tf = td + deltat
            tf1=tf
            if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
                call r8inir(ndsde, 0.d0, dsde, 1)
            endif
            call lceqve(epsdt, eps)
            call lceqve(depst, deps)
            call lcprsv(1.d0/npal, deps, deps)
            call lceqvn(ndt, sigd, sd)
!
! --        REACTUALISATION DES VARIABLES POUR L INCREMENT SUIVANT
        else if (k .gt. 1) then
            td = tf
            td1=td
            tf = tf + deltat
            tf1=tf
            call lcsove(eps, deps, eps)
            call lceqvn(ndt, sigf, sd)
            call lceqvn(nvi, vinf, vind)
        endif
!
!
        call lc0000(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, crit, td, tf,&
                    neps, eps, deps, nsig, sd,&
                    vind, option, angmas, nwkin, wkin,&
                    cp, numlc, tempd, tempf, tref,&
                    sigf, vinf, ndsde, dsdelo, icomp,&
                    nvi, nwkout, wkout, codret)
!
        if (codret .gt. 0) goto (1,2), codret
!
        if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call lcprsm(1.d0/npal, dsdelo, dsdelo)
            call lcsoma(dsde, dsdelo, dsde)
        endif
!
124  continue
    goto 9999
!
 2  continue
    goto 9999
!
9999  continue
end subroutine
