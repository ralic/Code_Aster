subroutine lc0000(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, carcri, instam, instap,&
                  neps, epsm, deps, nsig, sigm,&
                  vim, option, angmas, nwkin, wkin,&
                  cp, numlc, tempd, tempf, tref,&
                  sigp, vip, ndsde, dsidep, icomp,&
                  nvi, nwkout, wkout, codret)
!
use calcul_module, only : calcul_status
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/lc0001.h"
#include "asterfort/lc0002.h"
#include "asterfort/lc0003.h"
#include "asterfort/lc0004.h"
#include "asterfort/lc0005.h"
#include "asterfort/lc0006.h"
#include "asterfort/lc0007.h"
#include "asterfort/lc0008.h"
#include "asterfort/lc0009.h"
#include "asterfort/lc0010.h"
#include "asterfort/lc0011.h"
#include "asterfort/lc0012.h"
#include "asterfort/lc0013.h"
#include "asterfort/lc0014.h"
#include "asterfort/lc0015.h"
#include "asterfort/lc0016.h"
#include "asterfort/lc0017.h"
#include "asterfort/lc0018.h"
#include "asterfort/lc0019.h"
#include "asterfort/lc0020.h"
#include "asterfort/lc0021.h"
#include "asterfort/lc0022.h"
#include "asterfort/lc0023.h"
#include "asterfort/lc0024.h"
#include "asterfort/lc0025.h"
#include "asterfort/lc0026.h"
#include "asterfort/lc0027.h"
#include "asterfort/lc0028.h"
#include "asterfort/lc0029.h"
#include "asterfort/lc0030.h"
#include "asterfort/lc0031.h"
#include "asterfort/lc0032.h"
#include "asterfort/lc0033.h"
#include "asterfort/lc0034.h"
#include "asterfort/lc0035.h"
#include "asterfort/lc0036.h"
#include "asterfort/lc0037.h"
#include "asterfort/lc0038.h"
#include "asterfort/lc0039.h"
#include "asterfort/lc0040.h"
#include "asterfort/lc0041.h"
#include "asterfort/lc0042.h"
#include "asterfort/lc0043.h"
#include "asterfort/lc0044.h"
#include "asterfort/lc0045.h"
#include "asterfort/lc0046.h"
#include "asterfort/lc0047.h"
#include "asterfort/lc0048.h"
#include "asterfort/lc0049.h"
#include "asterfort/lc0050.h"
#include "asterfort/lc0051.h"
#include "asterfort/lc0052.h"
#include "asterfort/lc0053.h"
#include "asterfort/lc0054.h"
#include "asterfort/lc0055.h"
#include "asterfort/lc0056.h"
#include "asterfort/lc0057.h"
#include "asterfort/lc0058.h"
#include "asterfort/lc0059.h"
#include "asterfort/lc0060.h"
#include "asterfort/lc0061.h"
#include "asterfort/lc0062.h"
#include "asterfort/lc0063.h"
#include "asterfort/lc0064.h"
#include "asterfort/lc0065.h"
#include "asterfort/lc0066.h"
#include "asterfort/lc0067.h"
#include "asterfort/lc0068.h"
#include "asterfort/lc0069.h"
#include "asterfort/lc0070.h"
#include "asterfort/lc0071.h"
#include "asterfort/lc0072.h"
#include "asterfort/lc0073.h"
#include "asterfort/lc0074.h"
#include "asterfort/lc0075.h"
#include "asterfort/lc0076.h"
#include "asterfort/lc0077.h"
#include "asterfort/lc0078.h"
#include "asterfort/lc0079.h"
#include "asterfort/lc0080.h"
#include "asterfort/lc0081.h"
#include "asterfort/lc0082.h"
#include "asterfort/lc0083.h"
#include "asterfort/lc0084.h"
#include "asterfort/lc0085.h"
#include "asterfort/lc0086.h"
#include "asterfort/lc0087.h"
#include "asterfort/lc0088.h"
#include "asterfort/lc0089.h"
#include "asterfort/lc0090.h"
#include "asterfort/lc0091.h"
#include "asterfort/lc0092.h"
#include "asterfort/lc0093.h"
#include "asterfort/lc0094.h"
#include "asterfort/lc0095.h"
#include "asterfort/lc0096.h"
#include "asterfort/lc0097.h"
#include "asterfort/lc0098.h"
#include "asterfort/lc0099.h"
#include "asterfort/lc0100.h"
#include "asterfort/lc9999.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcpto.h"
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
! aslint: disable=W1501,W1504
!
    integer :: imate, ndim, nvi, kpg, ksp
    integer :: neps, nsig, nwkin, nwkout, ndsde
    real(kind=8) :: carcri(*), angmas(3)
    real(kind=8) :: instam, instap, tempd, tempf, tref
    real(kind=8) :: wkin(nwkin), wkout(nwkout)
    real(kind=8) :: epsm(neps), deps(neps)
    real(kind=8) :: sigm(nsig), sigp(nsig)
    real(kind=8) :: vim(nvi), vip(nvi)
    real(kind=8) :: dsidep(ndsde)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    aster_logical :: cp
    integer :: icomp
    integer :: numlc
    integer :: codret
!
! ======================================================================
!     INTEGRATION DES LOIS DE COMPORTEMENT NON LINEAIRE POUR LES
!     ELEMENTS ISOPARAMETRIQUES EN PETITES OU GRANDES DEFORMATIONS
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
!     ICOMP   : COMPTEUR DE REDECOUPAGE PRODUIT PAR REDECE
!     NVI     : NOMBRE DE VARIABLES INTERNES DU POINT D'INTEGRATION
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
!    ATTENTION  VIM    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
!       ----------------------------------------------------------------

!     VARIABLES LOCALES POUR LE REDECOUPAGE DU PAS DE TEMPS
!             TD      INSTANT T
!             TF      INSTANT T+DT
!             TEMD    TEMPERATURE A T
!             TEMF    TEMPERATURE A T+DT
!             DEPS    INCREMENT DE DEFORMATION TOTALE
!             VD      VARIABLES INTERNES A T    + INDICATEUR ETAT T
!             DSIDEPLO MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!             ICOMP           COMPTEUR POUR LE REDECOUPAGE DU PAS DE
!                                  TEMPS
!             RETURN1 EN CAS DE NON CONVERGENCE LOCALE
!     ----------------------------------------------------------------
!
    character(len=16) :: mult_comp
!     ----------------------------------------------------------------
!     ------------------------------------------------------------------
!
!     NUMLC doit etre compris entre 1 et 100
!
    if (calcul_status() .eq. 3) then
        if (option(1:9) .ne. 'RIGI_MECA') then
!           DEFORMATION MECANIQUE ASSOCIEE A LA VARIABLE DE
!           COMMANDE PTOT. CE CALCUL N'EST POSSIBLE QUE :
!           1 => EN PETITES DEFORMATIONS
!           2 => AVEC UNE LOI MECANIQUE DU KIT THM
            call vrcpto(compor, deps, neps, fami, kpg,&
                        ksp, imate)
        endif
    endif
!
    mult_comp = compor(7)
!
    select case (numlc)
!
    case (1)
!     ELAS
        call lc0001(fami, kpg, ksp, ndim, imate,&
                    neps, deps, nsig, sigm, option,&
                    angmas, sigp, vip, typmod, ndsde,&
                    dsidep, codret)
    case (2)
!     VMIS_ISOT_XXX, VISC_ISOT_XXX
        call lc0002(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, neps,&
                    epsm, deps, nsig, sigm, vim,&
                    option, sigp, vip, typmod, ndsde,&
                    dsidep, codret)
    case (3)
!     VMIS_CINE_LINE, VMIS_ECMI_XXXX
        call lc0003(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (4)
!     VMIS_CINX_CHAB/MEMO VISC_CINX_CHAB/MEMO,
        call lc0004(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (5)
!     ENDO_FRAGILE+GRAD_EPSI
        call lc0005(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkout, typmod, icomp,&
                    nvi, dsidep, codret)
    case (6)
!     ENDO_ISOT_BETON
        call lc0006(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, neps,&
                    epsm, deps, nsig, sigm, vim,&
                    option, angmas, sigp, vip, nwkin,&
                    wkin, typmod, icomp, nvi, ndsde,&
                    dsidep, nwkout, wkout, codret)
    case (7)
!     ENDO_ORTH_BETON
        call lc0007(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (8)
!     MAZARS
        call lc0008(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkout, typmod, icomp,&
                    nvi, dsidep, codret)
    case (9)
!     BETON_REGLE_PR
        call lc0009(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (10)
!     CZM_EXP_REG
        call lc0010(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (11)
!     CZM_LIN_REG
        call lc0011(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (12)
!     CZM_EXP
        call lc0012(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (13)
!     JOINT_BA
        call lc0013(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (14)
!     ROUSSELIER
        call lc0014(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (15)
!     META_XXX
        call lc0015(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (16)
!     DRUCK_PRAGER
        call lc0016(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkout, typmod, icomp,&
                    nvi, dsidep, codret)
    case (17)
!     NORTON_HOFF
        call lc0017(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (18)
!     VISC_TAHERI
        call lc0018(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (19)
!     ELAS_HYPER
        call lc0019(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (20)
        call lc0020(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (21)
        call lc0021(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (22)
        call lc0022(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (23)
        call lc0023(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (24)
        call lc0024(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (25)
!     KIT_DDI : NE PAS UTILISER COMME EXEMPLE
        call lc0025(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, cp,&
                    epsm, deps, sigm, vim, option,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, numlc, dsidep, codret)
    case (26)
        call lc0026(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (27)
        call lc0027(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (28)
        call lc0028(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (29)
        call lc0029(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (30)
!     TEMPD,TEMPF,TREF pour PLASTI, CAR APPEL POSSIBLE EN THM
!     NE PAS UTILISER COMME EXEMPLE
        call lc0030(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, tempd, tempf, tref,&
                    wkin, typmod, icomp, nvi, dsidep,&
                    codret)
    case (31)
        call lc0031(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, neps,&
                    epsm, deps, sigm, vim, option,&
                    angmas, sigp, vip, wkin, typmod,&
                    icomp, nvi, dsidep, codret)
    case (32)
!     TEMPD,TEMPF,TREF pour PLASTI, CAR APPEL POSSIBLE EN THM
!     NE PAS UTILISER COMME EXEMPLE
        call lc0032(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, neps,&
                    epsm, deps, sigm, vim, option,&
                    angmas, sigp, vip, tempd, tempf,&
                    tref, wkin, typmod, icomp, nvi,&
                    dsidep, codret)
    case (33)
!     TEMPD,TEMPF,TREF pour PLASTI, CAR APPEL POSSIBLE EN THM
!     NE PAS UTILISER COMME EXEMPLE
        call lc0033(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, tempd, tempf, tref,&
                    wkin, typmod, icomp, nvi, dsidep,&
                    codret)
    case (34)
        call lc0034(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (35)
        call lc0035(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (36)
        call lc0036(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)

    case (37)
        call lc0037(fami, kpg, ksp, ndim, imate,&
                    compor, mult_comp, carcri, instam, instap, neps,&
                    epsm, deps, sigm, vim, option,&
                    angmas, sigp, vip, tempd, tempf,&
                    tref, wkin, typmod, icomp, nvi,&
                    dsidep, codret)
    case (38)
        call lc0038(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (39)
        call lc0039(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (40)
        call lc0040(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (41)
        call lc0041(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (42)
        call lc0042(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (43)
        call lc0043(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (44)
        call lc0044(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (45)
        call lc0045(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (46)
!     ENDO_SCALAIRE
        call lc0046(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, neps,&
                    epsm, deps, nsig, sigm, vim,&
                    option, angmas, sigp, vip, nwkin,&
                    wkin, typmod, icomp, nvi, ndsde,&
                    dsidep, nwkout, wkout, codret)
    case (47)
!     ENDO_HETEROGENE
        call lc0047(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, wkout, typmod,&
                    icomp, nvi, dsidep, codret)
    case (48)
        call lc0048(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (49)
        call lc0049(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (50)
!     UMAT
        call lc0050(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, carcri, instam, instap,&
                    neps, epsm, deps, nsig, sigm,&
                    nvi, vim, option, angmas, nwkin,&
                    wkin, icomp, sigp, vip, ndsde,&
                    dsidep, nwkout, wkout, codret)
    case (51)
        call lc0051(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (52)
        call lc0052(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (53)
!     ENDO_CARRE
        call lc0053(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (54)
        call lc0054(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (55)
        call lc0055(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (56)
        call lc0056(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (57)
        call lc0057(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, neps,&
                    epsm, deps, nsig, sigm, vim,&
                    option, angmas, sigp, vip, nwkin,&
                    wkin, typmod, icomp, nvi, ndsde,&
                    dsidep, nwkout, wkout, codret)
    case (58)
!     MFRONT
        call lc0058(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, carcri, instam, instap,&
                    neps, epsm, deps, nsig, sigm,&
                    nvi, vim, option, angmas, nwkin,&
                    wkin, icomp, sigp, vip, dsidep,&
                    codret)
    case (59)
        call lc0059(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (60)
        call lc0060(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (61)
        call lc0061(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (62)
        call lc0062(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (63)
        call lc0063(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (64)
        call lc0064(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (65)
        call lc0065(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (66)
        call lc0066(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (67)
        call lc0067(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (68)
        call lc0068(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (69)
        call lc0069(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (70)
        call lc0070(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (71)
        call lc0071(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (72)
        call lc0072(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (73)
        call lc0073(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (74)
        call lc0074(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (75)
        call lc0075(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (76)
        call lc0076(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (77)
        call lc0077(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (78)
        call lc0078(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (79)
        call lc0079(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (80)
        call lc0080(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (81)
        call lc0081(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (82)
        call lc0082(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (83)
        call lc0083(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (84)
        call lc0084(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (85)
        call lc0085(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (86)
        call lc0086(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (87)
        call lc0087(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (88)
        call lc0088(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (89)
        call lc0089(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (90)
        call lc0090(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (91)
        call lc0091(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (92)
        call lc0092(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (93)
        call lc0093(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (94)
        call lc0094(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (95)
        call lc0095(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (96)
        call lc0096(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (97)
        call lc0097(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (98)
        call lc0098(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (99)
        call lc0099(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (100)
        call lc0100(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case (9999)
        call lc9999(fami, kpg, ksp, ndim, imate,&
                    compor, carcri, instam, instap, epsm,&
                    deps, sigm, vim, option, angmas,&
                    sigp, vip, wkin, typmod, icomp,&
                    nvi, dsidep, codret)
    case default
        call utmess('F', 'COMPOR1_43', si=numlc)
    end select
!
end subroutine
