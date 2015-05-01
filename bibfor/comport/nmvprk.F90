subroutine nmvprk(fami, kpg, ksp, ndim, typmod,&
                  imat, comp, crit, timed, timef,&
                  neps, epsdt, depst, sigd, vind,&
                  opt, angmas, sigf, vinf, dsde,&
                  iret)
! aslint: disable=W1504
    implicit none
! ----------------------------------------------------------------------
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
! person_in_charge: jean-michel.proix at edf.fr
!     ================================================================
!     INTEGRATION DE LOIS DE COMPORTEMENT ELASTO-VISCOPLASTIQUE
!     PAR UNE METHODE DE RUNGE KUTTA
!             AVEC    .N VARIABLES INTERNES
!
!     INTEGRATION DES CONTRAINTES           = SIG(T+DT)
!     INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
!
!     CETTE METHODE NE FOURNIT PAS DE MATRICE TANGENTE POUR LA
!     RESOLUTION GLOBALE.
!     ON FOURNIT DONC LA MATRICE DSDE OBTENUE EN ELASTICITE
!     ================================================================
!
!     ARGUMENTS
!
!     IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!           KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!           NDIM   DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!           TYPMOD TYPE DE MODELISATION
!           IMAT   ADRESSE DU MATERIAU CODE
!           COMP   COMPORTEMENT DE L ELEMENT
!                  COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
!                  COMP(2) = NB DE VARIABLES INTERNES
!                  COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
!           OPT    OPTION DE CALCUL A FAIRE
!                          'RIGI_MECA_TANG'> DSDE(T)
!           CRIT   CRITERES  LOCAUX
!                  CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                            (ITER_INTE_MAXI == ITECREL)
!                  CRIT(3) = CRITERE DE PRECISION POUR L INTEGRATION
!                            PAR LA METHODE DE RUNGE KUTTA
!                  CRIT(6) = TYPE D INTEGRATION LOCAL POUR LA LOI DE
!                            COMPORTEMENT (ALGO_INTE)
!           TIMED   INSTANT T
!           TIMEF   INSTANT T+DT
!           EPSDT   DEFORMATION TOTALE A T
!           DEPST   INCREMENT DE DEFORMATION TOTALE
!           SIGD    CONTRAINTE A T
!           VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
!           ANGMAS  3 ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
!     OUT   SIGF    CONTRAINTE A T+DT
!           VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
!           DSDE    MATRICE DE COMPORTEMENT TANGENT ELASTIQUE
!     ----------------------------------------------------------------
!     INFO  MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
!                         (*,2) = CARACTERISTIQUES PLASTIQUES A T
!           MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
!                         (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
!           MATCST        'OUI' SI MATERIAU CST ENTRE T ET T+DT
!                         'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
!                         'NON' SINON
!           NDT            NB DE COMPOSANTES TOTALES DES TENSEURS
!                                  = 6  3D
!                                  = 4  AXIS  C_PLAN  D_PLAN
!           NDI            NB DE COMPOSANTES DIRECTES DES TENSEURS
!           NVI            NB DE VARIABLES INTERNES
!           NR             NB EQUATIONS SYSTEME INTEGRE A RESOUDRE
!     ----------------------------------------------------------------
!     ROUTINE LC....UTILITAIRES POUR INTEGRATION LOI DE COMPORTEMENT
!     ----------------------------------------------------------------
!     ORDRE DES TENSEURS  3D XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ
!                         DP XX YY ZZ SQRT(2)*XY
!                         AX RR ZZ TT SQRT(2)*RZ
!     ----------------------------------------------------------------
!     ATTENTION
!     SI OPT = 'RIGI_MECA_TANG' NE PAS TOUCHER AUX VARIABLES SIGF,VINF
!     QUI N ONT PAS DE PLACE MEMOIRE ALLOUEE
!
!     SIG EPS DEPS  ONT DEJA LEURS COMPOSANTES DE CISAILLEMENT
!     MULTIPLIES PAR RACINE DE 2 > PRISE EN COMPTE DES DOUBLES
!     PRODUITS TENSORIELS ET CONSERVATION DE LA SYMETRIE
!
!     ----------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/calsig.h"
#include "asterfort/gerpas.h"
#include "asterfort/lcdpeq.h"
#include "asterfort/lcmate.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcrkin.h"
#include "asterfort/lcrksg.h"
#include "blas/dcopy.h"
    character(len=*) :: fami
    integer :: imat, ndim, ndt, ndi, nr, nvi, kpg, ksp, i, nbphas, itmax
    integer :: nmat, ioptio, idnr, nsg, nfs, nhsr, neps
    integer :: irr, decirr, nbsyst, decal, gdef
!     POUR POLYCRISTAL, POUR POUVOIR STOCKER JUSQU'A 1000 PHASES
    parameter (nmat =6000)
!     POUR LCMATE (MONOCRISTAL) DIMENSIONS MAX
!        NSG=NOMBRE DE SYSTEMES DE GLISSEMENT MAXIMUM
!        NFS=NOMBRE DE FAMILLES DE SYSTEMES DE GLISSEMENT MAXIMUM
    parameter (nsg=30)
    parameter (nfs=5)
    parameter (nhsr=5)
    integer :: nbcomm(nmat, 3), numhsr(nmat), iret
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), epsdt(neps), depst(neps)
    real(kind=8) :: rbid
    real(kind=8) :: toler, ymfs, crit(*), vind(*), vinf(*), timed, timef
    real(kind=8) :: sigd(6), sigf(6), dsde(6, 6), angmas(*)
    real(kind=8) :: cothe(nmat), dcothe(nmat), pgl(3, 3), epsd(9)
    real(kind=8) :: coeff(nmat), dcoeff(nmat), coel(nmat), dtime, x
!     POUR POLYCRISTAL, 5 MATRICE HSR MAXI. POUR MONOCRISTAL, 1 MAXI
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg, nhsr), detot(9)
    character(len=3) :: matcst
    character(len=8) :: mod, typma, typmod(*)
    character(len=11) :: meting
    character(len=16) :: comp(*), opt, loi
    character(len=24) :: cpmono(5*nmat+1)
    common /tdim/   ndt,    ndi
    common /opti/   ioptio, idnr
    common /meti/   meting
    common/polycr/irr,decirr,nbsyst,decal,gdef
!
! --    INITIALISATION DES PARAMETRES DE CONVERGENCE ET ITERATIONS
!
    itmax = int(crit(1))
    toler = crit(3)
    meting = 'RUNGE_KUTTA'
    mod = typmod(1)
    loi = comp(1)
    gdef = 0
    if (comp(3) .eq. 'SIMO_MIEHE') gdef=1
!
!     YMFS EST UTILISE LORS DU CALCUL D ERREUR COMME MINIMUM DE
!     CHAQUE COMPOSANTE DE VINT. L IDEAL SERAIT DE RENTRER CE
!     PARAMETRE EN DONNEE POUR CHAQUE VARIABLE INTERNE
!
    ymfs = 0.0001d0
!
! --  RECUPERATION COEF(TEMP(T))) LOI ELASTO-PLASTIQUE A T ET/OU T+DT
!                    NB DE CMP DIRECTES/CISAILLEMENT + NB VAR. INTERNES
!
    call lcmate(fami, kpg, ksp, comp, mod,&
                imat, nmat, rbid, rbid, 1,&
                typma, hsr, materd, materf, matcst,&
                nbcomm, cpmono, angmas, pgl, 0,&
                toler, ndt, ndi, nr, crit,&
                nvi, vind, nfs, nsg, toutms,&
                nhsr, numhsr, sigd)
!
    if (opt(1:9) .eq. 'RIGI_MECA') goto 9000
!
    call dcopy(neps, depst, 1, detot, 1)
    call dcopy(neps, epsdt, 1, epsd, 1)
!
    dtime=timef-timed
!
! --  INITIALISATION DES VARIABLES INTERNES A T
!
    do 10 i = 1, nmat
        cothe(i)=materd(i,1)
        dcothe(i)=-cothe(i)+materf(i,1)
10  end do
!
    do 11 i = 1, nmat
        coeff(i)=materd(i,2)
        dcoeff(i)=-coeff(i)+materf(i,2)
11  end do
!
!     INITIALISATIONS PARTICULIERES POUR CERTAINES LOIS
!
    call lcrkin(ndim, opt, comp, materf, nbcomm,&
                cpmono, nmat, mod, nvi, sigd,&
                sigf, vind, vinf, nbphas, iret)
    if (iret .eq. 9) then
!        ENDOMMAGEMENT MAXI AU POINT DE GAUSS
        iret=0
        goto 9999
    endif
!
    call gerpas(fami, kpg, ksp, comp, mod,&
                imat, matcst, nbcomm, cpmono, nbphas,&
                nvi, nmat, vinf, dtime, itmax,&
                toler, ymfs, cothe, coeff, dcothe,&
                dcoeff, coel, pgl, angmas, neps,&
                epsd, detot, x, nfs, nsg,&
                nhsr, numhsr, hsr, iret)
    if (iret .ne. 0) then
        goto 9999
    endif
!
! --  CALCUL DES CONTRAINTES
!
    if ((loi(1:8).eq.'MONOCRIS') .and. (gdef.eq.1)) then
        call lcrksg(comp, nvi, vinf, epsd, detot,&
                    nmat, coel, sigf)
    else
        call calsig(fami, kpg, ksp, vinf, mod,&
                    comp, vinf, x, dtime, epsd,&
                    detot, nmat, coel, sigf)
    endif
!
    call lcdpeq(vind, vinf, comp, nbcomm, cpmono,&
                nmat, nvi, sigf, detot, epsd,&
                materf, pgl)
!
9000  continue
!
!     OPERATEUR TANGENT = ELASTIQUE OU SECANT (ENDOMMAGEMENT)
    if (materf(nmat,1) .eq. 0) then
!
        call lcopli('ISOTROPE', mod, materf(1, 1), dsde)
!
    else if (materf(nmat,1).eq.1) then
!
        call lcopli('ORTHOTRO', mod, materf(1, 1), dsde)
!
    endif
!
!
9999  continue
end subroutine
