subroutine brag01(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, instam, instap, sechm,&
                  sechp, tm, tp, tref, epsm,&
                  deps, sigm, vim, option, sigp,&
                  vip, dsidep, devpt, fluor)
!
!        ROUTINE ANCIENNEMENT NOMMEE BETON3DV13 PUIS BETRAG1
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!        E.GRIMAL JUILLET 2006
!     CORREC = ABREVIATION DE CORRECTION
!    CORREC 5/5/04 AS BCH=1 DS SIG_APP PCH ET PW SONT EFFECTIVES
!          AU SENS DE L ENDOMMAGEMENT
!    CORREC 25/5/04 AS E1D EST BIEN LA PARTIE REVERSIBLE DE LA
!          DEFORMATION DEVIATORIQUE ET E2D LA PARTIE VISCOPLASTIQUE
!    CORREC 24/6/04 AS FLUAGE CALCULE AVEC PW=-PC ET SIGMA EFF AVEC
!          PW*BW & BW=SR A PRECISER DS LES DONNEES MATERIAUX
!          PW BRANCHï¿œSUR LES 2 AMORTISSEURS SPHï¿œIQUES, 2CAS DE
!          FLUAGE SPHERIQUE AVEC OU SANS K2 SUIVANT LE SIGNE DE E2*DE2
!    CORREC 25/6/04 AS DS SIG_APP PW DOIT ETRE POSITIF POUR INTERVENIR
!          DANS LES CONTRAINTES EFFECTIVES
!    CORREC 6/7/04 AS MEME SCHEMA RHEOLOGIQUE EN DEV ET SPH SANS
!          PRESSION ET SANS CONSOLIDATION
!    CORREC 6/7/04 AS I1 REMPLACE PAR NEG(I1) DS LA CRITERE DE DRUCKER
!          PRAGER
!    CORREC 21/07/2004 AS RETOUR AU MONTAGE FRACTAL SUR LE FLUAGE AVEC
!          DEPRESSION CAPILLAIRE SPHERIQUE ET SURPRESSION GLOBALE
!          + RAJOUT DES 2+12 VARIABLES INTERNES SEUILS VISCOPLASTIQUES
!          DES ETAGES 2 (SEUIL PLASTIQUE SUR K2*E2)
!    CORREC 7/09/2004 AS PRESSION D EAU A COMPORTEMENT UNILATERAL
!          MACROSCOPIQUE DANS SIG_APP
!    CORREC 23/9/04 AS INTï¿œRATIONDE L' ENDOMMAGEMENT DE FLUAGE ET
!          COMPORTEMENT UNILATERAL EN SPHERIQUE
!    CORREC 27/9/04 AS ATTENUATION DES CARACTERISTIQUES DE FLUAGE
!          EN TRACTION (RFST)=FL T/FL C
!    CORREC 29/9/04 AS RAPPORT IRREVERSIBLE /REVERSIBLE REGLABLE
!          EN TRACTION  (KIST)=K2T/K1T=MU2T/MU1T
!    CORREC 8/10/04 AS SUPPRESSION DU CARACTERE UNILATERAL
!          SUR LA RHEOLOGIE
!          + RAJOUT DE LA DEFORMATION VISCOPLASTIQUE ANISOTROPES
!          DE TRACTION DANS LE CALCUL DES CONTRAINTES EFFECTIVES
!    CORREC 01/2005 EG MODIFICATION DE L'ENDO DE COMPRESSION
!          (PRISE EN COMPTE DES CONTRAINTES DE COMPRESSION)
!    CORREC 01/2005 EG RAJOUT DES VARIABLE SUIVANTE :
!          W0 (TENEUR EN EAU INITIALE)
!          EVPMAX (DEFORMATION DE FLUAGE MAXI)
!          SVFT(CONTRAINTE MODIFIANT LA VITESSE DE FLUAGE DE TRACTION)
!    CORREC 01/2005 EG MODIFICATION DE
!          PCH=(A-A0)+ * MCH * (VG-B(TR.EPS))+
!    CORREC 04/2005 EG MODIFICATION DE LOI AVANCEMENT PRISE EN COMPTE
!          D'UN SEUIL DE SR MINI POUR QUE RAG SOIT POSSIBLE
!    CORREC 02/2006 EG MODIFICATION LOI DU FLU_ORTO : SUPPRESSION DE
!          SVFT ET MODIFICATION E.VDT
!
!     TI TEMPS AU DEBUT DU PAS
!     TF TEMPS FIN DU PAS
!     EPSI DEFORMATION TOTALE SANS DEFORMATION THERMIQUE EN DEBUT
!          DE PAS
!     EPSF IDEM EN FIN DE PAS
!     XMAT00  CONSTANTES DU MATï¿œIAUX DANS L'ORDRE DES Dï¿œLARATIONS
!          DANS (IDVISC.ESO POUR CASTEM)(DS...POUR ASTER)
!     NMAT00 NOMBRE DE PARAMETREE MATERIAU=NBR MAXI DECLARE
!          DS IDVISC (NMAT0+12) +NBR POUR LA FORMU-
!          LATION ELASTIQ ISO (NMAT=4)+ CONSTANTES DS BETON3D (7)
!     VIM VARIABLES INTERNES AU DEBUT DU PAS CLASSEE CONFORMEMENT A LA
!          DECLARATION DS (IDVAR4.ESO POUR CASTEM) (...POUR ASTER)
!     VIP IDEM ï¿œLA FIN DU PAS
!     NVARI NOMBRE DE VARIBLES INTERNES
!          (31 D APRES DECLARATION DS IDVAR40.ESO)
!     SIGMA CONTRAINTES ï¿œLA FIN DU PAS
!     LOCAL INDICATEUR LOGIQUE POUR LE TRAITEMENT LOCAL DE LA
!          LOCALISATION (SI .TRUE. BESOIN DE LA TAILLE DES ELEMENTS...
!     CORREC BW=SR 26/09/05
!          COMPORTEMENT VISCO-ELASTIQUE ENDOMMAGEABLE ANISOTROPE
!          TRIDIM DU BETON (MODELE SIMPLIFIE TOTALEMENT EXPLICITE)
!
! aslint: disable=,W1504
    implicit none
!      IMPLICIT REAL*8(A-Z)
#include "asterc/r8pi.h"
#include "asterfort/brdefv.h"
#include "asterfort/brendo.h"
#include "asterfort/brfluo.h"
#include "asterfort/brseff.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    integer :: ndim, imate
    character(len=16) :: compor(3), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    real(kind=8) :: vim(65), vip(65)
    real(kind=8) :: instam, instap
    real(kind=8) :: sechm, sechp
    real(kind=8) :: sigm(6), deps(6), epsm(6)
    real(kind=8) :: sigp(6), dsidep(6, 6)
    real(kind=8) :: devpt(6)
    integer :: fluor, kpg, ksp
! LE NOMBRE DE PARAMTRES MATERIAUX EST DE 32 ET PAS 31
!     (CF. DEFI_MATERIAU.CAPY)
!
! SEULS LES VERITABLES PARAMETRES MATERIAUX ONT ETE CONSERVES
! ATTENTION : SECH DESIGNE LA SATURATION DU MATERIAU
!
!      CHARACTER*8  NOMRES(32),NOMPAR(1)
    character(len=8) :: nomres(33)
    integer :: icodre(33)
    real(kind=8) :: valres(33)
    real(kind=8) :: rbid, valr(4)
    integer :: i, j
    real(kind=8) :: dt
! CARATERISTIQUES THERMO-ELASTIQUES
    real(kind=8) :: e0, nu0, mu0, k0
!
! CARACTERISTIQUES DU FLUAGE
    logical(kind=1) :: fluage, pressn
!      LOGICAL CONSO
    real(kind=8) :: k1, eta1s, eta2s, mu1, eta1d, eta2d
    real(kind=8) :: k2, mu2
!      REAL*8  SVFT
    real(kind=8) :: eps0, tau0, evpmax
    real(kind=8) :: epsti(6), epstf(6)
!      REAL*8  EKD1,EKD2
    real(kind=8) :: ded
! CARACTERISTIQUES DE L'ENDOMMAGEMENT
    real(kind=8) :: rc, rt, delta, edpict, edpicc
    real(kind=8) :: xmc, xmt, dpicc0, dpict0
    real(kind=8) :: lcc, lct
    real(kind=8) :: sut, suc
    real(kind=8) :: se2sp1, se2dv6(6)
! CARACTERISTIQUES DES COUPLAGES FLUIDE/SQUELETTE ET GEL/SQUELETTE
    real(kind=8) :: bw, mw, w0
    real(kind=8) :: avg, bvg, sr, tpc, pw3, mm, pwc, bwmax
    real(kind=8) :: pch, ww, xx, pw2, pc, aa, bb, des
!      REAL*8  PW1
! CARACTERISTIQUES DE LA FORMATION DES GELS
    real(kind=8) :: vg, a0, alp0, ea, r, sr0
    real(kind=8) :: x00, f00, avrag, bch, mch, bchmax, eragmx
    real(kind=8) :: arag, aragm, aragp
    real(kind=8) :: tm, tp, tref, tmabs, tpabs, trfabs
! VARIABLES INTERNES
    real(kind=8) :: e1si, e1sf, e2si, e2sf
    real(kind=8) :: e2min, e2max
    real(kind=8) :: e1di(6), e1df(6)
    real(kind=8) :: e2di(6), e2df(6)
    real(kind=8) :: sigeff(6), sige6(6), sigec6(6), siget6(6)
    real(kind=8) :: dt6(6), dc, sut6(6), suc1, dt66(6, 6)
    real(kind=8) :: evp06(6), evp16(6), bt6(6), bc1
! VARIABLES DE CALCUL
    real(kind=8) :: t1, t2, t4, t7, t8, t10, t13, t19, t12, pi
    integer :: i0
!      REAL*8  PW2,PCH,ARAGM,ARAGP
!
! TAILLE ELEMENT
    real(kind=8) :: t33(3, 3), n33(3, 3)
!
    real(kind=8) :: epsi(6), epsf(6)
    logical(kind=1) :: local
!
    real(kind=8) :: yyi, yyf
    real(kind=8) :: e0df(6)
    real(kind=8) :: e0sf
!
!*********************************************************************
!C     VARIABLE LOGIQUE DE CONSOLIDATION DEVIATORIQUE
!      CONSO=.FALSE.
!C     VARIABLE LOGIQUE POUR PRISE EN COMPTE DU FLUAGE
!      FLUAGE=.TRUE.
!
!*********************************************************************
!       PRINT *,'FLUOR',FLUOR
!       IF (FLUOR.EQ.3) THEN
!       I0=0
!       DO I=1,64
!       T12=(1.+VIM(I0+I))**2.
!        IF (T12.GT.0.) THEN
!         IF (T12.GT.1000.0) THEN
!          PRINT *,'VIM(',I0+I,')=',VIM(I0+I)
!         ENDIF
!        ELSE
!         PRINT*,'VIM(',I0+I,')=',VIM(I0+I)
!        ENDIF
!       END DO
!       ENDIF
!*********************************************************************
!
!  **** CHARGEMENT DES PARAMETRES MATERIAU ****
!  ------- CARACTERISTIQUES ELASTIQUES
    nomres(1)='E'
    nomres(2)='NU'
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 2)
!
!        MODULES INSTANTANES ISOTROPES
    e0 = valres(1)
    nu0 = valres(2)
    k0 = e0/3.d0/(1.d0-2.d0*nu0)
    mu0 = e0/2.d0/(1.d0+nu0)
!
!  ------- CARACTERISTIQUES DE FLUAGE
!
    nomres(1) = 'ACTIV_FL'
    nomres(2) = 'K_RS'
    nomres(3) = 'K_IS'
    nomres(4) = 'ETA_RS'
    nomres(5) = 'ETA_IS'
    nomres(6) = 'K_RD'
    nomres(7) = 'K_ID'
    nomres(8) = 'ETA_RD'
    nomres(9) = 'ETA_ID'
    nomres(10) = 'EPS_0'
    nomres(11) = 'TAU_0'
    nomres(12) = 'EPS_FL_L'
!
!  ------- CARACTERISTIQUES DE L'ENDOMMAGEMENT
!
    nomres(13) = 'ACTIV_LO'
    nomres(14) = 'F_C'
    nomres(15) = 'F_T'
    nomres(16) = 'ANG_CRIT'
    nomres(17) = 'EPS_COMP'
    nomres(18) = 'EPS_TRAC'
    nomres(19) = 'LC_COMP'
    nomres(20) = 'LC_TRAC'
!
!  --- CARACTERISTIQUES DU COUPLAGE FLUIDE/SQUELETTE ET GEL/SQUELETTE
!
    nomres(21) = 'A_VAN_GE'
    nomres(22) = 'B_VAN_GE'
    nomres(23) = 'BIOT_EAU'
    nomres(24) = 'MODU_EAU'
    nomres(25) = 'W_EAU_0'
    nomres(26) = 'HYD_PRES'
!
!
!  ------ CARACTERISTIQUES DE LA FORMATION DES GELS
!
    nomres(27) = 'BIOT_GEL'
    nomres(28) = 'MODU_GEL'
    nomres(29) = 'VOL_GEL'
    nomres(30) = 'AVANC_LI'
    nomres(31) = 'PARA_CIN'
    nomres(32) = 'ENR_AC_G'
    nomres(33) = 'SEUIL_SR'
!
!  ------- AFFECTATION DES PARAMETRES DE FLUAGE
!
!          FLUAGE : 1  PRISE EN COMPTE DU FLUAGE
!                  AUTRE  PAS DE FLUAGE
!          CONSO : 1  PRISE EN COMPTE DE LA CONSOLIDATION
!                 AUTRE  PAS DE CONSOLIDATION
!          LOC : 1 PRISE EN COMPTE LOCALISATION
!
    rbid=0.d0
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'BETON_RAG', 0, ' ', [rbid],&
                33, nomres, valres, icodre, 2)
!
    if (valres(1) .eq. 1.0d0) then
        fluage=.true.
    else
        fluage=.false.
    endif
! PLUS DE LOI DE CONSOLIDATION
!      IF (VALRES(2).EQ.1.0d0) THEN
!      CONSO=.TRUE.
!      ELSE
!      CONSO=.FALSE.
!      ENDIF
!      FLUAGE = VALRES(1)
!      CONSO = VALRES(2)
!  MODULES DE COMPRESSIBILITE DIFFERES
!  (K1 POUR LA PARTIE REVERSIBLE ET K2 IRREVERSIBLE)
    k1 = valres(2)
    k2 = valres(3)
! VISCOSITES SPHERIQUES
! (ETA1S POUR LA PARTIE REVERSIBLE ET ETA2S IRREVERSIBLE)
    eta1s = valres(4)
    eta2s = valres(5)
!        MODULES DE CISAILLEMENT DIFFERES
    mu1 = valres(6)
    mu2 = valres(7)
! VISCOSITES DEVIATORIQUES
! (ETA1D POUR LA PARTIE REVERSIBLE ET ETA2D IRREVERSIBLE)
    eta1d = valres(8)
!        VISCOSITE FLUAGE DEVIATORIQUE IRREVERSIBLE
    eta2d = valres(9)
!        EXPOSANT DE LA LOIS DE CONSOLIDATION
!      EKD1 = VALRES(10)
!        DEF CARACT CONSOLIDATION DEVIATORIQUE ETAGE 1 ET 2
!      EKD2 = VALRES(11)
! DEFORMATION CARACTERISTIQUE
! DE VISCOPLASTICITE COUPLE ENDOMMAGEMENT DE TRACTION
    eps0 = valres(10)
!     TEMPS CARACTERISTIQUE DU FLAUGE ORTHOTROPE DE TRACTION
    tau0 = valres(11)
! DEFORMATION LIMITE DE FLUAGE ORTHOTROPE DE TRACTION
    evpmax = valres(12)
!  ------- AFFECTATION DES PARAMETRES D'ENDOMMAGEMENT
! ACTIVATION DE LA LOCALISATION
    if (valres(13) .eq. 1.0d0) then
        local=.true.
    else
        local=.false.
    endif
!     RESISTANCE DU BETON
    rc = valres(14)
    rt = valres(15)
!        CARACTERISTIQUES DU CRITERE DE COMPRESSION
!     ANGLE DU CRITERE DE DRUCKER PRAGER
!     + CONVERSION DE DEGRES EN RADIANS
    delta = valres(16)
    pi = r8pi()
    delta = delta*pi/180.d0
!        EXPOSANT DES LOIS D ENDOMMAGEMENT
    edpicc = valres(17)
    edpict = valres(18)
    dpicc0=1.d0-(rc/(e0*edpicc))
    dpict0=1.d0-(rt/(e0*edpict))
!
!
!      COHESION DES LOIS D'ENDOMMAGEMENT
!      FORTRAN POUR MT
    t1 = (nu0 ** 2)
    t7 = log((1d0 - dpict0))
    t12 = (1-nu0-2.d0*t1+2*t1*dpict0)/t7/(-1.d0+nu0)
    xmt=t12
    if (xmt .gt. 2.5d0) then
        xmt=2.5d0
    endif
!      FORTRAN POUR SUT
    t1 = nu0 ** 2
    t2 = 2 * t1
    t4 = 2 * t1 * dpict0
    t8 = log(1 - dpict0)
    t10 = log(-t8 * xmt)
    t13 = exp(t10 / xmt)
    t19 = rt*(1-nu0-t2+t4)/t13/(1-nu0-t2-dpict0+ nu0 * dpict0 + t4)
    sut=t19
!
!      FORTRAN POUR MC
    xmc=-1.d0/log(1.d0-dpicc0)
!      FORTRAN POUR SUC
    suc=rc*(sqrt(3.d0)-delta)/3.d0/exp(-1.d0/xmc)
!
!
!        LONGUEUR INTERNES EN TRACTION ET COMPRESSION
    lcc = valres(19)
    lct = valres(20)
!
! CHARGEMENT DE LA TAILLE DES ELEMENTS
! TRAITEMENT LOCAL LI=1, LC DONNE LE RAPPORT 1/LC####
! EXEMPLE SI LC=0.30 POUR LI=0.5, LI/LC=1.667 DONC LA LC=0.60
    do 50 i = 1, 3
        do 40 j = 1, 3
            t33(i,j) = 0.0d0
            n33(i,j) = 0.0d0
40      continue
50  end do
    do 60 i = 1, 3
        t33(i,i) = 1.0d0
        n33(i,i) = 1.0d0
60  end do
!  ---- CARACTERISTIQUES DU COUPLAGE FLUIDE/SQUELETTE ET GEL/SQUELETTE
!
!        MILIEU NON SATURE PARAMETRES DE VAN GENUCHTEN
    avg = valres(21)
    bvg = valres(22)
!        NOMBRE DE BIO ET MODULE DE BIO DE L'EAU
    bwmax = valres(23)
    mw = valres(24)
! TENEUR EN EAU INITIALE
    w0 = valres(25)
    if (w0 .eq. 0.d0) then
        w0 = 1.0d0
    endif
! INDICATEUR DE CALCUL PAR PRESSION IMPOSé
    if (valres(26) .eq. 1.d0) then
        pressn=.true.
    else
        pressn=.false.
    endif
!        DEGRE DE SATURATION
    if (pressn) then
!      PRINT *,'LE CALCUL EST DIRECTEMENT EN PRESSION'
        pw3 = (0.5d0*(sechm+sechp))
        if (pw3 .lt. 0.d0) then
            pwc=-pw3
            mm=1/bvg
            xx=(1+(pwc/avg)**(1/(1-mm)))**(-mm)
            sr=xx
        else
            sr=1.d0
        endif
    else
!      PRINT *,'LE CALCUL SE FAIT EN CONCENTRATION'
        sr = (0.5d0*(sechm+sechp))/w0
    endif
    if (sr .gt. 1.0001d0) then
        valr(1) = sr
        valr(2) = sechm
        valr(3) = sechp
        valr(4) = w0
        call utmess('A', 'COMPOR1_63', nr=4, valr=valr)
        sr = 1.d0
    endif
    if (sr .lt. 0.0d0) then
        valr(1) = sr
        valr(2) = sechm
        valr(3) = sechp
        valr(4) = w0
        call utmess('A', 'COMPOR1_64', nr=4, valr=valr)
        sr = 0.d0
    endif
!
!
!  ------ AFFECTATION DES PARAMETRESRELATIFS A LA FORMATION DES GELS
!        NOMBRE DE BIOT ET MODULE DE BIOT DE LA PHASE CHIMIQUE NEOFORMEE
    bchmax = valres(27)
    mch = valres(28)
!      VOLUME DE GEL MAXI POUVANT ETRE CREE
    vg = valres(29)
!      AVANCEMENT LIBRE
    a0 = valres(30)
!      PARAMETRE DE CINETIQUE
    alp0 = valres(31)
!      ENERGIE D'ACTIVATION DES GELS
    ea = valres(32)
    r = 8.12d0
!      SEUIL SR MINI POUR QUE LA RAG SOIT POSSIBLE
    sr0 = valres(33)
! CALCU DE EPSI ET EPSF
! ATTENTION ON TRAVAILLE AVEC DES GAMMA POUR I>3
    do 70 j = 1, 6
        epsti(j)=epsm(j)
        epstf(j)=epsm(j)+deps(j)
70  end do
    do 80 j = 4, 6
        epsti(j)=epsti(j)*sqrt(2.d0)
        epstf(j)=epstf(j)*sqrt(2.d0)
80  end do
!     APPORTS DE MASSE NORMALISES  ********** A CORRIGER ***********
!      WW=XMAT(22)
    ww = 0.0d0
    tmabs=tm+273.15d0
    tpabs=tp+273.15d0
    trfabs=tref+273.15d0
    tpc = 0.5d0*(tmabs+tpabs)
! ATTENTION IL FAUT TREF
!
    if (instam .eq. 0.d0) then
        aragm=0.d0
    else
        aragm=vim(31)
    endif
! CALCUL DE L'AVANCEMENT DE LA RAG
!
!      X00=-1*ALP0*EXP((EA/R)*(1/TRFABS-1/TPC))*SR*(INSTAP-INSTAM)
    xx=sr-sr0
    xx=0.5d0*(xx+abs(xx))
    xx=xx/(1-sr0)
    x00=-1*alp0*exp((ea/r)*(1/trfabs-1/tpc))*xx*(instap-instam)
!
    f00=exp(x00)
    if ((sr-aragm) .gt. 0.d0) then
        aragp=sr-(sr-aragm)*f00
    else
        aragp=aragm
    endif
    if (aragp .gt. aragm) then
        vip(31)=aragp
    else
        vip(31)=aragm
    endif
!
! CALCUL DE L'AVANCEMENT AU MILIEU DU PAS DE TEMPS
    arag=0.5d0*(aragm+aragp)
!
!
!       *** INITIALISATION PAS DE TEMPS
!
    dt=instap-instam
!
!     *** PRESSIONS ***********************************
!
!     **** EVOLUTION DE LA DEFORMATION SPHERIQUE ****
!
!        CALCUL DE DV/V=TR(EPSF) OU TR(EPSI)
    yyf=0.d0
    yyi=0.d0
    do 90 i = 1, 3
        yyf=yyf+epstf(i)
        yyi=yyi+epsti(i)
!       PRINT*,EPSTI(I),EPSTF(I)
90  end do
!      PRINT*, YYI,YYF
!
!     ON PEUT AMELIORER EN SUPPOSANT PW(T)=APW*T+BPW DANS
!     L'INTEGRATION DES EQUATIONS VISCO ELASTIQUES....SPHERIQUES
!
!        CALCUL DE LA PRESSION CHIMIQUE AU MILIEU DU PAS DE TEMPS
!      XX=VG-BCH*(YYF+YYI)/2.0
! MODIFICATION DE BCH POUR ETRE COMME LA POROMéCANIQUE
    bch=bchmax
!      BCH=BWMAX*(1-(0.5*((A0-ARAG)+ABS(A0-ARAG))))
!      BCH=BWMAX*ARAG
    xx=(a0*vg)+bch*((yyf+yyi)/2.d0)
    xx=0.5d0*(xx+abs(xx))
    avrag=(arag*vg-xx)
!      AVRAG=(ARAG-A0)
    avrag=0.5d0*(avrag+abs(avrag))
!     PCH=AVRAG*MCH*XX
    pch=avrag*mch
!      XX=WCH-BCH*YYF
!      XX=0.5d0*(XX+ABS(XX))
!      IF (ARAGM.GT.A0) THEN
!       PCH=MCH*XX
!      ELSE
!       PCH=0.0d0
!      ENDIF
!
!  CALCUL DE TR EPS RAG MAXIMUM
    xx=(arag*vg-a0*vg)
    xx=0.5d0*(xx+abs(xx))
    if (bch .eq. 0.d0) then
        eragmx=0.0d0
    else
        eragmx=xx/bch
    endif
!      PRINT*,'ERAGMX',ERAGMX
!
!
!     CALCUL DE LA PRESSION HYDRIQUE (SI HORS CADRE HM)AVEC LA DEF
!     A LA FIN DU PAS DE TEMPS (PW2) ET AU MILIEU (PW1)
!     POUR AVOIR UNE MEILLEURE PRECISION SUR LE RETRAIT
    bw=sr*bwmax
    if (sr .gt. 0.99999d0) then
        xx=(ww-bw*0.5d0*(yyf+yyi))
        xx=0.5d0*(xx+abs(xx))
!       PW1=MW*XX
        xx=(ww-bw*(yyf))
        xx=0.5d0*(xx+abs(xx))
        pw2=mw*xx
    else
        if (sr .lt. 1d-3) then
            sr=1.d-3
        endif
!        MILIEU NON SATURE ON UTILISE LA RELATION DE VAN GENUCHTEN
!        POUR CALCULER LA PRESSION CAPILLAIRE (EN MPA)
        pc=avg*(sr**(-bvg)-1.d0)**(1.d0-1.d0/bvg)
!
!   ET ON SUPPOSE QUE PC N AGIT PLUS DIRECTEMENT SUR LEMILIEU EXTERIEUR
!   MAIS UNIQUEMENT PAR DEPRESSION CAPILLAIRE SUR LES ETAGES VISQUEUX
!   SPHERIQUES
        pw2=-pc
!       PW1=0.5d0*(PW2+VIM(29))
!      ENDOMMAGEMENT MICRO PAR DEPRESSION CAPILLAIRE
!      IL SERA FONCTION DE L ETAT DE CONTRAINTE EFFECTIVE SPHERIQUE
    endif
!     STOCKAGE DES PRESSIONS  A LA FIN DU PAS DE TEMPS DANS LES VIM
    vip(29)=pw2
    vip(30)=pch
    vip(65)=bch*pch
!
    if (fluage) then
!
!     **** FLUAGE ORTHOTROPE DE TRACTION *************
!
!     SEUIL D ENDOMMAGEMENT DE TRACTION DU PAS PRECEDENT
        sut6(1)=vim(22)
        sut6(2)=vim(23)
        sut6(3)=vim(24)
        sut6(4)=vim(25)
        sut6(5)=vim(26)
        sut6(6)=vim(27)
!     CONTRAINTES EFFECTIVES DU PAS PRECEDENT
        sige6(1)=vim(46)
        sige6(2)=vim(47)
        sige6(3)=vim(48)
        sige6(4)=vim(49)
        sige6(5)=vim(50)
        sige6(6)=vim(51)
!     DEFORMATIONS VISCOPLASTIQUES AU PAS PRECEDENT
        evp06(1)=vim(52)
        evp06(2)=vim(53)
        evp06(3)=vim(54)
        evp06(4)=vim(55)
        evp06(5)=vim(56)
        evp06(6)=vim(57)
        if (fluor .eq. 1) then
            call brfluo(sut, sut6, xmt, sige6, eps0,&
                        tau0, dt, evp06, evp16, devpt,&
                        evpmax, (bch*pch), eragmx)
            do 100 i = 1, 6
                evp16(i)=evp06(i)+devpt(i)*dt
100          continue
        endif
!
        if (fluor .eq. 2) then
            call brfluo(sut, sut6, xmt, sige6, eps0,&
                        tau0, dt, evp06, evp16, devpt,&
                        evpmax, (bch*pch), eragmx)
            goto 210
        endif
!
        if (fluor .eq. 3) then
            do 110 i = 1, 6
                evp16(i)=evp06(i)+devpt(i)*dt
110          continue
        endif
!   STOCKAGE DES DEFORMATIONS VISCOPLASTIQUES DE TRACTION EN FIN DE PAS
!
        vip(52)=evp16(1)
        vip(53)=evp16(2)
        vip(54)=evp16(3)
        vip(55)=evp16(4)
        vip(56)=evp16(5)
        vip(57)=evp16(6)
!     **** PARTITION DU TENSEUR DES DEFORMATIONS POUR LA RHEOLOGIE ****
        do 120 i = 1, 6
            epsi(i)=epsti(i)-evp06(i)
            epsf(i)=epstf(i)-evp16(i)
120      end do
!
!     *** FLUAGE SPHERIQUE *****************************
!
!        CALCUL DE DV/V=TR(EPSF) OU TR(EPSI)
        yyf=0.d0
        yyi=0.d0
        do 130 i = 1, 3
            yyf=yyf+epsf(i)
            yyi=yyi+epsi(i)
130      end do
!
!        CHARGEMENT DES VARIABLES INTERNES POUR LA PARTIE SPHERIQUE
        e1si=vim(1)
        e2si=vim(2)
        e2min=vim(32)
        e2max=vim(33)
!        EVOLUTION DES DEFORMATIONS VISCO-ELASTIQUES SPHERIQUE
        aa=yyf-yyi
        bb=yyi
        des=aa
        if (dt .eq. 0.d0) then
            aa=0.d0
            bb=yyf
        else
            aa=aa/dt
        endif
!      PRINT*,''
!      PRINT*,'E1SI',E1SI,'E2SI',E2SI,'AA',AA,'DT',DT
!      PRINT*,'BB',BB,'K0',K0,'K1',K1,'ETA1S',ETA1S
!      PRINT*,'K2',K2,'ETA2S',ETA2S,'PW1',PW1,'E0SF',E0SF
!      PRINT*,'E1SF',E1SF,'E2SF',E2SF,'E2MIN',E2MIN
!      PRINT*,'E2MAX',E2MAX,'SE2SPH1',SE2SP1
!      CALL BRDEFV(E1SI,E2SI,AA,DT,BB,K0,K1,ETA1S,
!     #K2,ETA2S,PW1,E0SF,E1SF,E2SF,E2MIN,E2MAX,SE2SP1)
        call brdefv(e1si, e2si, aa, dt, bb,&
                    k0, k1, eta1s, k2, eta2s,&
                    0.0d0, e0sf, e1sf, e2sf, e2min,&
                    e2max, se2sp1)
!
!      PRINT*,''
!      PRINT*,'FLUAGE SPHERIQUE'
!      PRINT*,AA,BB
!      PRINT*,'E0SF',E0SF,' E1SF',E1SF,' E2SF',E2SF
!      READ(*,*)
!        MISE A JOUR DES VARIABLES INTERNES POUR LA PARTIE SPHERIQUE
        vip(1)=e1sf
        vip(2)=e2sf
        vip(32)=e2min
        vip(33)=e2max
!
!     *** CONSOLIDATION FLUAGE DEVIATORIQUE / SPHERIQUE ******
!
!      IF (CONSO)THEN
!        CC1=EXP(-0.5*(E1SF+E1SI)/EKD1)
!        MU1=XMAT00(NMAT+12)*CC1
!        ETA1D=XMAT00(NMAT+13)*CC1
!        CC2=EXP(-0.5*(E2SF+E2SI)/EKD2)
!        CC2=MAX(1.-0.5*(E2SF+E2SI)/EKD2,1.)
!         ESX=-0.5*(E2SF+E2SI)
!         IF(ESX.GT.0.)THEN
!          CC2=MAX(1.,(ESX/EKD2)**(EKD1))
!         ELSE
!          CC2=1.
!         ENDIF
!         ETA2D=XMAT00(NMAT+14)*CC2
!        ETA1D=XMAT00(NMAT+13)*CC2
!      ENDIF
!
!       *** FLUAGE DEVIATORIQUE ***************************
!
!        ON BOUCLE SUR LES 6 DEFORMATIONS DEVIATORIQUES
        do 140 j = 1, 6
!         CHARGEMENT DES VARIBLES INTERNES POUR LA PARTIE DEVIATORIQUE
            e1di(j)=vim(2+(j-1)*2+1)
            e2di(j)=vim(2+(j-1)*2+2)
            e2min=vim(33+(j-1)*2+1)
            e2max=vim(33+(j-1)*2+2)
!
!         VITESSE DE GLISSEMENT IMPOSEE
!         (ATTENTION ON TRAVAILLE AVEC LES GAMMA POUR J>3...)
            if (j .le. 3) then
                ded=2.d0*((epsf(j)-epsi(j))-des/3.d0)
            else
                ded=(epsf(j)-epsi(j))
            endif
            if (dt .eq. 0.d0) then
                if (j .le. 3) then
                    aa=0.d0
                    bb=2.d0*(epsf(j)-yyf/3.d0)
                else
                    aa=0.d0
                    bb=epsf(j)
                endif
            else
                if (j .le. 3) then
                    bb=2.d0*(epsi(j)-yyi/3.d0)
                else
                    bb=epsi(j)
                endif
                aa=ded/dt
            endif
!
            call brdefv(e1di(j), e2di(j), aa, dt, bb,&
                        mu0, mu1, eta1d, mu2, eta2d,&
                        0.d0, e0df(j), e1df(j), e2df(j), e2min,&
                        e2max, se2dv6(j))
!
!        MISE A JOUR DES VARIBLES INTERNES POUR LA PARTIE DEVIATORIQUE
            vip(2+(j-1)*2+1)=e1df(j)
            vip(2+(j-1)*2+2)=e2df(j)
            vip(33+(j-1)*2+1)=e2min
            vip(33+(j-1)*2+2)=e2max
!
140      continue
!
    else
!      PRINT *,'PAS FLUAGE'
!      SI FLUAGE=.FALSE. : CALCUL ELASTIQUE CLASSIQUE (LES COMPOSANTES
!                          VISQUEUSES SONT NULLES)
        e0sf=0.d0
        do 150 i = 1, 3
            e0sf=e0sf+epstf(i)
150      continue
        do 160 i = 1, 6
            if (i .le. 3) then
                e0df(i)=2.d0*(epstf(i)-e0sf/3.d0)
            else
                e0df(i)=epstf(i)
            endif
160      continue
!
    endif
!
!       *** CALCUL DES CONTRAINTES EFFECTIVES NIVEAU 0***************
    call brseff(k0, mu0, e0sf, e0df, sigeff)
!
!        STOCKAGE DES CONTRAINTES EFFECTIVES EN FIN DE PAS
    vip(46)=sigeff(1)
    vip(47)=sigeff(2)
    vip(48)=sigeff(3)
    vip(49)=sigeff(4)
    vip(50)=sigeff(5)
    vip(51)=sigeff(6)
!
!
!     *** ENDOMMAGEMENTS NIVEAU 0 *********************************
!     ** ENDOMMAGEMENTS MACROSCOPIQUES ******************************
!
!        CHARGEMENT DES ENDOMMAGEMENTS DU PAS PRECEDENT
    dt6(1)=vim(15)
    dt6(2)=vim(16)
    dt6(3)=vim(17)
    dt6(4)=vim(18)
    dt6(5)=vim(19)
    dt6(6)=vim(20)
    dc=vim(21)
    sut6(1)=vim(22)
    sut6(2)=vim(23)
    sut6(3)=vim(24)
    sut6(4)=vim(25)
    sut6(5)=vim(26)
    sut6(6)=vim(27)
    suc1=vim(28)
!
!     CHARGEMENT DES VARIABLES INTERNES D ENDOMMAGEMENT MACROSCOPIQUE
    i0=57
    do 170 i = 1, 6
        bt6(i)=vim(i0+i)
170  end do
    bc1=vim(64)
!
!       ON CHARGE LES CONTRAINTES DANS SIGE6 QUI EST MODIFIEE
!        PAR LA PROCEDURE DE TRAITEMENT DE LOCALISATION SI LOCAL=VRAI
    do 180 j = 1, 6
        sige6(j)=sigeff(j)
180  end do
    call brendo(sige6, bt6, sut, bc1, suc,&
                local, t33, n33, lct, bw,&
                pw2, bch, pch, delta, lcc,&
                xmt, xmc, siget6, sigec6, nu0,&
                dt66, dc, sut6, suc1, sigp,&
                dt6)
!
!      CALL ENDO_BETON(SUT,XMT,LCT,BETA,DELTA,SC,SUC,XMC,LCC,
!     #SIGE6,DT6,DC,LOCAL,T33,N33,.TRUE.,SUT6,SUC1,INDIC,
!     # BW,PW2,BCH,PCH)
!
!
!C       ***** CALCUL DES CONTRAINTES APPARENTES **********
!      CALL SIG_APP(SRT,XP,ALPHATC,ALPHACT,SIGE6,DT6,DC,SIGP,
!     S  SUT6,SUC1,XMT,SUT,XMC,SUC,BW,PW2,BCH,PCH,SIGEXT6)
!C      PRINT *,'SIGE6',SIGE6
!C      CALL SIG_APP(SRT,XP,ALPHATC,ALPHACT,SIGE6,DT6,
!C     S DC,SIGP,SUT6,SUC1,XMT,SUT,XMC,SUC,BW,PW2,BCH,
!C     S PCH,SIGEXT6,SUCLT,SUTLT,SUCF,SUTF,XMCLT,XMTLT)
!
!
!       ***** MISE A JOUR DES VARIBLES INTERNES D ENDOMMAGEMENT *****
    i0=57
    do 190 i = 1, 6
        vip(i0+i)=bt6(i)
190  end do
    vip(64)=bc1
!
    vip(15)=dt6(1)
    vip(16)=dt6(2)
    vip(17)=dt6(3)
    vip(18)=dt6(4)
    vip(19)=dt6(5)
    vip(20)=dt6(6)
    vip(21)=dc
    vip(22)=sut6(1)
    vip(23)=sut6(2)
    vip(24)=sut6(3)
    vip(25)=sut6(4)
    vip(26)=sut6(5)
    vip(27)=sut6(6)
    vip(28)=suc1
!
    do 200 i = 4, 6
        sigp(i)=sigp(i)*sqrt(2.d0)
        epstf(i)=epstf(i)/2.d0*sqrt(2.d0)
200  end do
210  continue
!*****************************************************************
end subroutine
