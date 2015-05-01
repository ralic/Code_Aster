subroutine lcumfe(fami, kpg, ksp, ndim, typmod,&
                  imate, tinstm, tinstp, epstm, depst,&
                  sigm, vim, option, sigp, vip,&
                  dsidpt, proj)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterc/iisnan.h"
#include "asterfort/lcmzge.h"
#include "asterfort/lcumef.h"
#include "asterfort/lcummd.h"
#include "asterfort/lcumme.h"
#include "asterfort/lcumsf.h"
#include "asterfort/lcumvi.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/sigela.h"
#include "asterfort/utmess.h"
    integer :: ndim, imate, kpg, ksp
    character(len=8) :: typmod(*)
    character(len=16) :: option(2), option2
    character(len=*) :: fami
    real(kind=8) :: tinstm, tinstp, epstm(12), depst(12)
    real(kind=8) :: sigm(6), sigp(6), vim(25), vip(25)
    real(kind=8) :: dsidpt(6, 6, 2), proj(6, 6), tbid(6),bendo,kdess
!
! COUPLAGE UMLV MAZARS EN NON LOCAL
!---&s---1---------2---------3---------4---------5---------6---------7--
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION MECANIQUE   (--> IFOU SOUS CASTEM)
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  TINSTM  : INSTANT AU CALCUL PRECEDENT
! IN  TINSTP  : INSTANT DU CALCUL
! IN  EPSTM   : EFORMATION A L INSTANT MOINS
! IN  DEPST   : INCREMENT DE DEFORMATION TOTALE
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : (1) OPTION DEMANDEE:RIGI_MECA_TANG, FULL_MECA ,RAPH_MECA
!               (2) MODELE MECA DE COUPLAGE EVENTUEL (MAZARS OU EIB)
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDPT  : MATRICE TANGENTE
!_______________________________________________________________________
!
! ROUTINE CALCULANT :
!
!    - CONTRAINTES FINALES          : SIGP (NSTRS)
!    - VARIABLES INTERNES FINALES   : VIP (NVARI)
!_______________________________________________________________________
!
! STRUCTURE DES PARAMETRES MATERIAU ET AUTRE
! PARAMETRES ELASTIQUES
!     CMAT(1)     = YOUN : MODULE D YOUNG
!     CMAT(2)     = XNU  : COEFFICIENT DE POISSON ELASTIQUE
! PARAMETRES DU FLUAGE PROPRE
!     CMAT(3)     = KRS   : RIGIDITE SPHERIQUE APPARENTE SQUELETTE
!     CMAT(4)     = ETARS : VISCOSITE SPHERIQUE APPARENTE EAU LIBRE
!     CMAT(5)     = KIS   : RIGIDITE SPHERIQUE APPARENTE HYDRATES
!     CMAT(6)     = ETAIS : VISCOSITE SPHERIQUE APPARENTE EAU LIEE
!     CMAT(7)     = KRD   : RIGIDITE DEVIATORIQUE APPARENTE
!     CMAT(8)     = ETARD : VISCOSITE DEVIATORIQUE APPARENTE EAU LIBRE
!     CMAT(9)     = ETAID : VISCOSITE DEVIATORIQUE APPARENTE EAU LIEE
! LES DEUX PARAMETRES SUIVANTS CONCERNENT UNIQUEMENT
! LE FLUAGE DE DESSICCATION
!     CMAT(10)    = RDES : COEFFICIENT UMLV FLUAGE DESSICCATION
!     CMAT(11)    = VDES : COEFFICIENT UMLV/BAZANT FLUAGE DESSICCATION
! OBSOLETE (CONCERNE UNIQUEMENT CASTEM) --> TYPMOD (CODE_ASTER)
!     CMAT(12)    = IFOU : HYPOTHESE DE CALCUL AUX ELEMENTS FINIS
!                     -2 : CONTRAINTES PLANES
!                     -1 : DEFORMATION PLANE
!                      0 : AXISYMETRIQUE
!                      2 : TRIDIMENSIONEL
! PAR PRINCIPE CMAT(13) = 2
!     CMAT(13)    = IFPO : OPTION SUR LE MODELE FLUAGE PROPRE
!                      0 : PAS DE FLUAGE PROPRE
!                      1 : PAS D INFUENCE DE L HUMIDITE REALTIVE
!                      2 : INFLUENCE DE L HUMIDITE RELATIVE
! OBSOLETE --> DEBRANCHE
!     CMAT(14)    = IDES : OPTION SUR LE MODELE FLUAGE DESSICCATION
!                      0 : PAS PRIS EN COMPTE
!                      1 : MODELE BAZANT
!                      2 : MODELE UMLV
!     CMAT(15)    = ICOU : OPTION COUPLAGE MECANIQUE/FLUAGE
!                      0 : PAS DE COUPLAGE (CALCUL CHAINE)
!                      1 : COUPLAGE FORT
!_______________________________________________________________________
!
!  STRUCTURE DES CONTRAINTES : SIGM,SIGP ( X = M ou P )
!    IFOU = -2 : CONTRAINTES PLANES
!      - SIGX(1) = SIGMA_XX
!      - SIGX(2) = SIGMA_YY
!      - SIGX(3) = SIGMA_XY
!      - (SIGX(4) = SIGMA_ZZ = 0)
!
!    IFOU = -1 : DEFORMATION PLANE
!      - SIGX(1) = SIGMA_XX
!      - SIGX(2) = SIGMA_YY
!      - SIGX(3) = SIGMA_ZZ
!      - SIGX(4) = SIGMA_XY
!
!    IFOU = 0  : AXISYMETRIQUE
!      - SIGX(1) = SIGMA_RR
!      - SIGX(2) = SIGMA_ZZ
!      - SIGX(3) = SIGMA_TT
!      - SIGX(4) = SIGMA_RZ
!
!    IFOU = 2  : TRIDIMENSIONEL
!      - SIGX(1) = SIGMA_XX
!      - SIGX(2) = SIGMA_YY
!      - SIGX(3) = SIGMA_ZZ
!      - SIGX(4) = SIGMA_XY
!      - SIGX(5) = SIGMA_ZX
!      - SIGX(6) = SIGMA_YZ
!_______________________________________________________________________
!
!  STRUCTURE DE L'INCREMENT DEFORMATION TOTALE : DEPS (NSTRS)
!
!    IFOU = -2 : CONTRAINTES PLANES
!      - DEPS(1) = DEPSILON_XX
!      - DEPS(2) = DEPSILON_YY
!      - DEPS(3) = DEPSILON_XY
!      - DEPS(4) = DEPSILON_ZZ
!
!    IFOU = -1 : DEFORMATION PLANE
!      - DEPS(1) = DEPSILON_XX
!      - DEPS(2) = DEPSILON_YY
!      - DEPS(3) = DEPSILON_XY
!      - (DEPS(4) = DEPSILON_ZZ = 0)
!
!    IFOU = 0  : AXISYMETRIQUE
!      - DEPS(1) = DEPSILON_RR
!      - DEPS(2) = DEPSILON_ZZ
!      - DEPS(3) = DEPSILON_TT
!      - DEPS(4) = DEPSILON_RZ
!
!    IFOU = 2  : TRIDIMENSIONEL
!      - DEPS(1) = SIGMA_XX
!      - DEPS(2) = SIGMA_YY
!      - DEPS(3) = SIGMA_ZZ
!      - DEPS(4) = SIGMA_XY
!      - DEPS(5) = SIGMA_ZX
!      - DEPS(6) = SIGMA_YZ
!_______________________________________________________________________
!
!  STRUCTURE DES VARIABLES INTERNES : VIM,VIP ( X = I ou F )
!
!     VIX(1)     = ERSP  : DEFORMATION DE FLUAGE REV SPHERIQUE
!     VIX(2)     = EISP  : DEFORMATION DE FLUAGE IRR SPHERIQUE
!     VIX(3)     = ERD11 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 11
!     VIX(4)     = EID11 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 11
!     VIX(5)     = ERD22 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 22
!     VIX(6)     = EID22 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 22
!     VIX(7)     = ERD33 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 33
!     VIX(8)     = EID33 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 33
!     VIX(9)     = EFD11 : DEFORMATION DE FLUAGE DE DESSICCATION  11
!     VIX(10)    = EFD22 : DEFORMATION DE FLUAGE DE DESSICCATION  22
!     VIX(11)    = EFD33 : DEFORMATION DE FLUAGE DE DESSICCATION  33
!     VIX(12)    = ERD12 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 12
!     VIX(13)    = EID12 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 12
!     VIX(14)    = ERD23 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 23
!     VIX(15)    = EID23 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 23
!     VIX(16)    = ERD31 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 31
!     VIX(17)    = EID31 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 31
!     VIX(18)    = EFD12 : DEFORMATION DE FLUAGE DE DESSICCATION  12
!     VIX(19)    = EFD23 : DEFORMATION DE FLUAGE DE DESSICCATION  23
!     VIX(20)    = EFD31 : DEFORMATION DE FLUAGE DE DESSICCATION  31
!     VIX(21)    = INDICATEUR DU FLUAGE SPHERIQUE (0 ou 1)
!     VARIABLES INTERNES UTILISEES SI COUPLAGE AVEC ENDO_ISOT_BETON
!     VIX(22)    = ENDOMMAGEMENT D DONNE PAR EIB
!     VIX(23)    = INDICATEUR D'ENDOMMAGEMENT DE EIB
!     VARIABLES INTERNES UTILISEES SI COUPLAGE AVEC MAZARS
!     VIX(22)    = ENDOMMAGEMENT D DONNE PAR MAZARS
!     VIX(23)    = INDICATEUR D'ENDOMMAGEMENT DE EIB
!     VIX(24)    = TEMPERATURE MAXIMALE ATTEINTE PAR LE MATERIAU
!     VIX(25)    = VALEUR DE EPSEQ (UTILE POUR POSTTRAITER)
!_______________________________________________________________________
!
    integer :: iret
    character(len=16) :: nomres(16)
    integer :: icodre(16)
    real(kind=8) :: cfps, cfpd
    integer :: i, j, k, nstrs, ifou, isph
    real(kind=8) :: tdt
    real(kind=8) :: youn, xnu
    real(kind=8) :: krs, etars, kis, etais, krd, etard, etaid
    real(kind=8) :: etafd
    real(kind=8) :: cmat(15), dep(6, 6), depm(6, 6)
    real(kind=8) :: an(6), bn(6, 6), cn(6, 6), valres(16)
    real(kind=8) :: hygrm, hygrp, rbid
    real(kind=8) :: epsrm, epsrp, epsfm(6)
    real(kind=8) :: kron(6)
    real(kind=8) :: hydrm, hydrp, sechm, sechp, sref, tm, tp, tref
    real(kind=8) :: epsthp, epsthm
    real(kind=8) :: tmaxp, tmaxm, younm, xnum, epsm(6), deps(6)
    real(kind=8) :: sigelm(6), sigelp(6), epsel(6)
    data     kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/

    rbid = 0.d0
!
!
!   CALCUL DE L'INTERVALLE DE TEMPS
!
    tdt = tinstp-tinstm
!
!   DIMENSION
!
    nstrs = 2*ndim
!
    do 13 i = 1, nstrs
        epsm(i)=epstm(i)
        deps(i)=depst(i)
13  end do
!
!   TYPE DE CALCUL
!
    if (typmod(1) .eq. 'C_PLAN') then
        ifou = -2
        goto 1
    else if (typmod(1) .eq. 'D_PLAN') then
        ifou = -1
        goto 1
    else if (typmod(1) .eq. 'AXIS') then
        ifou = 0
        goto 1
    else
        ifou = 2
    endif
 1  continue
!
!
!   INITIALISATION DU FLUAGE SPHERIQUE PROPRE
!
    isph = 1
!
!
! RECUPERATION DES VALEURS DE TEMPERATURE
!
    call rcvarc('F', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret)
    call rcvarc('F', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret)
    call rcvarc('F', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret)
!
!
!  ------- LECTURE DES CARACTERISTIQUES ELASTIQUES
!  LA DEPENDENCE DES PARAMETRES PAR RAPPORT A LA TEMPERATURE
!  CHANGE PAR RAPPORT A LA LOI D ENDOMMAGEMENT
!
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3) = 'ALPHA'
    nomres(4) = 'ALPHA'
!
!    IF (OPTION(2).EQ.'MAZARS') THEN
    tmaxm = vim(24)
    tmaxp = max(tmaxm, tp)
!
    call rcvalb(fami, 1, 1, '-', imate,&
                ' ', 'ELAS', 1, 'TEMP', [tmaxm],&
                2, nomres, valres, icodre, 1)
    younm = valres(1)
    xnum = valres(2)
!
    call rcvalb(fami, 1, 1, '+', imate,&
                ' ', 'ELAS', 1, 'TEMP', [tmaxp],&
                2, nomres, valres, icodre, 1)
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 1, 'TEMP', [tmaxm],&
                1, nomres(3), valres(3), icodre(3), 0)
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 1, 'TEMP', [tmaxp],&
                1, nomres(4), valres(4), icodre(4), 0)
!
!      ELSE
!     IF (OPTION(2).EQ.'ENDO_ISOT_BETON') THEN
!
!       ENDIF
!
    youn = valres(1)
    xnu = valres(2)
!
!
!  -------CALCUL DES DEFORMATIONS THERMIQUES
!
!      IF ((OPTION(2).EQ.'MAZARS') .OR.
!     &    (OPTION(2).EQ.'ENDO_ISOT_BETON')) THEN
    if ((iisnan(tref).eq.1) .or. (icodre(3).ne.0) .or. (icodre(4).ne.0)) then
        call utmess('F', 'CALCULEL_15')
    else
        if (iisnan(tm) .eq. 0) then
            epsthm = valres(3) * (tm - tref)
        else
            epsthm = 0.d0
        endif
        if (iisnan(tp) .eq. 0) then
            epsthp = valres(4) * (tp - tref)
        else
            epsthp = 0.d0
        endif
    endif
!      ELSE
!        CALL VERIFT(FAMI,KPG,KSP,'+',IMATE,'ELAS',1,EPSTHP,IRET1)
!        CALL VERIFT(FAMI,KPG,KSP,'-',IMATE,'ELAS',1,EPSTHM,IRET2)
!      ENDIF
!
!
!
! MODIFI DU 18 AOUT 2004 - AJOUT RETRAIT
!
!  ------- CARACTERISTIQUES DE RETRAIT ENDOGENE ET DE DESSICCATION
!
    nomres(1)='B_ENDOGE'
    nomres(2)='K_DESSIC'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                2, nomres, valres, icodre, 1)
    bendo=valres(1)
    kdess=valres(2)
!
!
!
!  ------- CARACTERISTIQUES FLUAGE PROPRE UMLV
!
    nomres(1)='K_RS'
    nomres(2)='ETA_RS'
    nomres(3)='K_IS'
    nomres(4)='ETA_IS'
    nomres(5)='K_RD'
    nomres(6)='ETA_RD'
    nomres(7)='ETA_ID'
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'BETON_UMLV_FP', 0, ' ', [rbid],&
                7, nomres, valres, icodre, 2)
    krs = valres(1)
    etars = valres(2)
    kis = valres(3)
    etais = valres(4)
    krd = valres(5)
    etard = valres(6)
    etaid = valres(7)
!
!
! ------- CARACTERISTIQUE FLUAGE DE DESSICATION DE BAZANT
!
    nomres(8)='ETA_FD'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'BETON_UMLV_FP', 0, ' ', [rbid],&
                8, nomres, valres, icodre, 0)
!     FLUAGE DE DESSICCATION NON ACTIVE
    if (icodre(8) .ne. 0) then
        cmat(14) = 0
        etafd = -1.0d0
!     FLUAGE DE DESSICCATION ACTIVE
    else
        cmat(14) = 1
        etafd = valres(8)
    endif
!
!
!
!  ------- CARACTERISTIQUES HYGROMETRIE H
!
    nomres(1)='FONC_DESORP'
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [rbid],&
                1, nomres(1), valres(1), icodre(1), 2)
    if (icodre(1) .ne. 0) then
        call utmess('F', 'ALGORITH4_94')
    endif
    hygrm=valres(1)
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [rbid],&
                1, nomres(1), valres(1), icodre(1), 2)
    if (icodre(1) .ne. 0) then
        call utmess('F', 'ALGORITH4_94')
    endif
    hygrp=valres(1)
!
!
!
! CONSTRUCTION DU VECTEUR CMAT CONTENANT LES CARACTERISTIQUES MECANIQUES
!
!     CMAT(1)     = YOUN   : MODULE D YOUNG
!     CMAT(2)     = XNU    : COEFFICIENT DE POISSON ELASTIQUE
!     CMAT(3)     = KRS   : RIGIDITE SPHERIQUE APPARENTE SQUELETTE
!     CMAT(4)     = ETARS : VISCOSITE SPHERIQUE APPARENTE EAU LIBRE
!     CMAT(5)     = KIS   : RIGIDITE SPHERIQUE APPARENTE HYDRATES
!     CMAT(6)     = ETAIS : VISCOSITE SPHERIQUE APPARENTE EAU LIEE
!     CMAT(7)     = KRD   : RIGIDITE DEVIATORIQUE APPARENTE
!     CMAT(8)     = ETARD : VISCOSITE DEVIATORIQUE APPARENTE EAU LIBRE
!     CMAT(9)     = ETAID : VISCOSITE DEVIATORIQUE APPARENTE EAU LIEE
!
    cmat(1) = youn
    cmat(2) = xnu
    cmat(3) = krs
    cmat(4) = etars
    cmat(5) = kis
    cmat(6) = etais
    cmat(7) = krd
    cmat(8) = etard
    cmat(9) = etaid
! MODIFI FD BAZANT
    cmat(11) = etafd
    cmat(12) = ifou
    cmat(13) = 2
! MODIFI 25/08/04 YLP - ACTIVATION DU FLUAGE DE DESSICATION DE BAZANT
! CMAT(14)=IDES 0 --> 1
!      CMAT(14)    = 1
    cmat(15) = 1
!
!   DANS LE CAS OU LE TEST DE DEFORMATION DE FLUAGE PROPRE
!        IRREVE A ECHOUE : ISPH = 0
!
10  continue
!
! INITIALISATION DES VARIABLES
!
    cfps = 0.d0
    cfpd = 0.d0
! MODIFI DU 6 JANVIER 2003 - YLP NSTRS -->  6
!      DO 11 I=1,NSTRS
    do 11 i = 1, 6
        an(i) = 0.d0
!        DO 12 J=1,NSTRS
        do 12 j = 1, 6
            dep(i,j) = 0.d0
            bn(i,j) = 0.d0
            cn(i,j) = 0.d0
12      continue
11  end do
!
!
!_______________________________________________________________________
!
! CALCUL DES MATRICES DES DEFORMATIONS DE FLUAGE TOTAL
!   DFLUT(N+1) = AN + BN * SIGMA(N) + CN * SIGMA(N+1)
!_______________________________________________________________________
    if (tdt .ne. 0.d0) then
        if (option(1)(1:9) .eq. 'RIGI_MECA') then
            isph=nint(vim(21))
        endif
        call lcummd(vim, 20, cmat, 15, sigm,&
                    nstrs, isph, tdt, hygrm, hygrp,&
                    an, bn, cn, cfps, cfpd)
    endif
!
!
!_______________________________________________________________________
!
! RECUPERATION DE L HYDRATATION E DU SECHAGE
! CALCUL DE LA SIGMA ELASTIQUE AU TEMP M POUR COUPLAGE AVEC MAZARS
!  MODIFIE 20 SEPT 2008 M.BOTTONI
!_______________________________________________________________________
!
!
    call lcumvi('FT', vim, epsfm)
!
!
    if ((option(1)(1:9).eq.'FULL_MECA') .or. (option(1)(1:9).eq.'RAPH_MECA')) then
!
! MODIFI DU 18 AOUT 2004 YLP - CORRECTION DE LA DEFORMATION DE FLUAGE
! PAR LES DEFORMATIONS DE RETRAIT
!
        call rcvarc(' ', 'HYDR', '+', fami, kpg,&
                    ksp, hydrp, iret)
        if (iret .ne. 0) hydrp=0.d0
        call rcvarc(' ', 'HYDR', '-', fami, kpg,&
                    ksp, hydrm, iret)
        if (iret .ne. 0) hydrm=0.d0
        call rcvarc(' ', 'SECH', '+', fami, kpg,&
                    ksp, sechp, iret)
        if (iret .ne. 0) sechp=0.d0
        call rcvarc(' ', 'SECH', '-', fami, kpg,&
                    ksp, sechm, iret)
        if (iret .ne. 0) sechm=0.d0
        call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                    ksp, sref, iret)
        if (iret .ne. 0) sref=0.d0
!
        epsrm = kdess*(sechm-sref)-bendo*hydrm + epsthm
        epsrp = kdess*(sechp-sref)-bendo*hydrp + epsthp
!
!    CALCUL DE LA DEFORMATION ELASTIQUE AU TEMP M
!    (LA SEULE QUI CONTRIBUE A FAIRE EVOLUER L'ENDOMMAGEMENT)
!    POUR LE COUPLAGE AVEC MAZARS
!
!      IF (OPTION(2).EQ.'MAZARS') THEN
        call r8inir(6, 0.d0, epsel, 1)
        do 35 k = 1, nstrs
            epsel(k) = epsm(k) - epsrm * kron(k) - epsfm(k)
35      continue
!
!
!  -  ON CALCUL LES CONTRAINTES ELASTIQUES AU TEMP M
!
        call sigela(typmod, ndim, younm, xnum, epsel,&
                    sigelm)
!        ENDIF
!
!
! ________________________________________________________________
!
!  1. CONSTRUCTION DE LA MATRICE D ELASTICITE DE HOOKE POUR MAZARS
!     OU UMLV SANS COUPLAGE, OU DE LA MATRICE ELASTO-ENDOMMAGEE POUR EIB
!  2. MISE A JOUR DE L ENDOMMAGEMENT ET DES SIGMA POUR EIB
! ________________________________________________________________
!
!
!        IF (OPTION(2).EQ.'ENDO_ISOT_BETON') THEN
!       ELSE
!    MATRICE D ELASTICITE DE HOOKE POUR MAZARS ET UMLV SANS COUPLAGE
        call lcumme(youn, xnu, ifou, dep)
        call lcumme(younm, xnum, ifou, depm)
!       ENDIF
!
!
! ________________________________________________________________
!
!  1. MISE A JOUR DES SIGMA POUR EIB ET UMLV SANS COUPLAGE
!     CALCUL DES SIGMA ELASTIQUES POUR MAZARS
!      (LCUMEF)
!  2. MISE A JOUR DES VARIABLES INTERNES FINALES DE FLUAGE
!      (LCUMSF)
! ________________________________________________________________
!
!  PRISE EN COMPTE DU FLUAGE PROPRE ET DE DESSICCATION
!   MODIFI DU 18 AOUT 2004 YLP - CORRECTION DE LA DEFORMATION DE FLUAGE
!   PAR LES DEFORMATIONS DE RETRAIT
!
!        IF (OPTION(2).EQ.'MAZARS') THEN
        call lcumef(option, dep, depm, an, bn,&
                    cn, epsm, epsrm, epsrp, deps,&
                    epsfm, sigelm, nstrs, sigelp)
        call lcumsf(sigelm, sigelp, nstrs, vim, 20,&
                    cmat, 15, isph, tdt, hygrm,&
                    hygrp, vip)
!
!        ELSE
!        ENDIF
!
        vip(21)=1
!
!  TEST DE LA CROISSANCE SUR LA DEFORMATION DE FLUAGE PROPRE SPHERIQUE
!
        if (isph .eq. 2) then
            isph = 0
            goto 10
        endif
!
!
!___________________________________________________________
!
!  MISE A JOUR DE L ENDOMMAGEMENT ET DES SIGMA POUR MAZARS
!_________________________________________________________
!
!
!        IF (OPTION(2).EQ.'MAZARS') THEN
!
        call lcmzge(fami, kpg, ksp, ndim, typmod,&
                    imate, epstm, depst, vim (22), 'RAPH_COUP       ',&
                    sigp, vip, dsidpt, proj)
!        ENDIF
!
! FIN DE (IF RAPH_MECA ET FULL_MECA)
    endif
!
!_______________________________________________________________________
!
! CONSTRUCTION DE LA MATRICE TANGENTE
!_______________________________________________________________________
!
!      IF (OPTION(2).EQ.'MAZARS') THEN
!  MB: LA MATRICE TANGENTE CALCULEE EST CELLE DU COMPORTEMENT DE MAZARS
!      I.E. LA CONTRIBUTION DU FLUAGE N EST PAS CONSIDEREE
!
    if ((option(1)(1:9).eq.'FULL_MECA') .or. (option(1)(1:9).eq.'RIGI_MECA')) then
!
        if (option(1)(1:9) .eq. 'FULL_MECA') option(1) = 'RIGI_COUP       '
        option2=option(1)
        call lcmzge(fami, kpg, ksp, ndim, typmod,&
                    imate, epstm, depst, vim(22), option2,&
                    tbid, vip, dsidpt, proj)
    endif
!      ENDIF
end subroutine
