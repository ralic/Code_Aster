      SUBROUTINE HMLIVA(YACHAI,OPTION,MECA,THER,HYDR,
     &                  IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICPVP,VICSAT,ADDEP1,ADCP11,
     +                  ADCP12,ADDETE,ADCOTE,CONGEM,CONGEP,VINTM,VINTP,
     +                  DSDE,EPSV,DEPSV,P1,DP1,T,DT,PHI,PVP,H11,H12,
     +                  RHO11,PHI0,PVP0,SAT,RETCOM,THMC,BIOT,RINSTP)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR GRANET S.GRANET 
C RESPONSABLE GRANET S.GRANET
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_21
C ======================================================================
C ROUTINE HMLIVA : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
C   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
C   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
C   DANS LE CAS OU THMC = 'LIQU_VAPE'
C ======================================================================
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C COMMENTAIRE DE NMCONV :
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C ======================================================================
      IMPLICIT NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,YAMEC,YATE,RETCOM
      INTEGER       ADCOME,ADCP11,ADCP12,ADCOTE,ADDEME,ADDEP1,ADDETE
      INTEGER       ADVIHY,ADVICO,VIHRHO,VICPHI,VICPVP,VICSAT
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON)
      REAL*8        VINTM(NBVARI),VINTP(NBVARI)
      REAL*8        DSDE(DIMCON,DIMDEF)
      REAL*8        EPSV,DEPSV,P1,DP1,T,DT
      REAL*8        PHI,PVP,H11,H12,RHO11
      REAL*8        PHI0,PVP0
      REAL*8        BMPH,UMS,PHIDS,RINSTP
      CHARACTER*16  OPTION,MECA,THER,HYDR,THMC
      LOGICAL       YACHAI
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I
      REAL*8       SATM,EPSVM,PHIM,RHO11M,RHO12M,PVPM,RHO110,DPVP
      REAL*8       DPVPT,DPVPL,BIOT,K0,CS,ALPHA0,ALPLIQ,CLIQ
      REAL*8       CP11,CP12,SAT,DSATP1,MAMOLV,EM
      REAL*8       R,RHO0,CSIGM,ALP11,ALP12,RHO12
      REAL*8       EPS
      PARAMETER  ( EPS = 1.D-21 )
      LOGICAL      EMMAG
C ======================================================================
C --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
C ======================================================================
      REAL*8       RBID1,  RBID2,  RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8       RBID8,  RBID10, RBID14(3)
      REAL*8       RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8       RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8       RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8       RBID33, RBID34, RBID35, RBID38
      REAL*8       RBID39, RBID40, RBID41, RBID42, RBID43
      REAL*8       RBID50, RBID51, RBID52, RBID53, RBID54, RBID55
      REAL*8       RBID45, RBID46, RBID47, RBID48, RBID49, RBID56
      REAL*8       RBID57, RBID58
      REAL*8       R3BID(6)
      REAL*8       M11M,M12M,COEPS,PINF,DP2,CP21,CP22,RHO21
      REAL*8       RHO22,DPAD,SIGNE
      REAL*8       DMVPP1,DMWP1V,DQDEPS,DQVPDP,DQVPDT,DMVPD2,DMWDT2
      REAL*8       DHDT,DHWDP1,DMDEPV,DSIPDT,DSPDLQ,APPMAS,SIGMAP
      REAL*8       CALOR,ENTEAU,ENTGAZ,DILEAU,DILGAZ,MASVOL,R8MAEM
C
      LOGICAL NET,BISHOP
C
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C --- UN PREMIER APPEL A THMRCP POUR RECUPERE SATM --------------------
C =====================================================================
      CALL NETBIS(MECA,NET,BISHOP)
      PVP  = VINTM(ADVICO+VICPVP) + PVP0
      PVPM = VINTM(ADVICO+VICPVP) + PVP0
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, RBID40,
     +             PVPM-P1+DP1,RBID6,RBID7,RBID8,
     +             RBID10, R, RHO0, CSIGM,BIOT, SATM, RBID42,
     +             RBID43, RBID14, RBID15, RBID16,RBID17, RBID18,
     +             RBID19, RBID20, RBID21, RBID22,RBID23, RBID24,
     +             RBID25, RHO110, CLIQ, ALPLIQ, CP11,RBID26,
     +             RBID27, RBID28, RBID29, RBID30, RBID31,RBID32,
     +             RBID33, RBID34, RBID35, MAMOLV, CP12,RBID38,
     +             RBID39, RBID45, RBID46, RBID47, RBID48,RBID49,
     >             EM,RBID57,R3BID,RBID58,RINSTP,RETCOM)
C ======================================================================
C --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
C --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
C --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
C ======================================================================
      EMMAG = .FALSE.
      DPVP   =  0.0D0
      DPVPL  =  0.0D0
      DPVPT  =  0.0D0
      SIGNE  = -1.0D0
      DP2    =  0.0D0
      DPAD   =  0.0D0
      RHO21  =  0.0D0
      RHO22  =  0.0D0
      CP21   =  0.0D0
      CP22   =  0.0D0
      RETCOM =  0
      RHO11  = VINTM(ADVIHY+VIHRHO) + RHO110
      RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
      PHI    = VINTM(ADVICO+VICPHI) + PHI0
      PHIM   = VINTM(ADVICO+VICPHI) + PHI0
      H11    = CONGEM(ADCP11+NDIM+1)
      H12    = CONGEM(ADCP12+NDIM+1)
      M11M   = CONGEM(ADCP11)
      M12M   = CONGEM(ADCP12)
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      IF((EM.GT.EPS).AND.(YAMEC.EQ.0))THEN
        EMMAG = .TRUE.
      ENDIF
      CALL INITHM(IMATE,YACHAI,YAMEC,PHI0,EM,ALPHA0,K0,CS,BIOT,T,
     +                                        EPSV,DEPSV,EPSVM)
C *********************************************************************
C *** LES VARIABLES INTERNES ******************************************
C *********************************************************************
      IF ((OPTION.EQ.'RAPH_MECA') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- EN LIQU_VAPE CALCUL DE RHO11, DES ENTHALPIES DE PVP ET RHOVP ----
C =====================================================================
         CALL VIRHOL(NBVARI,VINTM,VINTP,ADVIHY,VIHRHO,RHO110,
     +            DP1,DP2,DPAD,CLIQ,DT,ALPLIQ,SIGNE,RHO11,RHO11M,RETCOM)
C =====================================================================
C --- EN LIQU_VAPE CALCUL DE RHO11, DES ENTHALPIES DE PVP ET RHOVP ----
C =====================================================================
         PINF = R8MAEM()
         CALL VIPVP1(NBVARI,VINTM,VINTP,ADVICO,VICPVP,DIMCON,PINF,
     +      CONGEM,ADCP11,ADCP12,NDIM,PVP0,DP1,DP2,T,DT,MAMOLV,R,RHO11,
     +                            SIGNE,CP11,CP12,YATE,PVP,PVPM,RETCOM)
C =====================================================================
C --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
C =====================================================================
         IF (RETCOM.NE.0) THEN
            GO TO 30
         ENDIF
      ENDIF
      DPVP   =  PVP - PVPM
C =====================================================================
C --- ON PEUT MAINTENANT CALCULER SAT DANS LE CAS LIQU_VAPE -----------
C =====================================================================
      CALL THMRCP( 'SATURATI', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, RBID50,PVP-P1,
     +             RBID41,RBID6,RBID7, RBID8,
     +             RBID10, RBID51, RBID52, RBID53, RBID54, RBID41,SAT,
     +             DSATP1, RBID14, RBID15, RBID16, RBID17, RBID18,
     +             RBID19, RBID20, RBID21, RBID22, RBID23, RBID24,
     +             RBID25, RHO110, RBID53, RBID52, RBID51, RBID26,
     +             RBID27, RBID28, RBID29, RBID30, RBID31, RBID32,
     +             RBID33, RBID34, RBID35, RBID54, RBID55, RBID38,
     +             RBID39, RBID45, RBID46, RBID47, RBID48, RBID49,
     +             RBID56, RBID57, R3BID,  RBID58, RINSTP,RETCOM)
      IF ((OPTION.EQ.'RAPH_MECA') .OR.
     &    (OPTION.EQ.'FORC_NODA').OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
C =====================================================================
         IF ((YAMEC.EQ.1) )THEN
C =====================================================================
C --- ON POSE ICI P2 = PVP ET P1 = - (PVP - PW) (ON CHANGE LE SIGNE ---
C --- CAR ON MULTIPLIE DANS VIPORO PAR -1) ----------------------------
C =====================================================================
            CALL VIPORO(NBVARI,VINTM,VINTP,ADVICO,VICPHI,PHI0,DEPSV,
     +       ALPHA0,DT,DP1-DPVP,DPVP,SIGNE,SAT,CS,BIOT,PHI,PHIM,RETCOM)
         ENDIF
         IF (EMMAG )THEN
            CALL VIEMMA(NBVARI,VINTM,VINTP,ADVICO,VICPHI,PHI0,
     +       DP1-DPVP,DPVP,SIGNE,SAT,EM,PHI,PHIM,RETCOM)
         ENDIF
C =====================================================================
C --- RECUPERATION DE LA VARIABLE INTERNE DE SATURATION ---------------
C =====================================================================
         CALL VISATU(NBVARI,VINTP,ADVICO,VICSAT,SAT)
C =====================================================================
C --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
C =====================================================================
         IF (RETCOM.NE.0) THEN
            GO TO 30
         ENDIF
      ENDIF
C =====================================================================
C --- QUELQUES INITIALISATIONS ----------------------------------------
C =====================================================================
      BMPH  = BIOT - PHI
      UMS   = 1.D0 - SAT
      PHIDS = PHI*DSATP1

C **********************************************************************
C *** LES CONTRAINTES GENERALISEES *************************************
C **********************************************************************
C ======================================================================
C --- CALCUL DES MASSES VOLUMIQUES DE PRESSION DE VAPEUR ---------------
C ----------------------------------- AIR SEC --------------------------
C ----------------------------------- AIR DISSOUS ----------------------
C ======================================================================
      RHO12  = MASVOL(MAMOLV,PVP ,R,T   )
      RHO12M = MASVOL(MAMOLV,PVPM,R,T-DT)
C =====================================================================
C --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
C =====================================================================
C --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
C =====================================================================
         ALP11 = DILEAU(SAT,BIOT,PHI,ALPHA0,ALPLIQ)
         ALP12 = DILGAZ(SAT,BIOT,PHI,ALPHA0,T     )
C ======================================================================
C --- CALCUL DE LA CAPACITE CALORIFIQUE SELON FORMULE DOCR -------------
C ======================================================================
         CALL CAPACA(RHO0,RHO11,RHO12,RHO21,RHO22,SAT,PHI,
     +              CSIGM,CP11,CP12,CP21,CP22,K0,ALPHA0,T,COEPS,RETCOM)
C =====================================================================
C --- PROBLEME LORS DU CALCUL DE COEPS --------------------------------
C =====================================================================
         IF (RETCOM.NE.0) THEN
            GO TO 30
         ENDIF
C ======================================================================
C --- CALCUL DES ENTHALPIES SELON FORMULE DOCR -------------------------
C ======================================================================
         IF ((OPTION.EQ.'RAPH_MECA') .OR.
     +       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            CONGEP(ADCP11+NDIM+1) = CONGEP(ADCP11+NDIM+1)
     +               + ENTEAU(DT,ALPLIQ,T,RHO11,DP2,DP1,DPAD,SIGNE,CP11)
            CONGEP(ADCP12+NDIM+1) = CONGEP(ADCP12+NDIM+1)
     +                                                 + ENTGAZ(DT,CP12)
            H11 = CONGEP(ADCP11+NDIM+1)
            H12 = CONGEP(ADCP12+NDIM+1)
C ======================================================================
C --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
C =====================================================================
C --- ON POSE ICI P2 = PVP ET P1 = - (PVP - PW) (ON CHANGE LE SIGNE ---
C --- CAR ON MULTIPLIE DANS VIPORO PAR -1) ----------------------------
C ======================================================================
            CONGEP(ADCOTE) = CONGEP(ADCOTE)
     +     + CALOR(ALPHA0,K0,T,DT,DEPSV,DP1-DPVP,DPVP,SIGNE,ALP11,ALP12,
     +                                                            COEPS)
         ENDIF
      ENDIF
C =====================================================================
C --- DPVPL DERIVEE PRESSION DE VAPEUR / PRESSION DE LIQUIDE ----------
C --- DPVPT DERIVEE PRESSION DE VAPEUR / TEMP -------------------------
C =====================================================================
      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
         DPVPL = RHO12M/RHO11M
         IF (YATE.EQ.1) THEN
            DPVPT = RHO12M * (CONGEM(ADCP12+NDIM+1) -
     +                        CONGEM(ADCP11+NDIM+1)) / T
         ENDIF
      ELSE
         DPVPL = RHO12/RHO11
         IF (YATE.EQ.1) THEN
            DPVPT = RHO12 * (CONGEP(ADCP12+NDIM+1) -
     +                       CONGEP(ADCP11+NDIM+1)) / T
         ENDIF
      ENDIF
C ======================================================================
C --- CALCUL SI PAS RIGI_MECA_TANG -------------------------------------
C ======================================================================
      IF ((OPTION.EQ.'RAPH_MECA') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C ======================================================================
C --- CALCUL DES CONTRAINTES DE PRESSIONS ------------------------------
C ======================================================================
         IF (YAMEC.EQ.1) THEN
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)
     +              + SIGMAP(NET,BISHOP,SAT,SIGNE,BIOT,DPVP,DP1-DPVP)
         END IF
C ======================================================================
C --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
C ======================================================================
         CONGEP(ADCP11) = APPMAS(M11M,PHI,PHIM,SAT,SATM,RHO11,
     +                                                RHO11M,EPSV,EPSVM)
         CONGEP(ADCP12) = APPMAS(M12M,PHI,PHIM,1.0D0-SAT,
     +                               1.0D0-SATM,RHO12,RHO12M,EPSV,EPSVM)
      ENDIF

C **********************************************************************
C *** CALCUL DES DERIVEES **********************************************
C **********************************************************************
C ======================================================================
C --- CALCUL DES DERIVEES PARTIELLES DES PRESSIONS SELON FORMULES DOCR -
C --- UNIQUEMENT POUR LES OPTIONS RIGI_MECA ET FULL_MECA ---------------
C ======================================================================
      IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF (YAMEC.EQ.1) THEN
C ======================================================================
C --- CALCUL UNIQUEMENT EN PRESENCE DE MECANIQUE -----------------------
C ======================================================================
C --- CALCUL DES DERIVEES DE SIGMAP ------------------------------------
C ======================================================================
            DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)
     +                                          + DSPDLQ(BIOT,SAT,DPVPL)
C ======================================================================
C --- CALCUL DES DERIVEES DE SIGMAP PAR RAPPORT A LA TEMPERATURE -------
C ======================================================================
            IF (YATE.EQ.1) THEN
               DSDE(ADCOME+6,ADDETE) = DSDE(ADCOME+6,ADDETE)
     +                                          + DSIPDT(BIOT,SAT,DPVPT)
            ENDIF
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
C ======================================================================
            DO 10 I = 1,3
               DSDE(ADCP11,ADDEME+NDIM-1+I) =
     +             DSDE(ADCP11,ADDEME+NDIM-1+I) + DMDEPV(RHO11,SAT,BIOT)
               DSDE(ADCP12,ADDEME+NDIM-1+I) =
     +       DSDE(ADCP12,ADDEME+NDIM-1+I) + DMDEPV(RHO12,1.0D0-SAT,BIOT)
 10         CONTINUE
         ENDIF
         IF (YATE. EQ.1) THEN
C ======================================================================
C --- CALCUL UNIQUEMENT EN PRESENCE DE THERMIQUE -----------------------
C ======================================================================
C --- CALCUL DES DERIVEES DES ENTHALPIES -------------------------------
C ======================================================================
            DSDE(ADCP11+NDIM+1,ADDEP1)=DSDE(ADCP11+NDIM+1,ADDEP1)
     +                                    + DHWDP1(SIGNE,ALPLIQ,T,RHO11)
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)
     +                                                      + DHDT(CP11)
            DSDE(ADCP12+NDIM+1,ADDETE)=DSDE(ADCP12+NDIM+1,ADDETE)
     +                                                      + DHDT(CP12)
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
C ======================================================================
            DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE)
     +                     + DMWDT2(RHO11,ALP11,PHIDS,SAT,BMPH,CS,DPVPT)
            DSDE(ADCP12,ADDETE) = DSDE(ADCP12,ADDETE)
     +             + DMVPD2(RHO12,ALP12,DPVPT,PHI,UMS,PVP,PHIDS,BMPH,CS)
C ======================================================================
C --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
C ======================================================================
            DSDE(ADCOTE,ADDETE)=DSDE(ADCOTE,ADDETE)
     +                                     + DQVPDT(COEPS,ALP12,T,DPVPT)
            DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)
     +                                     + DQVPDP(ALP11,ALP12,T,DPVPL)
C ======================================================================
C --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
C --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
C ======================================================================
            IF (YAMEC.EQ.1) THEN
               DO 20 I = 1,3
                  DSDE(ADCOTE,ADDEME+NDIM-1+I) =
     +            DSDE(ADCOTE,ADDEME+NDIM-1+I) + DQDEPS(ALPHA0,K0,T)
   20          CONTINUE
            ENDIF
         ENDIF
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- POUR LES AUTRES CAS ----------------------------------------------
C ======================================================================
         DSDE(ADCP11,ADDEP1) = DSDE(ADCP11,ADDEP1)
     +                  + DMWP1V(RHO11,PHIDS,SAT,BMPH,CS,DPVPL,PHI,CLIQ)
         DSDE(ADCP12,ADDEP1) = DSDE(ADCP12,ADDEP1)
     +         + DMVPP1(RHO11,RHO12,PHIDS,UMS,BMPH,CS,DPVPL,SAT,PHI,PVP)
      ENDIF
C =====================================================================
 30   CONTINUE
C =====================================================================
      END
