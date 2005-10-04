      SUBROUTINE HMLVGA(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICPVP,VICSAT,ADDEP1,ADCP11,
     +                  ADCP12,ADDEP2,ADCP21,ADCP22,ADDETE,ADCOTE,
     +                  CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,DEPSV,P1,P2,
     +                  DP1,DP2,T,DT,PHI,PADP,PVP,H11,H12,H21,H22,KH,
     +                  RHO11,PHI0,PVP0,SAT,RETCOM,THMC)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 03/10/2005   AUTEUR GRANET S.GRANET 
C RESPONSABLE GRANET S.GRANET
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C **********************************************************************
C ROUTINE HMLVAG : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISE
C   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
C   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
C   DANS LE CAS OU THMC = 'LIQU_AD_GAZ_VAPE'
C **********************************************************************
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C COMMENTAIRE DE NMCONV :
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C  VARIABLES IN / OUT
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,YAMEC
      INTEGER       YATE,RETCOM,ADCOME,ADCP11,ADCP12,ADVIHY,ADVICO
      INTEGER       VIHRHO,VICPHI,VICPVP,VICSAT
      INTEGER       ADCP21,ADCP22,ADCOTE,ADDEME,ADDEP1,ADDEP2,ADDETE
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON),VINTM(NBVARI)
      REAL*8        VINTP(NBVARI),DSDE(DIMCON,DIMDEF),EPSV,DEPSV
      REAL*8        P1,DP1,P2,DP2,T,DT,PHI,PADP,PVP,H11,H12,H21,H22
      REAL*8        RHO11,PHI0,PVP0,KH 
      CHARACTER*16  OPTION,MECA,THER,HYDR,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I
      REAL*8       SATM,EPSVM,PHIM,RHO11M,RHO12M,RHO21M,PVPM,RHO22M
      REAL*8       BIOT,K0,CS,ALPHA0,ALPLIQ,CLIQ,RHO110
      REAL*8       CP11,CP12,CP21,SAT,DSATP1,MAMOLV,MAMOLG
      REAL*8       R,RHO0,COEPS,CSIGM,VARIA,ALP11,ALP12,ALP21
      REAL*8       DP11T, DP11P1, DP11P2, DP12T, DP12P1, DP12P2
      REAL*8       DP21T, DP21P1, DP21P2, DP22T, DP22P1, DP22P2
      REAL*8       UMPRHS, BIDON,RHO12,RHO21,CP22
      REAL*8       PADM,RHO22,VARPAD,VARBIO,VARLQ,EM
C ======================================================================
C --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
C ======================================================================
      REAL*8       RBID1,  RBID2,  RBID3,  RBID4,  RBID5,  RBID6, RBID7
      REAL*8       RBID8,  RBID9,  RBID10, RBID11, RBID12, RBID13,RBID14
      REAL*8       RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8       RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8       RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8       RBID33, RBID34, RBID35, RBID36, RBID37, RBID38
      REAL*8       RBID39, RBID40, RBID45, RBID46, RBID49, RBID50,RBID51
      REAL*8       SIGNE, PVP1, PVP1M, DPAD, QPRIME, PAS
      REAL*8       M11M,M12M,M21M,M22M,DMASP1,DMASP2,DMVDP1,DMVDP2
      REAL*8       DMADP1,DMADP2,DMWDP1,DMWDP2,DQDEPS,DQDP,DQDT,DMASDT
      REAL*8       DMVPDT,DMADT,DMWDT,DMDEPV,DSPDP1,DSPDP2,APPMAS,SIGMAP
      REAL*8       MASVOL,DILEAU,DILGAZ,ENTEAU,ENTGAZ,CALOR,DHW2P1
      REAL*8       DHW2P2,DHW2DT,DHDT,PINF,R8MAEM,MAJPAS
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +             P1-DP1,
     +             RBID6,
     +             RBID7, RBID8, RBID10, R, RHO0, CSIGM,
     +             BIOT, SATM, SAT, DSATP1, RBID14, RBID15,
     +             RBID16,
     +             RBID17, RBID18, RBID19, RBID20, RBID21, RBID22,
     +             RBID23, RBID24, RBID25, RHO110, CLIQ, ALPLIQ, CP11,
     +             RBID26, RBID27, RBID28, RBID29, MAMOLG, CP21,
     +             RBID32, RBID33, RBID34, RBID35, MAMOLV, CP12,RBID38,
     +             RBID39,RBID45,RBID46,CP22,KH,RBID49,EM,RBID50,RBID51)
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      ALP11  = 0.0D0
      ALP12  = 0.0D0
      ALP21  = 0.0D0
      SIGNE  = 1.0D0
      M11M   = CONGEM(ADCP11)
      M12M   = CONGEM(ADCP12)
      M21M   = CONGEM(ADCP21)
      M22M   = CONGEM(ADCP22)
      RHO11  = VINTM(ADVIHY+VIHRHO) + RHO110
      RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
      PVP    = VINTM(ADVICO+VICPVP) + PVP0
      PVPM   = VINTM(ADVICO+VICPVP) + PVP0
      PHI    = VINTM(ADVICO+VICPHI) + PHI0
      PHIM   = VINTM(ADVICO+VICPHI) + PHI0
      RETCOM = 0
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      CALL INITHM(IMATE,YAMEC,PHI0,EM,ALPHA0,K0,CS,BIOT,T,
     +                                  EPSV,DEPSV,EPSVM,MECA)
C *********************************************************************
C *** LES VARIABLES INTERNES ******************************************
C *********************************************************************
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
C =====================================================================
         IF (YAMEC.EQ.1) THEN
            CALL VIPORO(NBVARI,VINTM,VINTP,ADVICO,VICPHI,PHI0,
     +       DEPSV,ALPHA0,DT,DP1,DP2,SIGNE,SAT,CS,BIOT,PHI,PHIM,RETCOM)
         ENDIF
C =====================================================================
C --- CALCUL DE LA PRESSION DE VAPEUR TILDE SELON FORMULE DOCR --------
C --- ETAPE INTERMEDIAIRE AU CALCUL DE LA VARIABLE INTERNE ------------
C --- NB : CE CALCUL SE FAIT AVEC LA MASSE VOLUMIQUE DU FLUIDE --------
C ---    : A L INSTANT MOINS ------------------------------------------
C =====================================================================
         PINF = R8MAEM()
         CALL VIPVP1(NBVARI,VINTM,VINTP,ADVICO,VICPVP,DIMCON,PINF,
     +     CONGEM,ADCP11,ADCP12,NDIM,PVP0,DP1,DP2,T,DT,MAMOLV,R,RHO11M,
     +                          SIGNE,CP11,CP12,YATE,PVP1,PVP1M,RETCOM)
      IF (RETCOM.NE.0) THEN
         GO TO 30
      ENDIF
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE PRESSION DE VAPEUR -------------
C --- SELON FORMULE DOCR ----------------------------------------------
C =====================================================================
         CALL VIPVP2(NBVARI,VINTM,VINTP,ADVICO,VICPVP,PVP0,
     +        PVP1,P2,DP2,T,DT,KH,MAMOLV,R,RHO11M,YATE,PVP,PVPM,RETCOM)
C =====================================================================
C --- MISE A JOUR DE LA PRESSION D AIR DISSOUS SELON FORMULE DOCR -----
C =====================================================================
         CALL MAJPAD(P2,PVP,R,T,KH,DP2,PVPM,DT,PADP,PADM,DPAD,RETCOM)
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE MASSE VOLUMIQUE DU FLUIDE ------
C --- SELON FORMULE DOCR ----------------------------------------------
C =====================================================================
         CALL VIRHOL(NBVARI,VINTM,VINTP,ADVIHY,VIHRHO,RHO110,
     +           DP1,DP2,DPAD,CLIQ,DT,ALPLIQ,SIGNE,RHO11,RHO11M,RETCOM)
C =====================================================================
C --- RECUPERATION DE LA VARIABLE INTERNE DE SATURATION ---------------
C =====================================================================
         CALL VISATU(NBVARI,VINTP,ADVICO,VICSAT,SAT)
      ELSE
C =====================================================================
C --- MISE A JOUR DE LA PRESSION D AIR DISSOUS SELON FORMULE DOCR -----
C =====================================================================
         CALL MAJPAD(P2,PVP,R,T,KH,DP2,PVPM,DT,PADP,PADM,DPAD,RETCOM)
      ENDIF
C =====================================================================
C --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
C =====================================================================
      IF (RETCOM.NE.0) THEN
         GO TO 30
      ENDIF

C **********************************************************************
C *** LES CONTRAINTES GENERALISEES *************************************
C **********************************************************************
C ======================================================================
C --- CALCUL DES MASSES VOLUMIQUES DE PRESSION DE VAPEUR ---------------
C ----------------------------------- AIR SEC --------------------------
C ----------------------------------- AIR DISSOUS ----------------------
C ======================================================================
      RHO12  = MASVOL(MAMOLV,PVP        ,R,T   )
      RHO12M = MASVOL(MAMOLV,PVPM       ,R,T-DT)
      RHO21  = MASVOL(MAMOLG,P2-PVP     ,R,T   )
      RHO21M = MASVOL(MAMOLG,P2-DP2-PVPM,R,T-DT)
      RHO22  = MASVOL(MAMOLG,PADP       ,R,T   )
      RHO22M = MASVOL(MAMOLG,PADM       ,R,T-DT)
      PAS    = MAJPAS(P2,PVP)
C =====================================================================
C --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
C =====================================================================
C --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
C =====================================================================
         ALP11 = DILEAU(SAT,BIOT,PHI,ALPHA0,ALPLIQ)
         ALP12 = DILGAZ(SAT,BIOT,PHI,ALPHA0,T     )
         ALP21 = DILGAZ(SAT,BIOT,PHI,ALPHA0,T     )
         H11   = CONGEM(ADCP11+NDIM+1)
         H12   = CONGEM(ADCP12+NDIM+1)
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
         IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     +       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            CONGEP(ADCP11+NDIM+1) = CONGEP(ADCP11+NDIM+1)
     +              + ENTEAU(DT,ALPLIQ,T,RHO11,DP2,DP1,DPAD,SIGNE,CP11)
            CONGEP(ADCP12+NDIM+1) = CONGEP(ADCP12+NDIM+1)
     +                                                + ENTGAZ(DT,CP12)
            CONGEP(ADCP21+NDIM+1) = CONGEP(ADCP21+NDIM+1)
     +                                                + ENTGAZ(DT,CP21)
            CONGEP(ADCP22+NDIM+1) = CONGEP(ADCP22+NDIM+1)
     +                                                + ENTGAZ(DT,CP22)
            H11 = CONGEP(ADCP11+NDIM+1)
            H12 = CONGEP(ADCP12+NDIM+1)
C ======================================================================
C --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
C ======================================================================
            CONGEP(ADCOTE) = CONGEP(ADCOTE)
     +     + CALOR(ALPHA0,K0,T,DT,DEPSV,DP1,DP2,SIGNE,ALP11,ALP12,COEPS)
         ENDIF
      ENDIF
C ======================================================================
C --- CALCUL SI PAS RIGI_MECA_TANG -------------------------------------
C ======================================================================
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C ======================================================================
C --- CALCUL DES CONTRAINTES DE PRESSIONS ------------------------------
C ======================================================================
         IF (YAMEC.EQ.1) THEN
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)
     +                                  + SIGMAP(SAT,SIGNE,BIOT,DP2,DP1)
         END IF
C ======================================================================
C --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
C ======================================================================
         CONGEP(ADCP11) = APPMAS(M11M,PHI,PHIM,SAT,SATM,RHO11,
     +                                                RHO11M,EPSV,EPSVM)
         CONGEP(ADCP12) = APPMAS(M12M,PHI,PHIM,1.0D0-SAT,
     +                               1.0D0-SATM,RHO12,RHO12M,EPSV,EPSVM)
         CONGEP(ADCP21) = APPMAS(M21M,PHI,PHIM,1.0D0-SAT,
     +                               1.0D0-SATM,RHO21,RHO21M,EPSV,EPSVM)
         CONGEP(ADCP22) = APPMAS(M22M,PHI,PHIM,SAT,SATM,RHO22,
     +                                                RHO22M,EPSV,EPSVM)
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
         CALL DPLVGA(YATE,RHO11,RHO12,R,T,KH,CONGEM,DIMCON,ADCP11,
     +                  ADCP12,NDIM,PADP,DP11P1,DP11P2,DP12P1,DP12P2,
     +                  DP21P1,DP21P2,DP11T,DP12T,DP21T)
         IF (YAMEC.EQ.1) THEN
C ======================================================================
C --- CALCUL UNIQUEMENT EN PRESENCE DE MECANIQUE -----------------------
C ======================================================================
C --- CALCUL DES DERIVEES DE SIGMAP ------------------------------------
C ======================================================================
            DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)
     +                                          + DSPDP1(SIGNE,BIOT,SAT)
            DSDE(ADCOME+6,ADDEP2)=DSDE(ADCOME+6,ADDEP2)+DSPDP2(BIOT)
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
C ======================================================================
            DO 10 I = 1,3
               DSDE(ADCP11,ADDEME+NDIM-1+I) =
     +             DSDE(ADCP11,ADDEME+NDIM-1+I) + DMDEPV(RHO11,SAT,BIOT)
               DSDE(ADCP12,ADDEME+NDIM-1+I) =
     +       DSDE(ADCP12,ADDEME+NDIM-1+I) + DMDEPV(RHO12,1.0D0-SAT,BIOT)
               DSDE(ADCP21,ADDEME+NDIM-1+I) =
     +       DSDE(ADCP21,ADDEME+NDIM-1+I) + DMDEPV(RHO21,1.0D0-SAT,BIOT)
               DSDE(ADCP22,ADDEME+NDIM-1+I) = 
     +             DSDE(ADCP22,ADDEME+NDIM-1+I) + DMDEPV(RHO22,SAT,BIOT)
 10         CONTINUE
         ENDIF
         IF (YATE. EQ.1) THEN
C ======================================================================
C --- CALCUL UNIQUEMENT EN PRESENCE DE THERMIQUE -----------------------
C ======================================================================
C --- CALCUL DES DERIVEES DES ENTHALPIES -------------------------------
C ======================================================================
            DSDE(ADCP11+NDIM+1,ADDEP2) = DSDE(ADCP11+NDIM+1,ADDEP2)
     +                                   + DHW2P2(DP11P2,ALPLIQ,T,RHO11)
            DSDE(ADCP11+NDIM+1,ADDEP1)=DSDE(ADCP11+NDIM+1,ADDEP1)
     +                             + DHW2P1(SIGNE,DP11P1,ALPLIQ,T,RHO11)
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)
     +                               + DHW2DT(DP11T,ALPLIQ,T,RHO11,CP11)
            DSDE(ADCP12+NDIM+1,ADDETE)=DSDE(ADCP12+NDIM+1,ADDETE)
     +                                                      + DHDT(CP12)
            DSDE(ADCP21+NDIM+1,ADDETE)=DSDE(ADCP21+NDIM+1,ADDETE)
     +                                                      + DHDT(CP21)
            DSDE(ADCP22+NDIM+1,ADDETE)=DSDE(ADCP22+NDIM+1,ADDETE)
     +                                                      + DHDT(CP22)
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
C ======================================================================
            DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE)
     +                           + DMWDT(RHO11,PHI,SAT,CLIQ,DP11T,ALP11)
            DSDE(ADCP22,ADDETE) = DSDE(ADCP22,ADDETE)
     +                + DMADT(RHO22,SAT,PHI,MAMOLG,DP21T,KH,ALPHA0,BIOT)
            DSDE(ADCP12,ADDETE) = DSDE(ADCP12,ADDETE)
     +                       + DMVPDT(RHO12,SAT,PHI,H11,H12,PVP,T,ALP12)
            DSDE(ADCP21,ADDETE) = DSDE(ADCP21,ADDETE)
     +                 + DMASDT(RHO12,RHO21,SAT,PHI,PAS,H11,H12,T,ALP21)
C ======================================================================
C --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
C ======================================================================
            DSDE(ADCOTE,ADDETE)=DSDE(ADCOTE,ADDETE)+DQDT(COEPS)
            DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)+DQDP(SIGNE,ALP11,T)
            DSDE(ADCOTE,ADDEP2)=DSDE(ADCOTE,ADDEP2)
     +                                       - DQDP(SIGNE,ALP11+ALP12,T)
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
         DSDE(ADCP11,ADDEP1) = DSDE(ADCP11,ADDEP1) +
     +           DMWDP1(RHO11,SIGNE,SAT,DSATP1,BIOT,PHI,CS,CLIQ,-DP11P1)
         DSDE(ADCP11,ADDEP2) = DSDE(ADCP11,ADDEP2) +
     +                         DMWDP2(RHO11,SAT,BIOT,PHI,CS,CLIQ,DP11P2)
         DSDE(ADCP22,ADDEP1) = DSDE(ADCP22,ADDEP1) +
     +             DMADP1(RHO22,SAT,DSATP1,BIOT,PHI,CS,MAMOLG,KH,DP21P1)
         DSDE(ADCP22,ADDEP2) = DSDE(ADCP22,ADDEP2) +
     +                    DMADP2(RHO22,SAT,BIOT,PHI,CS,MAMOLG,KH,DP21P2)
         DSDE(ADCP12,ADDEP1) = DSDE(ADCP12,ADDEP1) +
     +                    DMVDP1(RHO11,RHO12,SAT,DSATP1,BIOT,PHI,CS,PVP)
         DSDE(ADCP12,ADDEP2) = DSDE(ADCP12,ADDEP2) +
     +                           DMVDP2(RHO11,RHO12,SAT,BIOT,PHI,CS,PVP)
         DSDE(ADCP21,ADDEP1) = DSDE(ADCP21,ADDEP1) +
     +           DMASP1(RHO11,RHO12,RHO21,SAT,DSATP1,BIOT,PHI,CS,P2-PVP)
         DSDE(ADCP21,ADDEP2) = DSDE(ADCP21,ADDEP2) +
     +                     DMASP2(RHO11,RHO12,RHO21,SAT,BIOT,PHI,CS,PAS)
      ENDIF
C ======================================================================
   30 CONTINUE
C ======================================================================
      END
