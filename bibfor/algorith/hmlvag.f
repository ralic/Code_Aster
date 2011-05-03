      SUBROUTINE HMLVAG(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICPVP,VICSAT,ADDEP1,ADCP11,
     +                  ADCP12,ADDEP2,ADCP21,ADDETE,ADCOTE,CONGEM,
     +                  CONGEP,VINTM,VINTP,DSDE,DEPS,EPSV,DEPSV,P1,P2,
     +                  DP1,DP2,T,DT,PHI,PVP,H11,H12,RHO11,PHI0,
     +                  PVP0,SAT,RETCOM,THMC,CRIT,BIOT,RINSTP)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/05/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GRANET S.GRANET
C TOLE CRP_21
C ======================================================================
C ROUTINE HMLVAG : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISE
C   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
C   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
C   DANS LE CAS OU THMC = 'LIQU_VAPE_GAZ'
C ======================================================================
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C COMMENTAIRE DE NMCONV :
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C ======================================================================
      IMPLICIT NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,YAMEC
      INTEGER       YATE,RETCOM,ADCOME,ADCP11,ADCP12,ADVIHY,ADVICO
      INTEGER       VIHRHO,VICPHI,VICPVP,VICSAT
      INTEGER       ADCP21,ADCOTE,ADDEME,ADDEP1,ADDEP2,ADDETE
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON),VINTM(NBVARI),PVP0
      REAL*8        VINTP(NBVARI),DSDE(DIMCON,DIMDEF),EPSV,DEPSV
      REAL*8        P1,DP1,P2,DP2,T,DT,PHI,PVP,H11,H12,RHO11,PHI0
      REAL*8        RINSTP
      CHARACTER*16  OPTION,MECA,THER,HYDR,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I
      REAL*8       SATM,EPSVM,PHIM,RHO11M,RHO12M,RHO21M,PVPM
      REAL*8       RHO110,BIOT,K0,CS,ALPHA0,ALPLIQ,CLIQ,RHO12
      REAL*8       RHO21,CP11,CP12,CP21,SAT,DSATP1,MAMOLV,MAMOLG
      REAL*8       R,RHO0,CSIGM,ALP11,ALP12,ALP21,EM,EPS
      PARAMETER  ( EPS = 1.D-21 )
      LOGICAL      EMMAG
C ======================================================================
C --- VARIABLES LOCALES POUR BARCELONE----------------------------------
C ======================================================================
      REAL*8       TINI,CRIT(*)
      REAL*8       DSIDP1(6),DEPS(6)
      REAL*8       DSDEME(6,6)
CCCC    SIP NECESSAIRE POUR CALCULER LES CONTRAINTES TOTALES
CCCC    ET ENSUITE CONTRAINTES NETTES POUR BARCELONE
      REAL*8  SIPM,SIPP
C ======================================================================
C --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
C ======================================================================
      REAL*8       RBID1, RBID2, RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8       RBID8, RBID10, RBID14(3)
      REAL*8       RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8       RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8       RBID27, RBID28, RBID29, RBID32
      REAL*8       RBID33, RBID34, RBID35, RBID38
      REAL*8       RBID39,RBID45,RBID46,RBID47,RBID48,RBID49
      REAL*8       RBID50,RBID51
      REAL*8       R3BID(6)
      REAL*8       SIGNE,DPAD,COEPS,CP22,PAS,RHO22,M11M,M12M,M21M
      REAL*8       DMASP1,DMASP2,DMVDP1,DMVDP2,DMWDP1,DMWDP2,DQDEPS
      REAL*8       DQDP,DQDT,DMASDT,DMVPDT,DMWDT,DHDT,DHWDP1,DHWDP2
      REAL*8       DMDEPV,DSPDP1,DSPDP2,APPMAS,SIGMAP,CALOR,ENTEAU
      REAL*8       ENTGAZ,DILEAU,DILGAZ,MAJPAS,MASVOL
C
      LOGICAL NET,BISHOP
C
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      CALL NETBIS(MECA,NET,BISHOP)
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +             P1-DP1,RBID6, RBID7, RBID8,
     +             RBID10, R, RHO0, CSIGM,BIOT, SATM, SAT,
     +             DSATP1, RBID14, RBID15, RBID16, RBID17, RBID18,
     +             RBID19, RBID20, RBID21, RBID22, RBID23, RBID24,
     +             RBID25, RHO110, CLIQ,   ALPLIQ, CP11,   RBID26,
     +             RBID27, RBID28, RBID29, MAMOLG, CP21,   RBID32,
     +             RBID33, RBID34, RBID35, MAMOLV, CP12,   RBID38,
     +             RBID39,RBID45,RBID46,   RBID47, RBID48, RBID49,
     +             EM,RBID50,R3BID,RBID51,RINSTP)
C ======================================================================
C --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
C --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
C --- ELLES NE SONT THEORIQUEMENT PAS UTILISEES ------------------------
C ======================================================================
      EMMAG = .FALSE.
      RETCOM = 0
      DPAD   = 0.0D0
      SIGNE  = 1.0D0
      RHO22  = 0.0D0
      CP22   = 0.0D0
      M11M   = CONGEM(ADCP11)
      M12M   = CONGEM(ADCP12)
      M21M   = CONGEM(ADCP21)
      PVP    = VINTM(ADVICO+VICPVP) + PVP0
      PVPM   = VINTM(ADVICO+VICPVP) + PVP0
      PHI    = VINTM(ADVICO+VICPHI) + PHI0
      PHIM   = VINTM(ADVICO+VICPHI) + PHI0
      RHO11  = VINTM(ADVIHY+VIHRHO) + RHO110
      RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      IF((EM.GT.EPS).AND.(YAMEC.EQ.0))THEN
        EMMAG = .TRUE.
      ENDIF
      CALL INITHM(IMATE,YAMEC,PHI0,EM,ALPHA0,K0,CS,BIOT,T,
     +                                           EPSV,DEPSV,EPSVM,MECA)
C *********************************************************************
C *** LES VARIABLES INTERNES ******************************************
C *********************************************************************
      IF ((OPTION.EQ.'RAPH_MECA') .OR.
     &    (OPTION.EQ.'FORC_NODA').OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
C =====================================================================
         IF ((YAMEC.EQ.1))THEN
            CALL VIPORO(NBVARI,VINTM,VINTP,ADVICO,VICPHI,PHI0,
     +       DEPSV,ALPHA0,DT,DP1,DP2,SIGNE,SAT,CS,BIOT,PHI,PHIM,RETCOM)
         ENDIF
         IF (EMMAG )THEN
            CALL VIEMMA(NBVARI,VINTM,VINTP,ADVICO,VICPHI,PHI0,
     +       DP1,DP2,SIGNE,SAT,EM,PHI,PHIM,RETCOM)
         ENDIF
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE MASSE VOLUMIQUE DU FLUIDE ------
C --- SELON FORMULE DOCR ----------------------------------------------
C =====================================================================
         CALL VIRHOL(NBVARI,VINTM,VINTP,ADVIHY,VIHRHO,RHO110,
     +           DP1,DP2,DPAD,CLIQ,DT,ALPLIQ,SIGNE,RHO11,RHO11M,RETCOM)
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE PRESSION DE VAPEUR -------------
C --- SELON FORMULE DOCR ----------------------------------------------
C =====================================================================
         CALL VIPVP1(NBVARI,VINTM,VINTP,ADVICO,VICPVP,DIMCON,P2,
     +      CONGEM,ADCP11,ADCP12,NDIM,PVP0,DP1,DP2,T,DT,MAMOLV,R,RHO11,
     +                            SIGNE,CP11,CP12,YATE,PVP,PVPM,RETCOM)
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

C **********************************************************************
C *** LES CONTRAINTES GENERALISEES *************************************
C **********************************************************************
      RHO12  = MASVOL(MAMOLV,PVP        ,R,T   )
      RHO12M = MASVOL(MAMOLV,PVPM       ,R,T-DT)
      RHO21  = MASVOL(MAMOLG,P2-PVP     ,R,T   )
      RHO21M = MASVOL(MAMOLG,P2-DP2-PVPM,R,T-DT)
      PAS    = MAJPAS(P2,PVP)
C =====================================================================
C --- CALCUL DES AUTRES COEFFICIENTS DEDUITS : DILATATIONS ALPHA ------
C ---  DANS LE CAS D'UN SEUL FLUIDE ---------------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
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
         IF ((OPTION.EQ.'RAPH_MECA') .OR.
     +       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            CONGEP(ADCP11+NDIM+1) = CONGEP(ADCP11+NDIM+1)
     +               + ENTEAU(DT,ALPLIQ,T,RHO11,DP2,DP1,DPAD,SIGNE,CP11)
            CONGEP(ADCP12+NDIM+1) = CONGEP(ADCP12+NDIM+1)
     +                                                 + ENTGAZ(DT,CP12)
            CONGEP(ADCP21+NDIM+1) = CONGEP(ADCP21+NDIM+1)
     +                                                 + ENTGAZ(DT,CP21)
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
      IF ((OPTION.EQ.'RAPH_MECA') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C ======================================================================
C --- CALCUL DES CONTRAINTES DE PRESSIONS ------------------------------
C ======================================================================
         IF (YAMEC.EQ.1) THEN
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)
     +                  + SIGMAP(NET,BISHOP,SAT,SIGNE,BIOT,DP2,DP1)
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
     +                      + DSPDP1(NET,BISHOP,SIGNE,BIOT,SAT)
            DSDE(ADCOME+6,ADDEP2)=DSDE(ADCOME+6,ADDEP2)
     >              +DSPDP2(NET,BISHOP,BIOT)
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
 10         CONTINUE
         ENDIF
         IF (YATE. EQ.1) THEN
C ======================================================================
C --- CALCUL UNIQUEMENT EN PRESENCE DE THERMIQUE -----------------------
C ======================================================================
C --- CALCUL DES DERIVEES DES ENTHALPIES -------------------------------
C ======================================================================
            DSDE(ADCP11+NDIM+1,ADDEP2) = DSDE(ADCP11+NDIM+1,ADDEP2)
     +                                          + DHWDP2(ALPLIQ,T,RHO11)
            DSDE(ADCP11+NDIM+1,ADDEP1)=DSDE(ADCP11+NDIM+1,ADDEP1)
     +                                    + DHWDP1(SIGNE,ALPLIQ,T,RHO11)
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)
     +                                                      + DHDT(CP11)
            DSDE(ADCP12+NDIM+1,ADDETE)=DSDE(ADCP12+NDIM+1,ADDETE)
     +                                                      + DHDT(CP12)
            DSDE(ADCP21+NDIM+1,ADDETE)=DSDE(ADCP21+NDIM+1,ADDETE)
     +                                                      + DHDT(CP21)
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
C ======================================================================
            DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE) +
     +                             DMWDT(RHO11,PHI,SAT,CLIQ,0.0D0,ALP11)
            DSDE(ADCP12,ADDETE) = DSDE(ADCP12,ADDETE) +
     +                         DMVPDT(RHO12,SAT,PHI,H11,H12,PVP,T,ALP12)
            DSDE(ADCP21,ADDETE) = DSDE(ADCP21,ADDETE) +
     +                   DMASDT(RHO12,RHO21,SAT,PHI,PAS,H11,H12,T,ALP21)
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
         DSDE(ADCP11,ADDEP1) = DSDE(ADCP11,ADDEP1)
     +           + DMWDP1(RHO11,SIGNE,SAT,DSATP1,BIOT,PHI,CS,CLIQ,1.0D0,
     >                  EMMAG,EM)
         DSDE(ADCP11,ADDEP2) = DSDE(ADCP11,ADDEP2)
     +                        + DMWDP2(RHO11,SAT,BIOT,PHI,CS,CLIQ,1.0D0,
     >                  EMMAG,EM)
         DSDE(ADCP12,ADDEP1) = DSDE(ADCP12,ADDEP1) +
     +                    DMVDP1(RHO11,RHO12,SAT,DSATP1,BIOT,PHI,CS,PVP,
     &                          EMMAG,EM)
         DSDE(ADCP12,ADDEP2) = DSDE(ADCP12,ADDEP2) +
     +                           DMVDP2(RHO11,RHO12,SAT,BIOT,PHI,CS,PVP,
     &                                 EMMAG,EM)
         DSDE(ADCP21,ADDEP1) = DSDE(ADCP21,ADDEP1) +
     +              DMASP1(RHO11,RHO12,RHO21,SAT,DSATP1,BIOT,PHI,CS,PAS,
     +                     EMMAG,EM)
         DSDE(ADCP21,ADDEP2) = DSDE(ADCP21,ADDEP2) +
     +                     DMASP2(RHO11,RHO12,RHO21,SAT,BIOT,PHI,CS,PAS,
     +                             EMMAG,EM)
      ENDIF

C =====================================================================
C --- TERMES SPECIAL BARCELONE --------------------------------------
C =====================================================================
      IF ((YAMEC.EQ.1).AND.(MECA.EQ.'BARCELONE')) THEN
        TINI = T-DT
        SIPM=CONGEM(ADCOME+6)
        SIPP=CONGEP(ADCOME+6)
        CALL NMBARC(  NDIM,  IMATE,  CRIT, SAT, BIOT,
     &                      TINI,T,
     >                      DEPS,
     >                      CONGEM(ADCOME), VINTM, OPTION,
     >                      CONGEP(ADCOME), VINTP,
     >                      DSDEME,P1,P2,DP1,DP2,
     &                      DSIDP1,SIPM,SIPP,RETCOM)
         IF (RETCOM.EQ.1) GO TO 30

         IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
C --- DSIGM/DEPP1
            DO 50 I = 1 , 2*NDIM
            DSDE(ADCOME+I-1,ADDEP1) = DSDE(ADCOME+I-1,ADDEP1) +
     &                               DSIDP1(I)
   50       CONTINUE
         ENDIF
      ENDIF
C =====================================================================
 30   CONTINUE
C =====================================================================
      END
