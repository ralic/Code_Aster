      SUBROUTINE HMLGAT(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICSAT,ADDEP1,ADCP11,ADDETE,
     +                  ADCOTE,CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,
     +                 DEPSV,P1,DP1,T,DT,PHI,RHO11,PHI0,SAT,RETCOM,THMC)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 31/01/2005   AUTEUR ROMEO R.FERNANDES 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C ======================================================================
C ROUTINE HMLGAT : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEE
C   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
C   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
C   DANS LE CAS OU THMC = 'LIQU_GAZ_ATM'
C ======================================================================
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C COMMENTAIRE DE NMCONV :
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,RETCOM,YAMEC
      INTEGER       YATE,ADCOME,ADCP11,ADCOTE,ADDEME,ADDEP1,ADDETE
      INTEGER       ADVIHY,ADVICO,VIHRHO,VICPHI,VICSAT
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON),VINTM(NBVARI)
      REAL*8        VINTP(NBVARI),DSDE(DIMCON,DIMDEF),EPSV,DEPSV
      REAL*8        P1,DP1,T,DT,PHI,RHO11,PHI0
      CHARACTER*16  OPTION,MECA,THER,HYDR,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I
      REAL*8       SATM,EPSVM,PHIM,RHO11M,RHO110,YOUNG,NU,BIOT,K0,CS
      REAL*8       ALPHA0,ALPLIQ,CLIQ,CP11,SAT,DSATP1,RHO0,C0EPS,CSIGM
      REAL*8       VARIA,ALP11,UMPRHS,RHO12,RHO21,MASRT,VARBIO,VARLQ
      REAL*8       VARVP,EM
C ======================================================================
C --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
C ======================================================================
      REAL*8       RBID1, RBID2, RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8       RBID8, RBID9, RBID10, RBID11, RBID12, RBID13, RBID14
      REAL*8       RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8       RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8       RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8       RBID33, RBID34, RBID35, RBID36, RBID37, RBID38
      REAL*8       RBID39, RBID40,RBID45,RBID46,RBID47,RBID48,RBID49
      REAL*8       RBID50,RBID51
      REAL*8       DP2,SIGNE,DPAD,COEPS,CP21,M11M,RHO22,ALP12,CP12
      REAL*8       DMWDP1,DQDEPS,DQDP,DQDT,DMWDT,DHDT,DHWDP1,DMDEPV
      REAL*8       DSPDP1,APPMAS,SIGMAP,CALOR,ENTEAU,DILEAU,CP22
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,P1-DP1,
     +             RBID6,RBID7, RBID8, RBID10, RBID11, RHO0,
     +             CSIGM,BIOT, SATM, SAT, DSATP1, RBID14, 
     +             RBID15, RBID16,RBID17, RBID18, RBID19, 
     +             RBID20, RBID21, RBID22,RBID23, RBID24, RBID25, 
     +             RHO110, CLIQ, ALPLIQ, CP11,RBID26, RBID27, RBID28, 
     +             RBID29, RBID30, RBID31,RBID32, RBID33, RBID34, 
     +             RBID35, RBID36, RBID37,RBID38, RBID39,RBID45,RBID46,
     +             RBID47,RBID48,RBID49,EM,RBID50,RBID51)
C ======================================================================
C --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
C --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
C --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
C ======================================================================
      CP12   = 0.0D0
      CP21   = 0.0D0
      CP22   = 0.0D0
      ALP11  = 0.0D0
      ALP12  = 0.0D0
      RHO12  = 0.0D0
      RHO21  = 0.0D0
      RHO22  = 0.0D0
      SIGNE  = 1.0D0
      DP2    = 0.0D0
      DPAD   = 0.0D0
      RETCOM = 0
      M11M   = CONGEM(ADCP11)
      RHO11  = VINTM(ADVIHY+VIHRHO) + RHO110
      RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
      PHI    = VINTM(ADVICO+VICPHI) + PHI0
      PHIM   = VINTM(ADVICO+VICPHI) + PHI0
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      CALL INITHM(IMATE,YAMEC,PHI0,EM,ALPHA0,K0,CS,BIOT,
     +                                                EPSV,DEPSV,EPSVM)

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
C --- CALCUL DE LA VARIABLE INTERNE DE MASSE VOLUMIQUE DU FLUIDE ------
C --- SELON FORMULE DOCR ----------------------------------------------
C =====================================================================
         CALL VIRHOL(NBVARI,VINTM,VINTP,ADVIHY,VIHRHO,RHO110,
     +           DP1,DP2,DPAD,CLIQ,DT,ALPLIQ,SIGNE,RHO11,RHO11M,RETCOM)
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
C =====================================================================
C --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
C =====================================================================
C --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
C =====================================================================
         ALP11 = DILEAU(SAT,BIOT,PHI,ALPHA0,ALPLIQ)
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
     +               + ENTEAU(DT,ALPLIQ,T,RHO11,DP2,DP1,DPAD,SIGNE,CP11)
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
     +                                          + DSPDP1(SIGNE,BIOT,SAT)
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
C ======================================================================
            DO 10 I = 1,3
               DSDE(ADCP11,ADDEME+NDIM-1+I) =
     +             DSDE(ADCP11,ADDEME+NDIM-1+I) + DMDEPV(RHO11,SAT,BIOT)
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
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
C ======================================================================
            DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE)
     +                           + DMWDT(RHO11,PHI,SAT,CLIQ,0.0D0,ALP11)
C ======================================================================
C --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
C ======================================================================
            DSDE(ADCOTE,ADDETE)=DSDE(ADCOTE,ADDETE)+DQDT(COEPS)
            DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)+DQDP(SIGNE,ALP11,T)
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
     +           + DMWDP1(RHO11,SIGNE,SAT,DSATP1,BIOT,PHI,CS,CLIQ,1.0D0)
      ENDIF
C =====================================================================
 30   CONTINUE
C =====================================================================
      END
