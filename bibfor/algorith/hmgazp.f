      SUBROUTINE HMGAZP(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF,
     +                  DIMCON,NBVARI,YAMEC,YATE,ADDEME,ADCOME,
     +                  ADVICO,VICPHI,ADDEP1,ADCP11,ADDETE,ADCOTE,
     +                  CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,DEPSV,P1,
     +                  DP1,T,DT,PHI,RHO11,PHI0,SAT,RETCOM,BIOT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 22/07/2008   AUTEUR PELLET J.PELLET 
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
C ROUTINE HMGAZP : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
C   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
C   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS 
C   DANS LE CAS OU THMC = 'GAZ'
C ======================================================================
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C COMMENTAIRE DE NMCONV :
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,YAMEC,YATE
      INTEGER       ADCOME,ADCP11,ADCOTE
      INTEGER       ADDEME,ADDEP1,ADDETE,ADVICO,VICPHI,RETCOM
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON)
      REAL*8        VINTM(NBVARI),VINTP(NBVARI)
      REAL*8        DSDE(DIMCON,DIMDEF),EPSV,DEPSV,P1,DP1,T,DT
      REAL*8        PHI,RHO11,PHI0
      CHARACTER*16  OPTION,MECA,THER,HYDR,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I
      REAL*8       EPSVM,PHIM,RHO11M,UMPRHS
      REAL*8       YOUNG,NU,BIOT,K0,CS,ALPHA0,ALPLIQ,CP12,SAT,N,MAMOLG
      REAL*8       R,RHO0,C0EPS,CSIGM,VARIA,ALP11,VARBIO,VARLQ,VARVP,EM
      REAL*8       EPS
      PARAMETER  ( EPS = 1.D-21 ) 
      LOGICAL      EMMAG
C ======================================================================
C --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
C ======================================================================
      REAL*8       RBID1, RBID2, RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8       RBID8, RBID9, RBID10, RBID11, RBID12, RBID13, RBID14
      REAL*8       RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8       RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8       RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8       RBID33, RBID34, RBID35, RBID36, RBID37, RBID38
      REAL*8       RBID39, RBID40, RBID41, RBID42, RBID43, RBID44
      REAL*8       RBID45,RBID46,RBID47,RBID48,RBID49,RBID50,RBID51
      REAL*8       SIGNE,DP2,CLIQ,COEPS,RHO12,ALP21,RHO21,RHO21M
      REAL*8       CP21,P2,SATM,RHO22,CP11,CP22,M11M
      REAL*8       DMASP2,DQDEPS,DQDP,DQDT,DMWDT,DHDT,DMDEPV,DSPDP2
      REAL*8       APPMAS,SIGMAP,CALOR,ENTGAZ,DILGAZ,MASVOL
C
      LOGICAL NET,BISHOP
C
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C --- DANS LE CALCUL ON LAISSE LES INDICES 11 POUR LE STOCKAGE DES ----
C --- VARIABLES. EN REVANCHE POUR UNE MEILLEURE COMPREHENSION PAR -----
C --- RAPPORT A LA DOC R7.01.11 ON NOTE LES INDICES 21 POUR LES -------
C --- VARIABLES DE CALCUL ---------------------------------------------
C =====================================================================
      EMMAG = .FALSE.
      CALL NETBIS(MECA,NET,BISHOP)
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, P1, RBID6,
     +             RBID44,
     +             RBID7, RBID8, RBID10, R, RHO0, CSIGM,
     +             BIOT, RBID12, SAT, RBID13, RBID14, RBID15, RBID16,
     +             RBID17, RBID18, RBID19, RBID20, RBID21, RBID22,
     +             RBID23, RBID24, RBID25, RBID43, RBID40, RBID41,
     +             RBID42,RBID26, RBID27, RBID28, RBID29,
     +             MAMOLG, CP21,RBID32, RBID33, RBID34, RBID35, 
     +             RBID36, RBID37,RBID38, RBID39,RBID45,RBID46,
     +             RBID47,RBID48,RBID49,EM,RBID50,RBID51)
C ======================================================================
C --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
C --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
C --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
C =====================================================================
C --- ON INVERSE POSE DP2 = DP1 ET DP1 = 0 POUR CONFOMITE A LA FORMULE-
C =====================================================================
      RETCOM = 0
      SIGNE  = 1.0D0
      P2     = P1
      DP2    = DP1
      DP1    = 0.0D0
      SAT    = 0.0D0
      SATM   = 0.0D0
      ALP11  = 0.0D0
      RHO11  = 0.0D0
      RHO21  = 0.0D0
      RHO22  = 0.0D0
      CP11   = 0.0D0
      CP12   = 0.0D0
      CP22   = 0.0D0
      CLIQ   = 0.0D0
      M11M   = CONGEM(ADCP11)
      PHI    = VINTM(ADVICO+VICPHI) + PHI0
      PHIM   = VINTM(ADVICO+VICPHI) + PHI0
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      IF(EM.GT.EPS)THEN 
        EMMAG = .TRUE.
      ENDIF
        
      CALL INITHM(IMATE,YAMEC,PHI0,EM,ALPHA0,K0,CS,BIOT,T,
     +                                       EPSV,DEPSV,EPSVM,MECA)

C *********************************************************************
C *** LES VARIABLES INTERNES ******************************************
C *********************************************************************
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE DE POROSITE SELON FORMULE DOCR ----
C =====================================================================
         IF ((YAMEC.EQ.1).OR.EMMAG )THEN
            CALL VIPORO(NBVARI,VINTM,VINTP,ADVICO,VICPHI,PHI0,
     +       DEPSV,ALPHA0,DT,DP1,DP2,SIGNE,SAT,CS,BIOT,PHI,PHIM,RETCOM)
         ENDIF
C =====================================================================
C --- PROBLEME DANS LE CALCUL DES VARIABLES INTERNES ? ----------------
C =====================================================================
         IF (RETCOM.NE.0) THEN
            GO TO 30
         ENDIF
      ENDIF
C =====================================================================
C --- CALCUL DE LA MASSE VOLUMIQUE DU GAZ AUX INSTANT PLUS ET MOINS ---
C =====================================================================
      RHO21  = MASVOL(MAMOLG,P2    ,R,T   )
      RHO21M = MASVOL(MAMOLG,P2-DP2,R,T-DT)

C =====================================================================
C --- CALCULS UNIQUEMENT SI PRESENCE DE THERMIQUE ---------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
C =====================================================================
C --- CALCUL DES COEFFICIENTS DE DILATATIONS ALPHA SELON FORMULE DOCR -
C =====================================================================
         ALP21 = DILGAZ(SAT,BIOT,PHI,ALPHA0,T     )
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
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)+ENTGAZ(DT,CP21)
C ======================================================================
C --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
C ======================================================================
            CONGEP(ADCOTE) = CONGEP(ADCOTE)
     +     + CALOR(ALPHA0,K0,T,DT,DEPSV,DP1,DP2,SIGNE,ALP11,ALP21,COEPS)
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
     +                + SIGMAP(NET,BISHOP,SAT,SIGNE,BIOT,DP2,DP1)
         END IF
C ======================================================================
C --- CALCUL DES APPORTS MASSIQUES SELON FORMULE DOCR ------------------
C ======================================================================
         CONGEP(ADCP11) = APPMAS(M11M,PHI,PHIM,1.0D0-SAT,
     +                               1.0D0-SATM,RHO21,RHO21M,EPSV,EPSVM)
      ENDIF

C **********************************************************************
C *** CALCUL DES DERIVEES **********************************************
C **********************************************************************
      IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF (YAMEC.EQ.1) THEN
C ======================================================================
C --- CALCUL UNIQUEMENT EN PRESENCE DE MECANIQUE -----------------------
C ======================================================================
C --- CALCUL DES DERIVEES DE SIGMAP ------------------------------------
C ======================================================================
       DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)+
     >  DSPDP2(NET,BISHOP,BIOT)
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIE MECANIQUE ------------------------------
C ======================================================================
            DO 10 I = 1,3
               DSDE(ADCP11,ADDEME+NDIM-1+I) =
     +       DSDE(ADCP11,ADDEME+NDIM-1+I) + DMDEPV(RHO21,1.0D0-SAT,BIOT)
 10         CONTINUE
         ENDIF
         IF (YATE. EQ.1) THEN
C ======================================================================
C --- CALCUL UNIQUEMENT EN PRESENCE DE THERMIQUE -----------------------
C ======================================================================
C --- CALCUL DES DERIVEES DES ENTHALPIES -------------------------------
C ======================================================================
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)
     +                                                      + DHDT(CP21)
C ======================================================================
C --- CALCUL DES DERIVEES DES APPORTS MASSIQUES ------------------------
C --- UNIQUEMENT POUR LA PARTIR THERMIQUE ------------------------------
C ======================================================================
            DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE)
     +                           + DMWDT(RHO21,PHI,SAT,CLIQ,0.0D0,ALP21)
C ======================================================================
C --- CALCUL DE LA DERIVEE DE LA CHALEUR REDUITE Q' --------------------
C ======================================================================
            DSDE(ADCOTE,ADDETE)=DSDE(ADCOTE,ADDETE)+DQDT(COEPS)
            DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)-DQDP(SIGNE,ALP21,T)
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
     +                    + DMASP2(1.0D0,0.0D0,RHO21,SAT,BIOT,PHI,CS,P2)
      ENDIF
C =====================================================================
C --- MISE A JOUR DES VARIABLES P1 ET DP1 POUR CONFOMITE AUX FORMULES -
C =====================================================================
      P1     = P2
      DP1    = DP2
      RHO11  = RHO21
C =====================================================================
 30   CONTINUE
C =====================================================================
      END
