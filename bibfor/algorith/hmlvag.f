      SUBROUTINE HMLVAG(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICPVP,VICSAT,ADDEP1,ADCP11,
     +                  ADCP12,ADDEP2,ADCP21,ADDETE,ADCOTE,CONGEM,
     +                  CONGEP,VINTM,VINTP,DSDE,DEPS,EPSV,DEPSV,P1,P2,
     +                  DP1,DP2,T,DT,PHI,PVP,H11,H12,H21,RHO11,PHI0,
     +                  PVP0,SAT,RETCOM,THMC,CRIT,BIOT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 23/05/2005   AUTEUR JOUMANA J.EL-GHARIB 
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
C TOLE CRP_20
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
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,YAMEC
      INTEGER       YATE,RETCOM,ADCOME,ADCP11,ADCP12,ADVIHY,ADVICO
      INTEGER       VIHRHO,VICPHI,VICPVP,VICSAT
      INTEGER       ADCP21,ADCOTE,ADDEME,ADDEP1,ADDEP2,ADDETE
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON),VINTM(NBVARI),PVP0
      REAL*8        VINTP(NBVARI),DSDE(DIMCON,DIMDEF),EPSV,DEPSV
      REAL*8        P1,DP1,P2,DP2,T,DT,PHI,PVP,H11,H12,H21,RHO11,PHI0
      CHARACTER*16  OPTION,MECA,THER,HYDR,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I,IADZI,IAZK24
      REAL*8       SATM,EPSVM,PHIM,RHO11M,RHO12M,RHO21M,PVPM,BIDON
      REAL*8       RHO110,YOUNG,NU,BIOT,K0,CS,ALPHA0,ALPLIQ,CLIQ,RHO12
      REAL*8       RHO21,MASRT,CP11,CP12,CP21,SAT,DSATP1,MAMOLV,MAMOLG
      REAL*8       UMPRHS,R,RHO0,C0EPS,CSIGM,VARIA,ALP11,ALP12,ALP21
      REAL*8       VARBIO,VARLQ,VARVP,EM
      CHARACTER*8  NOMAIL
C ======================================================================
C --- VARIABLES LOCALES POUR BARCELONE-------------------------------
C ======================================================================
      REAL*8       TINI,CRIT(*)
      REAL*8       DSIDP1(6),DSIDEP(6,6),DEPS(6) 
      REAL*8       DSDEME(6,6)
CCCC    SIP NECESSAIRE POUR CALCULER LES CONTRAINTES TOTALES
CCCC    ET ENSUITE CONTRAINTES NETTES POUR BARCELONE
      REAL*8  SIPM,SIPP         
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
      INTEGER    UMESS,IUNIFI
      REAL*8     EXPMAX
      PARAMETER (EXPMAX=5.D0)
C ======================================================================
C --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
C ======================================================================
      INTEGER      NELAS
      PARAMETER  ( NELAS=4 )
      REAL*8       RBID1, RBID2, RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8       RBID8, RBID9, RBID10, RBID11, RBID12, RBID13, RBID14
      REAL*8       RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8       RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8       RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8       RBID33, RBID34, RBID35, RBID36, RBID37, RBID38
      REAL*8       RBID39, RBID40,RBID45,RBID46,RBID47,RBID48,RBID49
      REAL*8       RBID50,RBID51
      REAL*8       ELAS(NELAS)
      CHARACTER*8  NCRA1(NELAS)
      CHARACTER*2  CODRET(NELAS)
C ======================================================================
C --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES -----------
C ======================================================================
      DATA NCRA1/'E','NU','RHO','ALPHA'/
C ======================================================================
C --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
C --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
C --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
C ======================================================================
      CSIGM  = 0.D0
      RHO0   = 0.D0
      MAMOLV = 0.D0
      CP12   = 0.D0
      RETCOM = 0
      UMESS  = IUNIFI('MESSAGE')
      CALL TECAEL(IADZI,IAZK24)
      NOMAIL = ZK24(IAZK24-1+3) (1:8)
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +             P1-DP1,
     +             RBID6,
     +             RBID7, RBID8, RBID10, R, RHO0, CSIGM,
     +             BIOT, SATM, SAT, DSATP1, RBID14, RBID15,
     +             RBID16,RBID17, RBID18, RBID19, 
     +             RBID20, RBID21, RBID22,RBID23, RBID24, RBID25,
     +             RHO110, CLIQ, ALPLIQ, CP11,RBID26, RBID27, RBID28,
     +             RBID29, MAMOLG, CP21,RBID32, RBID33, RBID34, 
     +             RBID35, MAMOLV, CP12,RBID38,RBID39,RBID45,RBID46,
     +             RBID47,RBID48,RBID49,EM,RBID50,RBID51)
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      IF (YAMEC.EQ.1) THEN
          CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,NELAS,NCRA1,ELAS,
     &                CODRET,'FM')
          YOUNG  = ELAS(1)
          NU     = ELAS(2)
          ALPHA0 = ELAS(4)
          K0     = YOUNG/3.D0/ (1.D0-2.D0*NU)
          CS     = (1.D0-BIOT)/K0
      ELSE
C =====================================================================
C --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0 OU EM --------------
C =====================================================================
         ALPHA0 = 0.D0
         CS     = EM
         BIOT   = PHI0
         PHI    = PHI0
         PHIM   = PHI0
         EPSV   = 0.D0
         DEPSV  = 0.D0
      ENDIF
C =====================================================================
C --- CALCUL EPSV AU TEMPS MOINS --------------------------------------
C =====================================================================
      EPSVM = EPSV - DEPSV
C =====================================================================
C --- CALCUL DES ARGUMENTS D EXP --------------------------------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         VINTP(ADVICO+VICSAT) = SAT
      END IF
      IF (YATE.EQ.1) THEN
         IF (T.LE.0.D0) THEN
            WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ',' T <0  ',
     +                                          'A LA MAILLE: ',NOMAIL
            RETCOM = 1
            GO TO 30
         ENDIF
      ENDIF
C =====================================================================
C --- DIFFERENTS TESTS D'ESTIMATIONS ----------------------------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         VARVP = 0.D0
C =====================================================================
C --- ON FAIT ICI UNE APPROXIMATON EN REMPLACANT RHO11 PAR RHO110 -----
C =====================================================================
         VARVP = MAMOLV/R/T* (DP2-DP1)/RHO110
         IF (YATE.EQ.1) THEN
         VARVP = VARVP + (CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))*
     &             (1.D0/ (T-DT)-1.D0/T)*MAMOLV/R
         VARVP = VARVP + (CP12-CP11)* (LOG(T/ (T-DT))- (DT/T))*MAMOLV/R
         ENDIF
         IF (YAMEC.EQ.1) THEN
            VARBIO = DEPSV
            IF (YATE.EQ.1) THEN
               VARBIO = VARBIO - 3.D0*ALPHA0*DT
            ENDIF
            VARBIO = VARBIO + CS* (DP2-SAT*DP1)
         ENDIF
         VARLQ = 0.D0
         IF (YATE.EQ.1) THEN
            VARLQ = -3.D0*ALPLIQ*DT
         ELSE
            VARLQ = 0.D0
         END IF
         VARLQ = VARLQ + (DP2-DP1)*CLIQ
C =====================================================================
C --- TESTS SUR LES ESTIMATIONS DE COEF D EXPONENTIELLES --------------
C =====================================================================
         IF (YAMEC.EQ.1) THEN
            IF (-VARBIO.GT.EXPMAX) THEN
               WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ',
     &                      '-VARBIO > EXPMAX  ','A LA MAILLE: ',NOMAIL
               RETCOM = 1
               GO TO 30
            ENDIF
         ENDIF
         IF (VARLQ.NE.0.D0) THEN
            IF (VARLQ.GT.EXPMAX) THEN
               WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ',
     &                       'VARLQ > EXPMAX  ','A LA MAILLE: ',NOMAIL
               RETCOM = 1
               GO TO 30
            ENDIF
            IF (VARLQ.LT.-EXPMAX) THEN
               WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ',
     &                      'VARLQ <- EXPMAX  ','A LA MAILLE: ',NOMAIL
               RETCOM = 1
               GO TO 30
            ENDIF
        ENDIF
        IF (VARVP.GT.EXPMAX) THEN
          WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ','VARVP > EXPMAX  ',
     &      'A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
        IF (VARVP.LT.-EXPMAX) THEN
          WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ','VARVP <- EXPMAX  ',
     &      'A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
      END IF
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE PHI A L'INSTANT PLUS --------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF (YAMEC.EQ.1) THEN
            VARIA = DEPSV
            IF (YATE.EQ.1) THEN
               VARIA = VARIA - 3.D0*ALPHA0*DT
            END IF
            VARIA = VARIA + CS* (DP2-SAT*DP1)
            VINTP(ADVICO+VICPHI) = BIOT - PHI0 -
     &                     (BIOT-VINTM(ADVICO+VICPHI)-PHI0)*EXP(-VARIA)
         ENDIF
      ENDIF
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE RHO11 A L'INSTANT PLUS SI LIQUIDE -
C =====================================================================
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         VARIA = (DP2-DP1)*CLIQ
         IF (YATE.EQ.1) THEN
            VARIA = VARIA - 3.D0*ALPLIQ*DT
         ENDIF
         VINTP(ADVIHY+VIHRHO) =
     +                 -RHO110+(VINTM(ADVIHY+VIHRHO)+RHO110)*EXP(VARIA)
      ENDIF
C =====================================================================
C --- CALCUL DE PHI ET DE RHO11 (SI LIQ) A L'INSTANT COURANT ----------
C =====================================================================
      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
         IF (YAMEC.EQ.1) THEN
            PHI = VINTM(ADVICO+VICPHI) + PHI0
         ELSE
            PHI = PHI0
         ENDIF
         RHO11 = VINTM(ADVIHY+VIHRHO) + RHO110
         RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
      ELSE
         IF (YAMEC.EQ.1) THEN
            PHI = VINTP(ADVICO+VICPHI) + PHI0
            PHIM = VINTM(ADVICO+VICPHI) + PHI0
         ELSE
            PHI = PHI0
         ENDIF
         RHO11 = VINTP(ADVIHY+VIHRHO) + RHO110
         RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
      ENDIF
C =====================================================================
C --- CALCUL DES AUTRES COEFFICIENTS DEDUITS : DILATATIONS ALPHA ------
C --- ET C0EPS DANS LE CAS D'UN SEUL FLUIDE ---------------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
         IF (YAMEC.NE.1) ALPHA0 = 0.D0
         ALP11 = SAT* (BIOT-PHI)*ALPHA0 + ALPLIQ*PHI*SAT
         ALP12 = (BIOT-PHI)* (1.D0-SAT)*ALPHA0 + PHI* (1.D0-SAT)/3.D0/T
         ALP21 = ALP12
      ENDIF
C =====================================================================
C --- CALCUL ENTHALPIES ET DERIVEES DES ENTHALPIES --------------------
C --- CALCUL FAIT EN AVANCE CAR ON A BESOIN DE H11 ET H12 POUR PVP ----
C =====================================================================
      IF ((YATE.EQ.1)) THEN
         IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DSDE(ADCP11+NDIM+1,ADDEP2) = DSDE(ADCP11+NDIM+1,ADDEP2) +
     &                                 (1.D0-3.D0*ALPLIQ*T)/RHO11
          DSDE(ADCP11+NDIM+1,ADDEP1) = DSDE(ADCP11+NDIM+1,ADDEP1) -
     &                                 (1.D0-3.D0*ALPLIQ*T)/RHO11
          DSDE(ADCP11+NDIM+1,ADDETE) = DSDE(ADCP11+NDIM+1,ADDETE) + CP11
          DSDE(ADCP21+NDIM+1,ADDETE) = DSDE(ADCP21+NDIM+1,ADDETE) + CP21
          DSDE(ADCP12+NDIM+1,ADDETE) = DSDE(ADCP12+NDIM+1,ADDETE) + CP12
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
          CONGEP(ADCP11+NDIM+1) = CONGEP(ADCP11+NDIM+1) +
     &                            (1.D0-3.D0*ALPLIQ*T)/RHO11*DP2 -
     &                            (1.D0-3.D0*ALPLIQ*T)/RHO11*DP1 +
     &                            CP11*DT
          CONGEP(ADCP12+NDIM+1) = CONGEP(ADCP12+NDIM+1) + CP12*DT
          CONGEP(ADCP21+NDIM+1) = CONGEP(ADCP21+NDIM+1) + CP21*DT
         ENDIF
      ENDIF
C =====================================================================
C --- CALCUL DES ENTHALPIES INTERMEDIAIRES H11,H12,H21 ----------------
C --- UNIQUEMENT SI CHANGEMENT DE PHASE (I.E. LIQUIDE GAZ 1 ) ---------
C =====================================================================
      IF ((YATE.EQ.1)) THEN
         IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
            H11 = CONGEM(ADCP11+NDIM+1)
            H12 = CONGEM(ADCP12+NDIM+1)
            H21 = CONGEM(ADCP21+NDIM+1)
         ELSE
            H11 = CONGEP(ADCP11+NDIM+1)
            H12 = CONGEP(ADCP12+NDIM+1)
            H21 = CONGEP(ADCP21+NDIM+1)
         ENDIF
      ENDIF
C =====================================================================
C --- CALCUL DE LA VARIABLE INTERNE PRESSION VAPEUR ET DES AUTRES -----
C --- MASSES VOLUMIQUES AVEC 2 FLUIDES, AINSI QUE C0EPS ---------------
C =====================================================================
      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
         PVP = VINTM(ADVICO+VICPVP) + PVP0
         PVPM = VINTM(ADVICO+VICPVP) + PVP0
      ELSE
         VARIA = MAMOLV/R/T* (DP2-DP1)/RHO11
         IF (YATE.EQ.1) THEN
          VARIA = VARIA + (CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))*
     &             (1.D0/ (T-DT)-1.D0/T)*MAMOLV/R
          VARIA = VARIA + (CP12-CP11)* (LOG(T/ (T-DT))- (DT/T))*MAMOLV/R
         ENDIF
         VINTP(ADVICO+VICPVP) =
     +                    -PVP0 + (VINTM(ADVICO+VICPVP)+PVP0)*EXP(VARIA)
         PVP = VINTP(ADVICO+VICPVP) + PVP0
         PVPM = VINTM(ADVICO+VICPVP) + PVP0
      ENDIF
      RHO12 = MAMOLV*PVP/R/T
      RHO21 = MAMOLG* (P2-PVP)/R/T
      MASRT = MAMOLG/R/T
      IF ((P2-PVP).LT.0.D0) THEN
        WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ','PAIR <=0  ',
     &    'A LA MAILLE: ',NOMAIL
        RETCOM = 1
        GO TO 30
      END IF
      RHO12M = MAMOLV*PVPM/R/ (T-DT)
      RHO21M = MAMOLG* (P2-DP2-PVPM)/R/ (T-DT)
      IF (YATE.EQ.1) THEN
        UMPRHS = RHO0 - RHO11*SAT*PHI - (RHO12+RHO21)* (1.D0-SAT)*PHI
        IF (UMPRHS.LE.0.D0) THEN
          WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ',
     &      ' RHOS(1-PHI) <=0  ',' A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
        C0EPS = UMPRHS*CSIGM + PHI*SAT*RHO11*CP11 +
     &          PHI* (1.D0-SAT)* (RHO12*CP12+RHO21*CP21)
        IF (YAMEC.EQ.1) THEN
          C0EPS = C0EPS - 9.D0*T*K0*ALPHA0*ALPHA0
        END IF
        IF (C0EPS.LE.0.D0) THEN
          WRITE (UMESS,9001) 'CALCCO_LIQ_VAPE_GAZ','C0EPS <=0  ',
     &      'A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
      END IF
C =====================================================================
C --- CALCUL DE SIGMAP ------------------------------------------------
C =====================================================================
      IF (YAMEC.EQ.1) THEN
         IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            DSDE(ADCOME+6,ADDEP1) = DSDE(ADCOME+6,ADDEP1) + BIOT*SAT
            DSDE(ADCOME+6,ADDEP2) = DSDE(ADCOME+6,ADDEP2) - BIOT
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
          CONGEP(ADCOME+6) = CONGEP(ADCOME+6) + BIOT*SAT*DP1 - BIOT*DP2
         ENDIF
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
C --- CALCUL DES APPORTS MASSIQUES ------------------------------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
        DSDE(ADCP11,ADDEP1) = DSDE(ADCP11,ADDEP1) +
     &                        RHO11* (DSATP1*PHI-SAT*PHI*CLIQ-
     &                        SAT*SAT* (BIOT-PHI)*CS)
        DSDE(ADCP11,ADDEP2) = DSDE(ADCP11,ADDEP2) +
     &                        RHO11*SAT* (PHI*CLIQ+ (BIOT-PHI)*CS)
        DSDE(ADCP12,ADDEP1) = DSDE(ADCP12,ADDEP1) +
     &                        RHO12* (-DSATP1*PHI- (1.D0-SAT)*SAT*
     &                        (BIOT-PHI)*CS) - RHO12*PHI* (1.D0-SAT)/
     &                        PVP*RHO12/RHO11
        DSDE(ADCP12,ADDEP2) = DSDE(ADCP12,ADDEP2) +
     &                        RHO12* ((BIOT-PHI)* (1.D0-SAT)*CS) +
     &                        RHO12*PHI* (1.D0-SAT)/PVP*RHO12/RHO11
        DSDE(ADCP21,ADDEP1) = DSDE(ADCP21,ADDEP1) +
     &                        RHO21* (-DSATP1*PHI- (1.D0-SAT)*SAT*
     &                        (BIOT-PHI)*CS) + MASRT*PHI* (1.D0-SAT)*
     &                        RHO12/RHO11
        DSDE(ADCP21,ADDEP2) = DSDE(ADCP21,ADDEP2) +
     &                        RHO21* ((BIOT-PHI)* (1.D0-SAT)*CS) +
     &                        MASRT* (PHI* (1.D0-SAT)* (RHO11-RHO12)/
     &                        RHO11)
        IF (YAMEC.EQ.1) THEN
          DO 10 I = 1,3
            DSDE(ADCP11,ADDEME+NDIM-1+I) = DSDE(ADCP11,
     &        ADDEME+NDIM-1+I) + RHO11*BIOT*SAT
            DSDE(ADCP12,ADDEME+NDIM-1+I) = DSDE(ADCP12,
     &        ADDEME+NDIM-1+I) + RHO12*BIOT* (1.D0-SAT)
            DSDE(ADCP21,ADDEME+NDIM-1+I) = DSDE(ADCP21,
     &        ADDEME+NDIM-1+I) + RHO21*BIOT* (1.D0-SAT)
 10       CONTINUE
        END IF
        IF (YATE.EQ.1) THEN
          DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE) - 3.D0*RHO11*ALP11
          DSDE(ADCP12,ADDETE) = DSDE(ADCP12,ADDETE) +
     &                          RHO12*RHO12*PHI* (1.D0-SAT)/PVP*
     &                          (H12-H11)/T - 3.D0*RHO12*ALP12
          DSDE(ADCP21,ADDETE) = DSDE(ADCP21,ADDETE) -
     &                          MASRT*RHO12*PHI* (1.D0-SAT)* (H12-H11)/
     &                          T - 3.D0*RHO21*ALP21
        END IF
      END IF
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
        CONGEP(ADCP11) = CONGEM(ADCP11) + PHI*SAT*RHO11* (1.D0+EPSV) -
     &                   PHIM*SATM*RHO11M* (1.D0+EPSVM)
        CONGEP(ADCP12) = CONGEM(ADCP12) +
     &                   PHI* (1.D0-SAT)*RHO12* (1.D0+EPSV) -
     &                   PHIM* (1.D0-SATM)*RHO12M* (1.D0+EPSVM)
        CONGEP(ADCP21) = CONGEM(ADCP21) +
     &                   PHI* (1.D0-SAT)*RHO21* (1.D0+EPSV) -
     &                   PHIM* (1.D0-SATM)*RHO21M* (1.D0+EPSVM)
      ENDIF
C =====================================================================
C --- CALCUL DE LA CHALEUR REDUITE Q' ---------------------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
        IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- TERME SUIVANTE  EST SANS CONTROLE, TOUT LE MONDE Y PASSE DQ/DT --
C =====================================================================
          DSDE(ADCOTE,ADDETE) = DSDE(ADCOTE,ADDETE) + C0EPS
          IF (YAMEC.EQ.1) THEN
              DO 20 I = 1,3
                DSDE(ADCOTE,ADDEME+NDIM-1+I) = DSDE(ADCOTE,
     &            ADDEME+NDIM-1+I) + ALPHA0*YOUNG/ (1.D0-2.D0*NU)*T
 20           CONTINUE
          END IF
          DSDE(ADCOTE,ADDEP1) = DSDE(ADCOTE,ADDEP1) + 3.D0*ALP11*T
          DSDE(ADCOTE,ADDEP2) = DSDE(ADCOTE,ADDEP2) -
     &                          3.D0* (ALP11+ALP12)*T
        END IF
        IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- TERME SUIVANTE  EST SANS CONTROLE, TOUT LE MONDE Y PASSE --------
C =====================================================================
          CONGEP(ADCOTE) = CONGEP(ADCOTE) + C0EPS*DT
          IF (YAMEC.EQ.1) THEN
              CONGEP(ADCOTE) = CONGEP(ADCOTE) +
     &                         ALPHA0*YOUNG/ (1.D0-2.D0*NU)* (T-DT/2)*
     &                         DEPSV
          ENDIF
          CONGEP(ADCOTE) = CONGEP(ADCOTE) + 3.D0*ALP11* (T-DT/2.D0)*DP1
          CONGEP(ADCOTE) = CONGEP(ADCOTE) -
     &                     3.D0* (ALP11* (T-DT/2.D0)+ALP12*
     &                     (T-DT/2.D0))*DP2
         ENDIF
      ENDIF
C =====================================================================
 30   CONTINUE
C =====================================================================
 9001 FORMAT (A10,2X,A20,2X,A20,2X,A8)
C =====================================================================
      END
