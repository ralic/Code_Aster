      SUBROUTINE HMGAZP(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF,
     +             DIMCON,
     +            NVIMEC,NVITH,YAMEC,YATE,ADDEME,ADCOME,ADVIME,ADVITH,
     +                  ADDEP1,ADCP11,ADCP12,ADDEP2,ADCP21,ADCP22,
     +                  ADDETE,ADCOTE,CONGEM,CONGEP,VINTM,VINTP,DSDE,
     +                  EPSV,DEPSV,P1,P2,DP1,DP2,T,DT,PHI,PVP,H11,H12,
     +                  H21,RHO11,PHI0,PVP0,P10,P20,T0,SAT,RETCOM)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 26/09/2003   AUTEUR DURAND C.DURAND 
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
      INTEGER       NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,IMATE,YAMEC,YATE
      INTEGER       ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE
      INTEGER       ADDEME,ADDEP1,ADDEP2,ADDETE,ADVIME,ADVITH,RETCOM
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON)
      REAL*8        VINTM(NVIMEC+NVITH),VINTP(NVIMEC+NVITH)
      REAL*8        DSDE(DIMCON,DIMDEF),EPSV,DEPSV,P1,DP1,P2,DP2,T,DT
      REAL*8        PHI,PVP,H11,H12,H21,RHO11,PHI0,PVP0,P10,P20,T0
      CHARACTER*16  OPTION,MECA,THER,HYDR,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I,IADZI,IAZK24
      REAL*8       EPSVM,PHIM,RHO11M,UMPRHS
      REAL*8       YOUNG,NU,BIOT,K0,CS,ALPHA0,ALPLIQ,CP11,SAT,N,MAMOLG
      REAL*8       R,RHO0,C0EPS,CSIGM,VARIA,ALP11,VARBIO,VARLQ,VARVP
      CHARACTER*8  NOMAIL
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
      INTEGER UMESS,IUNIFI
      REAL*8 EXPMAX
      PARAMETER (EXPMAX=5.D0)
C ======================================================================
C --- DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MECANIQUES ---
C ======================================================================
      INTEGER     NELAS
      PARAMETER ( NELAS = 4 )
      REAL*8       RBID1, RBID2, RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8       RBID8, RBID9, RBID10, RBID11, RBID12, RBID13, RBID14
      REAL*8       RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8       RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8       RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8       RBID33, RBID34, RBID35, RBID36, RBID37, RBID38
      REAL*8       RBID39, RBID40, RBID41, RBID42, RBID43, RBID44
      REAL*8       ELAS(NELAS)
      CHARACTER*2  CODRET(NELAS)
      CHARACTER*8  NCRA1(NELAS)
C ======================================================================
C --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES -----------
C ======================================================================
      DATA NCRA1/'E','NU','RHO','ALPHA'/
C ======================================================================
C --- POUR EVITER DES PB AVEC OPTIMISEUR ON MET UNE VALEUR DANS CES ----
C --- VARIABES POUR QU ELLES AIENT UNE VALEUR MEME DANS LES CAS OU -----
C --- ELLES NE SONT THEOTIQUEMENT PAS UTILISEES ------------------------
C ======================================================================
      CSIGM  = 0.0D0
      RHO0   = 0.0D0
      RETCOM = 0
      UMESS  = IUNIFI('MESSAGE')
      CALL TECAEL(IADZI,IAZK24)
      NOMAIL = ZK24(IAZK24-1+3) (1:8)
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, P1, RBID6,
     +             RBID44,
     +             RBID7, RBID8, RBID9, RBID10, R, RHO0, CSIGM,
     +             BIOT, RBID12, SAT, RBID13, RBID14, RBID15, RBID16,
     +             RBID17, RBID18, RBID19, RBID20, RBID21, RBID22,
     +             RBID23, RBID24, RBID25, RBID43, RBID40, RBID41,
     +             RBID42,
     +             RBID26, RBID27, RBID28, RBID29, MAMOLG, CP11,
     +             RBID32, RBID33, RBID34, RBID35, RBID36, RBID37,
     +             RBID38, RBID39)
C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================
      IF (YAMEC.EQ.1) THEN
          CALL RCVALA(IMATE,'ELAS',0,' ',0.D0,NELAS,NCRA1,ELAS,CODRET,
     +                'FM')
          YOUNG  = ELAS(1)
          NU     = ELAS(2)
          ALPHA0 = ELAS(4)
          K0     = YOUNG/3.D0/ (1.D0-2.D0*NU)
          CS     = (1.D0-BIOT)/K0
      ELSE
C =====================================================================
C --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0 -----------------------
C =====================================================================
        ALPHA0 = 0.D0
        CS     = 0.D0
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
      IF (YATE.EQ.1) THEN
         IF (T.LE.0.D0) THEN
        WRITE (UMESS,9001) 'CALCCO_GAZ',' T <0  ','A LA MAILLE: ',
     &      NOMAIL
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
        IF (YAMEC.EQ.1) THEN
            VARBIO = DEPSV
            IF (YATE.EQ.1) THEN
              VARBIO = VARBIO - 3.D0*ALPHA0*DT
            END IF
            VARBIO = VARBIO + DP1*CS
        END IF
C =====================================================================
C --- TESTS SUR LES ESTIMATIONS DE COEF D EXPONENTIELLES --------------
C =====================================================================
        IF (YAMEC.EQ.1) THEN
          IF (-VARBIO.GT.EXPMAX) THEN
            WRITE (UMESS,9001) 'CALCCO_GAZ','-VARBIO > EXPMAX  ',
     &        'A LA MAILLE: ',NOMAIL
            RETCOM = 1
            GO TO 30
          END IF
        END IF
        IF (VARVP.GT.EXPMAX) THEN
          WRITE (UMESS,9001) 'CALCCO_GAZ','VARVP > EXPMAX  ',
     &      'A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
        IF (VARVP.LT.-EXPMAX) THEN
          WRITE (UMESS,9001) 'CALCCO_GAZ','VARVP <- EXPMAX  ',
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
            VARIA = VARIA + DP1*CS
            VINTP(ADVITH) = BIOT - PHI0 -
     &                      (BIOT-VINTM(ADVITH)-PHI0)*EXP(-VARIA)
        END IF
      END IF
C =====================================================================
C --- CALCUL DE PHI ET DE RHO11 (SI LIQ) A L'INSTANT COURANT ----------
C =====================================================================
      IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
         IF (YAMEC.EQ.1) THEN
            PHI = VINTM(ADVITH) + PHI0
         ELSE
            PHI = PHI0
         ENDIF
      ELSE
         IF (YAMEC.EQ.1) THEN
            PHI = VINTP(ADVITH) + PHI0
            PHIM = VINTM(ADVITH) + PHI0
         ELSE
            PHI = PHI0
         ENDIF
      ENDIF
      RHO11  = MAMOLG * P1/R/T
      RHO11M = MAMOLG * (P1-DP1)/R/ (T-DT)
C =====================================================================
C --- CALCUL DES AUTRES COEFFICIENTS DEDUITS : DILATATIONS ALPHA ------
C --- ET C0EPS DANS LE CAS D'UN SEUL FLUIDE ---------------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
         ALP11 = PHI/3.D0/T
         UMPRHS = (RHO0-RHO11*PHI)
         IF (UMPRHS.LE.0.D0) THEN
            WRITE (UMESS,9001) 'CALCCO_GAZ',' RHOS(1-PHI) <=0 ',
     &      '  A LA MAILLE: ',NOMAIL
            RETCOM = 1
            GO TO 30
         ENDIF
         C0EPS = UMPRHS*CSIGM + PHI*RHO11*CP11
         IF (YAMEC.EQ.1) THEN
            C0EPS = C0EPS - 9.D0*T*K0*ALPHA0*ALPHA0
         ENDIF
         IF (C0EPS.LE.0.D0) THEN
            WRITE (UMESS,9001) 'CALCCO_GAZ','C0EPS <=0  ',
     &      'A LA MAILLE: ',NOMAIL
            RETCOM = 1
            GO TO 30
         ENDIF
         IF (YAMEC.EQ.1) THEN
            ALP11 = ALP11 + ALPHA0* (BIOT-PHI)
         ENDIF
      ENDIF
C =====================================================================
C --- CALCUL ENTHALPIES ET DERIVEES DES ENTHALPIES --------------------
C --- CALCUL FAIT EN AVANCE CAR ON A BESOIN DE H11 ET H12 POUR PVP ----
C =====================================================================
      IF ((YATE.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DSDE(ADCP11+NDIM+1,ADDETE) = DSDE(ADCP11+NDIM+1,ADDETE) + CP11
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            CONGEP(ADCP11+NDIM+1) = CONGEP(ADCP11+NDIM+1) + CP11*DT
         ENDIF
      ENDIF
C =====================================================================
C --- CALCUL DE SIGMAP ------------------------------------------------
C =====================================================================
      IF ((YAMEC.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            DSDE(ADCOME+6,ADDEP1) = DSDE(ADCOME+6,ADDEP1) - BIOT
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            CONGEP(ADCOME+6) = CONGEP(ADCOME+6) - BIOT*DP1
         ENDIF
      ENDIF
C =====================================================================
C --- CALCUL DES APPORTS MASSIQUES ------------------------------------
C =====================================================================
      IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         N = (BIOT-PHI)*CS + PHI/P1
         DSDE(ADCP11,ADDEP1) = DSDE(ADCP11,ADDEP1) + RHO11*N
         IF (YATE.EQ.1) THEN
          DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE) - 3.D0*RHO11*ALP11
         ENDIF
         IF (YAMEC.EQ.1) THEN
            DO 10 I = 1,3
               DSDE(ADCP11,ADDEME+NDIM-1+I) = DSDE(ADCP11,
     &        ADDEME+NDIM-1+I) + RHO11*BIOT
 10         CONTINUE
         ENDIF
      ENDIF
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         CONGEP(ADCP11) = CONGEM(ADCP11) + PHI*RHO11* (1.D0+EPSV) -
     &                   PHIM*RHO11M* (1.D0+EPSVM)
      ENDIF
C =====================================================================
C --- CALCUL DE LA CHALEUR REDUITE Q' ---------------------------------
C =====================================================================
      IF (YATE.EQ.1) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
C =====================================================================
C --- TERME SUIVANT EST SANS CONTROLE, TOUT LE MONDE Y PASSE DQ/DT ----
C =====================================================================
          DSDE(ADCOTE,ADDETE) = DSDE(ADCOTE,ADDETE) + C0EPS
          IF (YAMEC.EQ.1) THEN
              DO 20 I = 1,3
                DSDE(ADCOTE,ADDEME+NDIM-1+I) = DSDE(ADCOTE,
     &            ADDEME+NDIM-1+I) + ALPHA0*YOUNG/ (1.D0-2.D0*NU)*T
 20           CONTINUE
          END IF
          DSDE(ADCOTE,ADDEP1) = DSDE(ADCOTE,ADDEP1) - 3.D0*ALP11*T
        END IF
C =====================================================================
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
          END IF
          CONGEP(ADCOTE) = CONGEP(ADCOTE) - 3.D0*ALP11* (T-DT/2)*DP1
        END IF
      END IF
 30   CONTINUE
C =====================================================================
 9001 FORMAT (A10,2X,A20,2X,A20,2X,A8)
C =====================================================================
      END
