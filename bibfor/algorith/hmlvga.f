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
C MODIF ALGORITH  DATE 06/08/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C TOLE CRP_20
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
      INTEGER      I,IADZI,IAZK24
      REAL*8 SATM,EPSVM,PHIM,RHO11M,RHO12M,RHO21M,PVPM,RHO22M
      INTEGER ELA,CJS,CAMCLA,LAIGLE,MAZARS,ENISBE
      LOGICAL PORELA
      REAL*8 YOUNG,NU,BIOT,K0,CS,ALPHA0,ALPLIQ,CLIQ,RHO110
      REAL*8 CP11,CP12,CP21,SAT,DSATP1,MAMOLV,MAMOLG
      REAL*8 R,RHO0,C0EPS,CSIGM,VARIA,ALP11,ALP12,ALP21,ALP22
      REAL*8 DP11T, DP11P1, DP11P2, DP12T, DP12P1, DP12P2
      REAL*8 DP21T, DP21P1, DP21P2, DP22T, DP22P1, DP22P2, L
      REAL*8 UMPRHS, BIDON,RHO12,RHO21,MASRT,CP22
      REAL*8 PADM,RHO22,VARPAD,VARBIO,VARLQ,EM
      CHARACTER*8 NOMAIL

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
      REAL*8       RBID39, RBID40,RBID45,RBID46,RBID49,RBID50
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
      CSIGM = 0.D0
      RHO0 = 0.D0
      MAMOLV = 0.D0
      CP12 = 0.D0
      RETCOM = 0
      UMESS = IUNIFI('MESSAGE')
      CALL TECAEL(IADZI,IAZK24)
      NOMAIL = ZK24(IAZK24-1+3) (1:8)

C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      CALL THMRCP( 'INTERMED', IMATE, THMC, MECA, HYDR, THER,
     +             RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +             P1-DP1,
     +             RBID6,
     +             RBID7, RBID8, RBID9, RBID10, R, RHO0, CSIGM,
     +             BIOT, SATM, SAT, DSATP1, RBID14, RBID15,
     +             RBID16,
     +             RBID17, RBID18, RBID19, RBID20, RBID21, RBID22,
     +             RBID23, RBID24, RBID25, RHO110, CLIQ, ALPLIQ, CP11,
     +             RBID26, RBID27, RBID28, RBID29, MAMOLG, CP21,
     +             RBID32, RBID33, RBID34, RBID35, MAMOLV, CP12,RBID38,
     +             RBID39,RBID45,RBID46,CP22,KH,RBID49,EM,RBID50)

C =====================================================================
C --- RECUPERATION DES COEFFICIENTS MECANIQUES ------------------------
C =====================================================================

      IF (YAMEC.EQ.1) THEN
          CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,NELAS,NCRA1,ELAS,
     &               CODRET, 'FM')
          YOUNG = ELAS(1)
          NU = ELAS(2)
          ALPHA0 = ELAS(4)
          K0 = YOUNG/3.D0/ (1.D0-2.D0*NU)
          CS = (1.D0-BIOT)/K0
      ELSE
C =====================================================================
C --- EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0 OU EM -------------
C =====================================================================
        ALPHA0 = 0.D0
        CS = EM
        BIOT = PHI0
        PHI = PHI0
        PHIM = PHI0
        EPSV = 0.D0
        DEPSV = 0.D0
      END IF
C =====================================================================
C --- CALCUL EPSV AU TEMPS MOINS --------------------------------------
C =====================================================================
      EPSVM = EPSV - DEPSV

      
C fin traitement provisoire      

      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
        VINTP(ADVICO+VICSAT) = SAT
      END IF
C   CALCUL DES ARGUMENTS D EXP

      IF (YATE.EQ.1) THEN
        IF (T.LE.0.D0) THEN
          WRITE (UMESS,9001) 'CALCCO_LIQU_AD_GAZ_VAPE',' T <0  ',
     &    'A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
      END IF
C =====================================================================
C --- DIFFERENTS TESTS D'ESTIMATIONS ----------------------------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN

C
C ON FAIT ICI UNE APPROXIMATON EN REMPLACANT RHO11 PAR RHO110
C
        VARIA = MAMOLV*(1/R/T-1/KH)* DP2/RHO110-MAMOLV/R/T*DP1/RHO110
        IF (YATE.EQ.1) THEN
          VARIA = VARIA + (CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))*
     &             (1.D0/ (T-DT)-1.D0/T)*MAMOLV/R
          VARIA = VARIA + (CP12-CP11)* (LOG(T/ (T-DT))- (DT/T))*MAMOLV/R
        END IF
        PVPM = VINTM(ADVICO+VICPVP) + PVP0
        VARIA = (PVPM)*EXP(VARIA)
        PVP = MAMOLV*(PVPM+(P2-DP2)*R*LOG(T/ (T-DT)))
        PVP = RHO110*KH-PVP
        PVP = PVP/((RHO110*KH/VARIA)-MAMOLV*(1+R*LOG(T/ (T-DT))))
        VARPAD = (DP2 -(PVP-PVPM))*R*T/KH
C
        IF (YAMEC.EQ.1) THEN
            VARBIO = DEPSV
            IF (YATE.EQ.1) THEN
              VARBIO = VARBIO - 3.D0*ALPHA0*DT
            END IF
            VARBIO = VARBIO + CS* (DP2-SAT*DP1)
        END IF

        VARLQ = 0.D0
        IF (YATE.EQ.1) THEN
          VARLQ = -3.D0*ALPLIQ*DT
        ELSE
          VARLQ = 0.D0
        END IF
        VARLQ = VARLQ + (DP2-DP1-VARPAD)*CLIQ

C TESTS SUR LES ESTIMATIONS DE COEF D EXPONENTIELLES
        IF (YAMEC.EQ.1) THEN
          IF (-VARBIO.GT.EXPMAX) THEN
            WRITE (UMESS,9001) 'CALCCO_LIQU_AD_GAZ_VAPE',
     &        '-VARBIO > EXPMAX  ','A LA MAILLE: ',NOMAIL
            RETCOM = 1
            GO TO 30
          END IF
        END IF
        IF (VARLQ.NE.0.D0) THEN
          IF (VARLQ.GT.EXPMAX) THEN
            WRITE (UMESS,9001) 'CALCCO_LIQU_AD_GAZ_VAPE',
     &        'VARLQ > EXPMAX  ','A LA MAILLE: ',NOMAIL
            RETCOM = 1
            GO TO 30
          END IF
          IF (VARLQ.LT.-EXPMAX) THEN
            WRITE (UMESS,9001) 'CALCCO_LIQU_AD_GAZ_VAPE',
     &        'VARLQ <- EXPMAX  ','A LA MAILLE: ',NOMAIL
            RETCOM = 1
            GO TO 30
          END IF
        END IF
      END IF
C 
C **********************************************************************
C CALCUL DE LA VARIABLE INTERNE PHI A L'INSTANT PLUS

      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
        IF (YAMEC.EQ.1) THEN
            VARIA = DEPSV
            IF (YATE.EQ.1) THEN
              VARIA = VARIA - 3.D0*ALPHA0*DT
            END IF
            VARIA = VARIA + CS* (DP2-SAT*DP1)
            VINTP(ADVICO+VICPHI) = BIOT - PHI0 -
     &                      (BIOT-VINTM(ADVICO+VICPHI)-PHI0)*EXP(-VARIA)
        END IF
      END IF


C **********************************************************************
C CALCUL DE PHI  A L'INSTANT COURANT

      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
        IF (YAMEC.EQ.1) THEN
          PHI = VINTM(ADVICO+VICPHI) + PHI0
        ELSE
          PHI = PHI0
        END IF
      ELSE
        IF (YAMEC.EQ.1) THEN
          PHI = VINTP(ADVICO+VICPHI) + PHI0
          PHIM = VINTM(ADVICO+VICPHI) + PHI0
        ELSE
          PHI = PHI0
        END IF
      END IF

C **********************************************************************
C CALCUL DES AUTRES COEFFICIENTS DEDUITS : DILATATIONS ALPHA
C 

      IF (YATE.EQ.1) THEN
        IF (YAMEC.NE.1) ALPHA0 = 0.D0
        ALP11 = SAT* (BIOT-PHI)*ALPHA0 + ALPLIQ*PHI*SAT
        ALP12 = (BIOT-PHI)* (1.D0-SAT)*ALPHA0 + PHI* (1.D0-SAT)/3.D0/T
        ALP21 = ALP12
        ALP22 = SAT* (BIOT-PHI)*ALPHA0 
      END IF

C **********************************************************************
C RECUPERATION DE LA MASSE VOLUMIQUE A L INSTANT MOINS
C 
        RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110

C **********************************************************************
C  CALCUL DE LA VARIABLE INTERNE PRESSION VAPEUR PUIS 
C  PRESSIONS AIR SEC DISSOUS ET EAU

      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
        PVP = VINTM(ADVICO+VICPVP) + PVP0
        PVPM = VINTM(ADVICO+VICPVP) + PVP0
      ELSE
        VARIA = MAMOLV*(1/R/T-1/KH)* DP2/RHO11M-MAMOLV/R/T*DP1/RHO11M
        IF (YATE.EQ.1) THEN
          VARIA = VARIA + (CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))*
     &             (1.D0/ (T-DT)-1.D0/T)*MAMOLV/R
          VARIA = VARIA + (CP12-CP11)* (LOG(T/ (T-DT))- (DT/T))*MAMOLV/R
        END IF
        PVPM = VINTM(ADVICO+VICPVP) + PVP0
        VARIA = (PVPM)*EXP(VARIA)
        PVP = MAMOLV*(PVPM+(P2-DP2)*R*LOG(T/ (T-DT)))
        PVP = RHO11M*KH-PVP
        PVP = PVP/((RHO11M*KH/VARIA)-MAMOLV*(1+R*LOG(T/ (T-DT))))
        VINTP(ADVICO+VICPVP) = PVP - PVP0
      END IF
      PADP = (P2 - PVP)*R*T/KH
      PADM = ((P2-DP2) - PVPM)*R*(T-DT)/KH
      RHO12 = MAMOLV*PVP/R/T
      RHO21 = MAMOLG* (P2-PVP)/R/T
      RHO22 = MAMOLG* PADP/R/T
      MASRT = MAMOLG/R/T
      
      IF (((P2-PVP).LT.0.D0).OR. (PADP .LT.0.D0)) THEN
        WRITE (UMESS,9001) 'CALCCCO_LIQU_AD_GAZ_VAPE','PAIR <=0  ',
     &    'A LA MAILLE: ',NOMAIL
        RETCOM = 1
        GO TO 30
      END IF
      RHO12M = MAMOLV*PVPM/R/ (T-DT)
      RHO21M = MAMOLG* (P2-DP2-PVPM)/R/ (T-DT)
      RHO22M = MAMOLG* PADM/R/ (T-DT)

C **********************************************************************
C  CALCUL DE LA MASSES VOLUMIQUE DE L'EAU  A L'INSTANT PLUS 
C 
      IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
        VARIA = (DP2-DP1-(PADP-PADM))*CLIQ
        IF (YATE.EQ.1) THEN
          VARIA = VARIA - 3.D0*ALPLIQ*DT
        END IF
        VINTP(ADVIHY+VIHRHO) =
     +                -RHO110 + (VINTM(ADVIHY+VIHRHO)+RHO110)*EXP(VARIA)
      END IF

C **********************************************************************
C  CALCUL DES MASSES VOLUMIQUES  A L'INSTANT COURANT 
C 

      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
        RHO11 = VINTM(ADVIHY+VIHRHO) + RHO110
        RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
      ELSE
        RHO11 = VINTP(ADVIHY+VIHRHO) + RHO110
        RHO11M = VINTM(ADVIHY+VIHRHO) + RHO110
      END IF

C
      IF (YATE.EQ.1) THEN

        UMPRHS = RHO0 - (RHO11+RHO22)*SAT*PHI - 
     &   (RHO12+RHO21)* (1.D0-SAT)*PHI
        IF (UMPRHS.LE.0.D0) THEN
          WRITE (UMESS,9001) 'CALCCO_LIQU_AD_GAZ_VAPE',
     &      ' RHOS(1-PHI) <=0  ',' A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
        C0EPS = UMPRHS*CSIGM + PHI*SAT*(RHO11*CP11+RHO22*CP22) +
     &          PHI* (1.D0-SAT)* (RHO12*CP12+RHO21*CP21)
        IF (YAMEC.EQ.1) THEN
          C0EPS = C0EPS - 9.D0*T*K0*ALPHA0*ALPHA0
        END IF
        IF (C0EPS.LE.0.D0) THEN
          WRITE (UMESS,9001) 'CALCCO_LIQU_AD_GAZ_VAPE','C0EPS <=0  ',
     &      'A LA MAILLE: ',NOMAIL
          RETCOM = 1
          GO TO 30
        END IF
      END IF
C **********************************************************************
C CALCUL DES DERIVEES PARTIELLES DES PRESSIONS UTILISEES PAR LA SUITE
C 
      DP11P1 = 1/((RHO12*R*T/RHO11/KH)-1)
      DP11P2 = (R*T/KH - 1)/((RHO12*R*T/RHO11/KH)-1)
      DP12P1 = 1/(R*T/KH-(RHO11/RHO12))
      DP12P2 = (R*T/KH-1)*DP12P1
      DP21P1 = - DP12P1
      DP21P2 = 1 - DP12P2
CC      DP22P1 = -1- DP11P1
CC      DP22P2 = 1- DP11P2
C 
      IF ((YATE.EQ.1)) THEN
         L = (CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))
C               
         DP11T = (-L*R*RHO12/KH+PADP/T)/((RHO12*R*T/RHO11/KH)-1)
         DP12T = (-L*RHO11+PADP)/T*DP12P1
         DP21T =  - DP12T
CC         DP22T =  - DP11T
      ENDIF
      


C **********************************************************************
C CALCUL ENTHALPIES ET DERIVEES DES ENTHALPIES
C 

      IF ((YATE.EQ.1)) THEN
        IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DSDE(ADCP11+NDIM+1,ADDEP2) = DSDE(ADCP11+NDIM+1,ADDEP2) +
     &                        DP11P2 * (1.D0-3.D0*ALPLIQ*T)/RHO11
          DSDE(ADCP11+NDIM+1,ADDEP1) = DSDE(ADCP11+NDIM+1,ADDEP1) +
     &                        DP11P1 * (1.D0-3.D0*ALPLIQ*T)/RHO11
          DSDE(ADCP11+NDIM+1,ADDETE) = DSDE(ADCP11+NDIM+1,ADDETE) + 
     &                        CP11 + DP11T* (1.D0-3.D0*ALPLIQ*T)/RHO11
          DSDE(ADCP21+NDIM+1,ADDETE) = DSDE(ADCP21+NDIM+1,ADDETE) + CP21
          DSDE(ADCP12+NDIM+1,ADDETE) = DSDE(ADCP12+NDIM+1,ADDETE) + CP12
          DSDE(ADCP22+NDIM+1,ADDETE) = DSDE(ADCP22+NDIM+1,ADDETE) + CP22
        END IF
        IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          CONGEP(ADCP11+NDIM+1) = CONGEP(ADCP11+NDIM+1) +
     &                     DP11P2 *(1.D0-3.D0*ALPLIQ*T)/RHO11*DP2-
     &                     DP11P1 *(1.D0-3.D0*ALPLIQ*T)/RHO11*DP1+
     &                    (DP11T *(1.D0-3.D0*ALPLIQ*T)/RHO11+CP11)*DT
          CONGEP(ADCP12+NDIM+1) = CONGEP(ADCP12+NDIM+1) + CP12*DT
          CONGEP(ADCP21+NDIM+1) = CONGEP(ADCP21+NDIM+1) + CP21*DT
          CONGEP(ADCP22+NDIM+1) = CONGEP(ADCP22+NDIM+1) + CP22*DT
        END IF
      END IF



C **********************************************************************
C  CALCUL DES ENTHALPIES INTERMEDIAIRES H11,H12,H21, H22

      IF ((YATE.EQ.1)) THEN
        IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
          H11 = CONGEM(ADCP11+NDIM+1)
          H12 = CONGEM(ADCP12+NDIM+1)
          H21 = CONGEM(ADCP21+NDIM+1)
          H22 = CONGEM(ADCP22+NDIM+1)
        ELSE
          H11 = CONGEP(ADCP11+NDIM+1)
          H12 = CONGEP(ADCP12+NDIM+1)
          H21 = CONGEP(ADCP21+NDIM+1)
          H22 = CONGEP(ADCP22+NDIM+1)
        END IF
      END IF

C **********************************************************************
C  CALCUL DE SIGMAP

      IF (((YAMEC.EQ.1))) THEN
        IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DSDE(ADCOME+6,ADDEP1) = DSDE(ADCOME+6,ADDEP1) + BIOT*SAT
          DSDE(ADCOME+6,ADDEP2) = DSDE(ADCOME+6,ADDEP2) - BIOT
        END IF
        IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          CONGEP(ADCOME+6) = CONGEP(ADCOME+6) + BIOT*SAT*DP1 - BIOT*DP2
        END IF
      END IF


C **********************************************************************
C  CALCUL DES APPORTS MASSIQUES



      IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
        DSDE(ADCP11,ADDEP1) = DSDE(ADCP11,ADDEP1) +
     &                      RHO11* (DSATP1*PHI+SAT*PHI*CLIQ*DP11P1-
     &                      SAT*SAT* (BIOT-PHI)*CS)
        DSDE(ADCP11,ADDEP2) = DSDE(ADCP11,ADDEP2) +
     &                      RHO11*SAT* (PHI*CLIQ*DP11P2+ (BIOT-PHI)*CS)
        DSDE(ADCP22,ADDEP1) = DSDE(ADCP22,ADDEP1) +
     &                      RHO22* (DSATP1*PHI + 
     &                      SAT*PHI*MAMOLG*DP21P1/RHO22/KH-
     &                      SAT*SAT* (BIOT-PHI)*CS)
        DSDE(ADCP22,ADDEP2) = DSDE(ADCP22,ADDEP2) +
     &                      RHO22*SAT* (PHI*MAMOLG*DP21P2/RHO22/KH+ 
     &                      (BIOT-PHI)*CS)
        DSDE(ADCP12,ADDEP1) = DSDE(ADCP12,ADDEP1) +
     &                      RHO12* (-DSATP1*PHI- (1.D0-SAT)*SAT*
     &                      (BIOT-PHI)*CS) - RHO12*PHI* (1.D0-SAT)/
     &                      PVP*RHO12/RHO11
        DSDE(ADCP12,ADDEP2) = DSDE(ADCP12,ADDEP2) +
     &                      RHO12* ((BIOT-PHI)* (1.D0-SAT)*CS) +
     &                      RHO12*PHI* (1.D0-SAT)/PVP*RHO12/RHO11
        DSDE(ADCP21,ADDEP1) = DSDE(ADCP21,ADDEP1) +
     &                      RHO21* (-DSATP1*PHI- (1.D0-SAT)*SAT*
     &                      (BIOT-PHI)*CS) + MASRT*PHI* (1.D0-SAT)*
     &                      RHO12/RHO11
        DSDE(ADCP21,ADDEP2) = DSDE(ADCP21,ADDEP2) +
     &                      RHO21*((BIOT-PHI)* (1.D0-SAT)*CS) +
     &                      MASRT*(PHI*(1.D0-SAT)*(RHO11-RHO12)/RHO11)
        IF (YAMEC.EQ.1) THEN
          DO 10 I = 1,3
            DSDE(ADCP11,ADDEME+NDIM-1+I) = DSDE(ADCP11,
     &        ADDEME+NDIM-1+I) + RHO11*BIOT*SAT
            DSDE(ADCP22,ADDEME+NDIM-1+I) = DSDE(ADCP22,
     &        ADDEME+NDIM-1+I) + RHO22*BIOT*SAT
            DSDE(ADCP12,ADDEME+NDIM-1+I) = DSDE(ADCP12,
     &        ADDEME+NDIM-1+I) + RHO12*BIOT* (1.D0-SAT)
            DSDE(ADCP21,ADDEME+NDIM-1+I) = DSDE(ADCP21,
     &        ADDEME+NDIM-1+I) + RHO21*BIOT* (1.D0-SAT)
   10     CONTINUE
        END IF
        IF (YATE.EQ.1) THEN
          DSDE(ADCP11,ADDETE) = DSDE(ADCP11,ADDETE) +
     &                         RHO11*(PHI*SAT*CLIQ*DP11T- 3.D0*ALP11)
          DSDE(ADCP22,ADDETE) = DSDE(ADCP22,ADDETE) +RHO22*SAT*
     &                         (PHI*MAMOLG*DP21T/RHO22/KH- 3.D0 *ALP22)
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
        CONGEP(ADCP22) = CONGEM(ADCP22) + PHI*SAT*RHO22* (1.D0+EPSV) -
     &                   PHIM*SATM*RHO22M* (1.D0+EPSVM)
        CONGEP(ADCP12) = CONGEM(ADCP12) +
     &                   PHI* (1.D0-SAT)*RHO12* (1.D0+EPSV) -
     &                   PHIM* (1.D0-SATM)*RHO12M* (1.D0+EPSVM)
        CONGEP(ADCP21) = CONGEM(ADCP21) +
     &                   PHI* (1.D0-SAT)*RHO21* (1.D0+EPSV) -
     &                   PHIM* (1.D0-SATM)*RHO21M* (1.D0+EPSVM)
      END IF





C **********************************************************************
C    CALCUL DE LA CHALEUR REDUITE Q'

      IF (YATE.EQ.1) THEN
        IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
C   TERME SUIVANTE  EST SANS CONTROLE, TOUT LE MONDE Y PASSE
C        DQ/DT
          DSDE(ADCOTE,ADDETE) = DSDE(ADCOTE,ADDETE) + C0EPS
          IF (YAMEC.EQ.1) THEN
              DO 20 I = 1,3
                DSDE(ADCOTE,ADDEME+NDIM-1+I) = DSDE(ADCOTE,
     &            ADDEME+NDIM-1+I) + ALPHA0*YOUNG/ (1.D0-2.D0*NU)*T
   20         CONTINUE
          END IF
          DSDE(ADCOTE,ADDEP1) = DSDE(ADCOTE,ADDEP1) + 3.D0*ALP11*T
          DSDE(ADCOTE,ADDEP2) = DSDE(ADCOTE,ADDEP2) -
     &                          3.D0* (ALP11+ALP12)*T
        END IF

        IF ((OPTION(1:9).EQ.'RAPH_MECA') .OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
C   TERME SUIVANTE  EST SANS CONTROLE, TOUT LE MONDE Y PASSE
          CONGEP(ADCOTE) = CONGEP(ADCOTE) + C0EPS*DT
          IF (YAMEC.EQ.1) THEN
              CONGEP(ADCOTE) = CONGEP(ADCOTE) +
     &                         ALPHA0*YOUNG/ (1.D0-2.D0*NU)* (T-DT/2)*
     &                         DEPSV
          END IF
          CONGEP(ADCOTE) = CONGEP(ADCOTE) + 3.D0*ALP11* (T-DT/2.D0)*DP1
          CONGEP(ADCOTE) = CONGEP(ADCOTE) -
     &                     3.D0* (ALP11* (T-DT/2.D0)+ALP12*
     &                     (T-DT/2.D0))*DP2
        END IF
      END IF
   30 CONTINUE
 9001 FORMAT (A10,2X,A20,2X,A20,2X,A8)

      END
