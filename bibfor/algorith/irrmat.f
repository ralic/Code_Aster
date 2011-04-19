      SUBROUTINE IRRMAT(FAMI,KPG,KSP,MODEL,IMAT,NMAT,ITMAX,RELA,
     &                  MATERD,MATERF,MATCST,NDT,NDI,NR,NVI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE FLEJOU J-L.FLEJOU
      IMPLICIT NONE
      CHARACTER*8   MODEL
      CHARACTER*3   MATCST
      CHARACTER*(*) FAMI
      INTEGER IMAT,NMAT,NDT,NDI,NR,NVI,KPG,KSP,IRET,ITMAX
      REAL*8  MATERD(NMAT,2),MATERF(NMAT,2),RELA

C     ----------------------------------------------------------------
C     IRRAD3M   : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                 NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                MATER(*,1) = E , NU , ALPHA
C     ----------------------------------------------------------------
C     IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C         KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C         IMAT   :  ADRESSE DU MATERIAU CODE
C         MODEL  :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION  DE MATER
C         ITMAX  :  NOMBRE D ITERATION MAX
C         RELA   :  TOLERANCE RELATIVE DES VALEURS MATERIAUX
C         VIND   :  VARIABLES INTERNES A T
C     OUT MATERD :  COEFFICIENTS MATERIAU A T
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C                   MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                   MATER(*,2) = CARACTERISTIQUES   AUTRE
C         MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                   'NON' SINON
C         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C         NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C         NR     :  NB DE COMPOSANTES SYSTEME NL
C         NVI    :  NB DE VARIABLES INTERNES, DANS LE SYSTEME NL
C     ----------------------------------------------------------------
      INTEGER     ITERAT,NBCARA
C     NOMBRE DE PARAMETRES DE LA LOI : NBCARA
      PARAMETER   (NBCARA = 12 )
      INTEGER CERR(NBCARA)
      CHARACTER*8 NOMCIR(NBCARA)
      REAL*8      MAT(NBCARA)
      CHARACTER*8 NOMCEL(3)

      REAL*8      P0,IRRAD,IRRAF,PE,K,A,TEMPD,TEMPF
      REAL*8      R02,EU,RM,AI0,ETAIS,RG0,ALPHA,PHI0,KAPPA,ZETAG,ZETAF
      REAL*8      N0,N1,F0,F1,FE,PASN,EXPH,EXP0,SPE,COEFFA

      REAL*8       VALRM(12)
      INTEGER      VALIM(2)
      CHARACTER*10 VALKM(2)

      DATA PE    /2.0D-3/

      DATA NOMCEL /'E       ','NU      ','ALPHA   '/

      DATA NOMCIR /'R02     ','EPSI_U  ','RM      ','AI0     ',
     &             'ETAI_S  ','RG0     ','ALPHA   ','PHI0    ',
     &             'KAPPA   ','ZETA_F  ','ZETA_G  ','TOLER_ET'/

C     NOM                         a t-                 a t+ (t-+dt)
C     -------------------------------------------------------------
C     E                           MATERD(1,1)          MATERF(1,1)
C     NU                          MATERD(2,1)          MATERF(2,1)
C     ALPHA                       MATERD(3,1)          MATERF(3,1)

C     AI0                         MATERD(4,2)          MATERF(4,2)
C     ETAI_S                      MATERD(5,2)          MATERF(5,2)
C     AG                          MATERD(6,2)          MATERF(6,2)
C     K                           MATERD(7,2)          MATERF(7,2)
C     N                           MATERD(8,2)          MATERF(8,2)
C     P0                          MATERD(9,2)          MATERF(9,2)
C     KAPPA                       MATERD(10,2)         MATERF(10,2)
C     R02                         MATERD(11,2)         MATERF(11,2)
C     ZETAF                       MATERD(12,2)         MATERF(12,2)
C     PENTE EN PE                 MATERD(13,2)         MATERF(13,2)
C     PK                          MATERD(14,2)         MATERF(14,2)
C     PE                          MATERD(15,2)         MATERF(15,2)
C     CONTRAINTE EN PE            MATERD(16,2)         MATERF(16,2)
C     ZETAG                       MATERD(17,2)         MATERF(17,2)

C     IRRADIATION                 MATERD(18,2)         MATERF(18,2)
C     AGINT                       MATERD(19,2)         MATERF(19,2)

C     TOLER SUR SEUIL             MATERD(20,2)         MATERF(20,2)
C     ERREUR SUR SEUIL            MATERD(21,2)         MATERF(21,2)

C     TEMPERATURE                 MATERD(22,2)         MATERF(22,2)

C     INCREMENT IRRADIATION       MATERD(23,2)         MATERF(23,2)
C     INCREMENT TEMPERATURE       MATERD(24,2)         MATERF(24,2)


C -   PROTECTION SUR LA DIMENSION DES TABLEAUX : MATERD MATERF
      CALL ASSERT(NMAT.GE.30)

C -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
      CALL IRRNVI ( MODEL, NDT, NDI, NR, NVI )

C === ================================================
C
C     RECUPERATION MATERIAU A TEMPD ET IRRAD
C
C === ================================================
C     CARACTERISTIQUES ELASTIQUES A TEMP- ET IRRA-
      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','ELAS',0,' ',0.0D0,
     &            3,NOMCEL,MATERD(1,1),CERR, 1)

C     TEMPERATURE A T-
      CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TEMPD,IRET)
C     IRRADIATION A T-
      CALL RCVARC('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
C     CARACTERISTIQUES MATERIAU A TEMP- ET IRRA-
      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','IRRAD3M',0,' ',0.0D0,
     &            NBCARA,NOMCIR,MAT,CERR, 1)

C     POUR PLUS DE CLARETE, JE RENOMME LES GRANDEURS
      IF ( CERR(10) .EQ. 0 ) THEN
         ZETAF = MAT(10)
      ELSE
         ZETAF = 1.0D0
      ENDIF
      IF ( CERR(11) .EQ. 0 ) THEN
         ZETAG = MAT(11)
      ELSE
         ZETAG = 1.0D0
      ENDIF
      R02   = MAT(1)
      EU    = MAT(2)
      RM    = MAT(3)
      AI0   = MAT(4)
      ETAIS = MAT(5)
      RG0   = MAT(6)
      ALPHA = MAT(7)
      PHI0  = MAT(8)
      KAPPA = MAT(9)

C     CALCUL DE LA PUISSANCE PAR DICHOTOMIE
C       - LA FONCTION EST MONOTONE DECROISSANTE
C       - NORMALISATION PAR R02
      COEFFA = RM*EXP(EU)/R02
C     F(n) = 1.0 - RM*EXP(EU)*((PE+n-EU)**n)/((n**n)*R02)
C     Finf = Limite F(n)       Fzero = Limite F(n)
C            n->infini                 n->0+
      N0 = EU - PE
      F1 = 1.0D0 - COEFFA*EXP(-N0)
C     L'équation peut ne pas avoir de solution, pour le vérifier on
C     calcule sa valeur FE à la 1ère borne de recherche +PE/10000.0
C     C'est avec FE que l'on vérifie que l'on à des solutions
      IF ( N0 .GE. 0.0D0 ) THEN
         N1 = N0 + PE/1000.0D0
      ELSE
         N1 = PE/1000.0D0
      ENDIF
      FE = 1.0D0 - COEFFA*((N1-N0)**N1)/(N1**N1)
      IF ( (FE*F1.GT.0.0D0) .OR. (N0.EQ.0.0D0) ) THEN
C        VALEURS PAR DEFAUT
         N1 = EU
C        VALEUR DE K , N
         IF ( N1 .GT. 0.0D0 ) THEN
            MATERD(7,2) = RM*EXP(EU)/(N1**N1)
            MATERD(8,2) = N1
         ELSE
            MATERD(7,2) = RM
            MATERD(8,2) = 0.0D0
         ENDIF
C        VALEUR DE P0
         MATERD(9,2) = 0.0D0
C        -----------------
         K   = MATERD(7,2)
         SPE = K*(PE**N1)
         A   = N1*K*(PE**(N1-1.D0))
      ELSE
         IF ( N0 .GT. 0.0D0 ) THEN
            F0   = 1.0D0
            PASN = N0/10.0D0
            N1   = N0 - (PASN*0.9999D0)
         ELSE
            F0   = 1.0D0 - COEFFA
            PASN = PE/10.0D0
            N1   = - (PASN*0.9999D0)
         ENDIF
         ITERAT = 0
C        WHILE TRUE
10       CONTINUE
            N1 = N1 + PASN
            F1 = 1.0D0 - COEFFA*((N1-N0)**N1)/(N1**N1)
            IF ( ABS(F1) .LE. RELA ) GOTO 12
            ITERAT=ITERAT+1
            IF (ITERAT.GT.ITMAX) THEN
               VALKM(1) = 'PREMIERE'
               VALIM(1) = ITMAX
               VALRM(1) = F0
               VALRM(2) = F1
               VALRM(3) = N1
               VALRM(4) = PASN
               VALRM(5) = RM
               VALRM(6) = EU
               VALRM(7) = R02
               VALRM(8) = RELA
C              VALEURS INITIALES
               VALRM(9)  = EU - PE
               VALRM(10) = 1.0D0 - COEFFA*EXP(PE-EU)
               VALRM(11) = 1.0D0 - COEFFA
               VALRM(12) = FE
               CALL U2MESG('F','COMPOR1_55',1,VALKM,1,VALIM,12,VALRM)
            ENDIF
            IF ( F1*F0 .GT. 0.0D0 ) THEN
               F0 = F1
            ELSE
               N1   = N1 - PASN
               PASN = PASN * 0.5D0
            ENDIF
         GOTO 10
12       CONTINUE
C        VALEUR DE K
         MATERD(7,2) = RM*EXP(EU)/(N1**N1)
C        VALEUR DE N
         MATERD(8,2) = N1
C        VALEUR DE P0
         MATERD(9,2) = N1 - EU
C        ---------------------
         K   = MATERD(7,2)
         P0  = MATERD(9,2)
         SPE = K*((PE+P0)**N1)
         A   = N1*K*((PE+P0)**(N1-1.D0))
      ENDIF
      IF ( A .GT. 0.0D0 ) THEN
C        VALEUR DE LA PENTE EN PE
         MATERD(13,2) = A
C        VALEUR DE PK
         MATERD(14,2) = PE - (SPE - KAPPA*R02)/A
      ELSE
C        VALEUR DE LA PENTE EN PE
         MATERD(13,2) = 0.0D0
C        VALEUR DE PK
         MATERD(14,2) = 0.0D0
      ENDIF
C     VALEUR DE AI0
      MATERD(4,2) = AI0
C     VALEUR DE ETAI_S
      MATERD(5,2) = ETAIS
C     VALEUR DE AG
      EXPH = EXP(ALPHA*(PHI0-IRRAD))
      MATERD(6,2) = RG0/(1.0D0+EXPH)/3.0D0
C     VALEUR DE KAPPA
      MATERD(10,2) = KAPPA
C     VALEUR DE R02
      MATERD(11,2) = R02
C     VALEUR DE ZETAF
      MATERD(12,2) = ZETAF
C     VALEUR DE PE
      MATERD(15,2) = PE
C     VALEUR DE LA CONTRAINTE EN PE
      MATERD(16,2) = SPE
C     VALEUR DE ZETAG
      MATERD(17,2) = ZETAG
C     IRRADIATION
      MATERD(18,2) = IRRAD
C     VALEUR DE AG DEJA INTEGRE
      IF ( ALPHA .GT. 0.0D0 ) THEN
         EXP0 = EXP(ALPHA*PHI0)
         EXPH = EXP(ALPHA*IRRAD)
         MATERD(19,2) = RG0*LOG((EXP0+EXPH)/(1.0D0+EXP0))/(3.0D0*ALPHA)
      ELSE
         MATERD(19,2) = 0.0D0
      ENDIF
C     TOLERENCE ET ERREUR SUR LE FRANCHISSEMENT DU SEUIL
      MATERD(20,2) = MAT(12)
      MATERD(21,2) = 0.0D0
C     TEMPERATURE
      MATERD(22,2) = TEMPD

C === ================================================
C
C     RECUPERATION MATERIAU A TEMPF ET IRRAF
C
C === ================================================
C     CARACTERISTIQUES ELASTIQUES A TEMP+ ET IRRA+
      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','ELAS',0,' ',0.0D0,
     &            3,NOMCEL,MATERF(1,1),CERR, 1)

C     TEMPERATURE A T+
      CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TEMPF,IRET)
C     IRRADIATION A T+
      CALL RCVARC('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)
C     L'IRRADIATION NE PEUT PAS DECROITRE
      IF ( IRRAD .GT. IRRAF*1.00001D0 ) THEN
         VALRM(1) = TEMPD
         VALRM(2) = TEMPF
         CALL U2MESR('I','COMPOR1_57',2,VALRM)
         VALRM(1) = IRRAD
         VALRM(2) = IRRAF
         CALL U2MESR('I','COMPOR1_56',2,VALRM)
      ENDIF
      IF ( IRRAD .GT. IRRAF ) THEN
         IRRAF = IRRAD
      ENDIF
C     CARACTERISTIQUES MATERIAU A TEMP+ ET IRRA+
      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','IRRAD3M',0,' ',0.0D0,
     &            NBCARA,NOMCIR,MAT,CERR, 1)

C     POUR PLUS DE CLARETE
      IF ( CERR(10) .EQ. 0 ) THEN
         ZETAF = MAT(10)
      ELSE
         ZETAF = 1.0D0
      ENDIF
      IF ( CERR(11) .EQ. 0 ) THEN
         ZETAG = MAT(11)
      ELSE
         ZETAG = 1.0D0
      ENDIF
      R02   = MAT(1)
      EU    = MAT(2)
      RM    = MAT(3)
      AI0   = MAT(4)
      ETAIS = MAT(5)
      RG0   = MAT(6)
      ALPHA = MAT(7)
      PHI0  = MAT(8)
      KAPPA = MAT(9)

C     CALCUL DE LA PUISSANCE PAR DICHOTOMIE
C       - LA FONCTION EST MONOTONE DECROISSANTE
C       - NORMALISATION PAR R02
      COEFFA = RM*EXP(EU)/R02
C     F(n) = 1.0 - RM*EXP(EU)*((PE+n-EU)**n)/((n**n)*R02)
C     Finf = Limite F(n)       Fzero = Limite F(n)
C            n->infini                 n->0+
      N0 = EU - PE
      F1 = 1.0D0 - COEFFA*EXP(-N0)
C     L'équation peut ne pas avoir de solution, pour le vérifier on
C     calcule sa valeur FE à la 1ère borne de recherche +PE/1000.0
C     C'est avec FE que l'on vérifie que l'on à des solutions
      IF ( N0 .GE. 0.0D0 ) THEN
         N1 = N0 + PE/1000.0D0
      ELSE
         N1 = PE/1000.0D0
      ENDIF
      FE = 1.0D0 - COEFFA*((N1-N0)**N1)/(N1**N1)
      IF ( (FE*F1.GE.0.0D0) .OR. (N0.EQ.0.0D0) ) THEN
C        VALEURS PAR DEFAUT
         N1 = EU
C        VALEUR DE K , N
         IF ( N1 .GT. 0.0D0 ) THEN
            MATERF(7,2) = RM*EXP(EU)/(N1**N1)
            MATERF(8,2) = N1
         ELSE
            MATERF(7,2) = RM
            MATERF(8,2) = 0.0D0
         ENDIF
C        VALEUR DE P0
         MATERF(9,2) = 0.0D0
C        -----------------
         K   = MATERF(7,2)
         SPE = K*(PE**N1)
         A   = N1*K*(PE**(N1-1.D0))
      ELSE
         IF ( N0 .GT. 0.0D0 ) THEN
            F0   = 1.0D0
            PASN = N0/10.0D0
            N1   = N0 - (PASN*0.9999D0)
         ELSE
            F0   = 1.0D0 - COEFFA
            PASN = PE/10.0D0
            N1   = - (PASN*0.9999D0)
         ENDIF
         ITERAT = 0
C        WHILE TRUE
20       CONTINUE
            N1 = N1 + PASN
            F1 = 1.0D0 - COEFFA*((N1-N0)**N1)/(N1**N1)
            IF ( ABS(F1) .LE. RELA ) GOTO 22
            ITERAT=ITERAT+1
            IF (ITERAT.GT.ITMAX) THEN
               VALKM(1) = 'DEUXIEME'
               VALIM(1) = ITMAX
               VALRM(1) = F0
               VALRM(2) = F1
               VALRM(3) = N1
               VALRM(4) = PASN
               VALRM(5) = RM
               VALRM(6) = EU
               VALRM(7) = R02
               VALRM(8) = RELA
C              VALEURS INITIALES
               VALRM(9)  = EU - PE
               VALRM(10) = 1.0D0 - COEFFA*EXP(PE-EU)
               VALRM(11) = 1.0D0 - COEFFA
               VALRM(12) = FE
               CALL U2MESG('F','COMPOR1_55',1,VALKM,1,VALIM,12,VALRM)
            ENDIF
            IF ( F1*F0 .GT. 0.0D0 ) THEN
               F0 = F1
            ELSE
               N1   = N1 - PASN
               PASN = PASN * 0.5D0
            ENDIF
         GOTO 20
22       CONTINUE
C        VALEUR DE K
         MATERF(7,2) = RM*EXP(EU)/(N1**N1)
C        VALEUR DE N
         MATERF(8,2) = N1
C        VALEUR DE P0
         MATERF(9,2) = N1 - EU
C        ---------------------
         K   = MATERF(7,2)
         P0  = MATERF(9,2)
         SPE = K*((PE+P0)**N1)
         A   = N1*K*((PE+P0)**(N1-1.D0))
      ENDIF
      IF ( A .GT. 0.0D0 ) THEN
C        VALEUR DE LA PENTE EN PE
         MATERF(13,2) = A
C        VALEUR DE PK
         MATERF(14,2) = PE - (SPE - KAPPA*R02)/A
      ELSE
C        VALEUR DE LA PENTE EN PE
         MATERF(13,2) = 0.0D0
C        VALEUR DE PK
         MATERF(14,2) = 0.0D0
      ENDIF
C     VALEUR DE AI0
      MATERF(4,2) = AI0
C     VALEUR DE ETAI_S
      MATERF(5,2) = ETAIS
C     VALEUR DE AG
      EXPH = EXP(ALPHA*(PHI0-IRRAF))
      MATERF(6,2) = RG0/(1.0D0+EXPH)/3.0D0
C     VALEUR DE KAPPA
      MATERF(10,2) = KAPPA
C     VALEUR DE R02
      MATERF(11,2) = R02
C     VALEUR DE ZETAF
      MATERF(12,2) = ZETAF
C     VALEUR DE PE
      MATERF(15,2) = PE
C     VALEUR DE LA CONTRAINTE EN PE
      MATERF(16,2) = SPE
C     VALEUR DE ZETAG
      MATERF(17,2) = ZETAG
C     IRRADIATION
      MATERF(18,2) = IRRAF
C     VALEUR DE AG DEJA INTEGRE
      IF ( ALPHA .GT. 0.0D0 ) THEN
         EXP0 = EXP(ALPHA*PHI0)
         EXPH = EXP(ALPHA*IRRAF)
         MATERF(19,2) = RG0*LOG((EXPH+EXP0)/(1.0D0+EXP0))/(3.0D0*ALPHA)
      ELSE
         MATERF(19,2) = 0.0D0
      ENDIF
C     TOLERENCE ET ERREUR SUR LE FRANCHISSEMENT DU SEUIL
      MATERF(20,2) = MAT(12)
      MATERF(21,2) = 0.0D0
C     TEMPERATURE
      MATERF(22,2) = TEMPF

C     INCREMENT IRRADIATION
      MATERD(23,2) = MATERF(18,2) - MATERD(18,2)
      MATERF(23,2) = MATERD(23,2)
C     INCREMENT TEMPERATURE
      MATERD(24,2) = MATERF(22,2) - MATERD(22,2)
      MATERF(24,2) = MATERD(24,2)

C -   MATERIAU CONSTANT ?
C -   ON NE PEUT PAS SAVOIR A L AVANCE DONC NON
      MATCST = 'NON'
      END
