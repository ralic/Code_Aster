      SUBROUTINE IRRMAT ( FAMI,KPG,KSP,MOD,IMAT,NMAT,ITMAX,RELA,VIND,
     &                 TEMPD,TEMPF,MATERD,MATERF,MATCST,NDT,NDI,NR,NVI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/05/2006   AUTEUR MJBHHPE J.L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      CHARACTER*8   MOD
      CHARACTER*3   MATCST
      CHARACTER*(*) FAMI
      INTEGER IMAT,NMAT,NDT,NDI,NR,NVI,KPG,KSP,IRET,ITMAX
      REAL*8  TEMPD,TEMPF,MATERD(NMAT,2),MATERF(NMAT,2),NC,FC,RELA
      REAL*8  VIND(*)

C     ----------------------------------------------------------------
C     IRRAD3M   : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                 NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                MATER(*,1) = E , NU , ALPHA
C     ----------------------------------------------------------------
C     IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C         KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C         IMAT   :  ADRESSE DU MATERIAU CODE
C         MOD    :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION  DE MATER
C         ITMAX  :  NOMBRE D ITERATION MAX
C         RELA   :  TOLERANCE RELATIVE DES VALEURS MATERIAUX
C         VIND   :  VARIABLES INTERNES A T
C         TEMPD  :  TEMPERATURE  A T
C         TEMPF  :  TEMPERATURE  A T+DT
C     OUT MATERD :  COEFFICIENTS MATERIAU A T
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C                   MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                   MATER(*,2) = CARACTERISTIQUES   AUTRE
C         MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                   'NON' SINON
C         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C         NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C         NR     :  NB DE COMPOSANTES SYSTEME NL
C         NVI    :  NB DE VARIABLES INTERNES
C     ----------------------------------------------------------------
      INTEGER     I,ITERAT
      CHARACTER*2 CERR(13)
      CHARACTER*8 NOMC(13),VALM(2)
      REAL*8      MAT(10),P0,PAS,IRRAD,IRRAF,PE,K,A,B,IRRAT,TEMPT

      REAL*8      R02,EU,RM,AI0,ETAIS,RR,ALPHA,PHI0,KAPPA,ZETA
      REAL*8      N0,N1,F0,F1,PASN,EXPPH,SPE,VALR(2)

      DATA PE    /2.0D-3/


C     NOM               a t-           a t-+dt/2      a t+ (t-+dt)
C     -------------------------------------------------------------
C     E                 MATERD(1,1)                   MATERF(1,1)
C     NU                MATERD(2,1)                   MATERF(2,1)
C     ALPHA             MATERD(3,1)                   MATERF(3,1)

C     AI0               MATERD(4,2)                   MATERF(4,2)
C     ETAI_S            MATERD(5,2)                   MATERF(5,2)
C     AG                MATERD(6,2)                   MATERF(6,2)
C     K                 MATERD(7,2)                   MATERF(7,2)
C     N                 MATERD(8,2)                   MATERF(8,2)
C     P0                MATERD(9,2)                   MATERF(9,2)
C     KAPPA             MATERD(10,2)                  MATERF(10,2)
C     R02               MATERD(11,2)                  MATERF(11,2)
C     ZETA              MATERD(12,2)                  MATERF(12,2)
C     PENTE EN PE       MATERD(13,2)                  MATERF(13,2)
C     PK                MATERD(14,2)                  MATERF(14,2)
C     PE                MATERD(15,2)                  MATERF(15,2)
C     CONTRAINTE EN PE  MATERD(16,2)                  MATERF(16,2)

C     AG                               MATERF(17,2)
C     ZETA                             MATERF(18,2)


C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
      CALL IRRNVI ( MOD , NDT , NDI, NR )
      NVI = 5

C =================================================================
C --- DEFINITION DES CHAMPS ---------------------------------------
C =================================================================
      NOMC(1)  = 'E       '
      NOMC(2)  = 'NU      '
      NOMC(3)  = 'ALPHA   '
      NOMC(4)  = 'R02     '
      NOMC(5)  = 'EPSI_U  '
      NOMC(6)  = 'RM      '
      NOMC(7)  = 'AI0     '
      NOMC(8)  = 'ETAI_S  '
      NOMC(9)  = 'R       '
      NOMC(10) = 'ALPHA   '
      NOMC(11) = 'PHI0    '
      NOMC(12) = 'KAPPA   '
      NOMC(13) = 'ZETA    '

C === ================================================  
C
C     RECUPERATION MATERIAU A TEMPD ET IRRAD
C
C === ================================================  
C     CARACTERISTIQUES ELASTIQUES A TEMP- ET IRRA-   
      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','ELAS',1,'TEMP',TEMPD,
     +            3,NOMC,MATERD(1,1),CERR(1), 'FM' )

C     IRRADIATION A T-
      CALL RCVARC('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
C     CARACTERISTIQUES MATERIAU A TEMP- ET IRRA-
      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','IRRAD3M',1,'TEMP',TEMPD,
     +            10,NOMC(4),MAT,CERR(4), 'FM' )

C     POUR PLUS DE CLARETE, JE RENOMME LES GRANDEURS
      IF ( CERR(13) .EQ. 'OK' ) THEN
         ZETA = MAT(10)
      ELSE
         ZETA = 1.0D0
      ENDIF
      R02   = MAT(1)
      EU    = MAT(2)
      RM    = MAT(3)
      AI0   = MAT(4)
      ETAIS = MAT(5)
      RR    = MAT(6)
      ALPHA = MAT(7)
      PHI0  = MAT(8)
      KAPPA = MAT(9)

C     CALCUL DE LA PUISSANCE PAR DICHOTOMIE
C       - LA FONCTION EST MONOTONE DECROISSANTE
C       - NORMALISATION PAR R02
C     F(n) = 1.0 - RM*EXP(EU)*((PE+n-EU)**n)/((n**n)*R02)
C     F1 = Limite F(n)       F0 = Limite F(n)
C          n->infini              n->0+
      N0 = EU - PE
      F1 = 1.0D0 - RM*EXP(EU)*EXP(PE-EU)/R02
      F0 = 1.0D0 - RM*EXP(EU)/R02
      IF ( ((N0.GT.0.0D0).AND.(F1.GE.0.0D0)) .OR.
     &     ((N0.LT.0.0D0).AND.(F0*F1.GE.0.0D0)) ) THEN
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
         ELSE
            N0   = 0.0D0
            PASN = PE/10.0D0
         ENDIF
         ITERAT = 0
         N1     = N0
C        WHILE TRUE
10       CONTINUE
            N1 = N1 + PASN
            F1 = 1.0D0 - RM*EXP(EU)*((PE+N1-EU)**N1)/((N1**N1)*R02)
            IF ( ABS(F1) .LE. RELA ) GOTO 12
            ITERAT=ITERAT+1
            IF (ITERAT.GT.ITMAX) THEN
               CALL UTMESS ('F','IRRMAT','LA DICHOTOMIE POUR IRRAD3M'//
     &            ' N A PAS TROUVE DE SOLUTION POUR LE NOMBRE'//
     &            ' D ITERATION DONNE.')
            ENDIF
            IF ( F1*F0 .GT. 0.0D0 ) THEN
               F0 = F1
               N0 = N1
            ELSE
               PASN = PASN * 0.5D0
               N1 = N0
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
      EXPPH = EXP(ALPHA*(PHI0-IRRAD))
      MATERD(6,2) = RR*(1.0D0-EXPPH/(1.0D0+EXPPH))/3.0D0
C     VALEUR DE KAPPA
      MATERD(10,2) = KAPPA
C     VALEUR DE R02
      MATERD(11,2) = R02
C     VALEUR DE ZETA
      MATERD(12,2) = ZETA
C     VALEUR DE PE
      MATERD(15,2) = PE
C     VALEUR DE LA CONTRAINTE EN PE
      MATERD(16,2) = SPE


C === ================================================  
C
C     RECUPERATION MATERIAU A TEMPF ET IRRAF
C
C === ================================================  
C     CARACTERISTIQUES ELASTIQUES A TEMP+ ET IRRA+
      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','ELAS',1,'TEMP',TEMPF,
     +            3,NOMC(1),MATERF(1,1),CERR(1), 'FM' )

C     IRRADIATION A T+
      CALL RCVARC('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)
C     CARACTERISTIQUES MATERIAU A TEMP+ ET IRRA+
      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','IRRAD3M',1,'TEMP',TEMPF,
     +            10,NOMC(4),MAT,CERR(4), 'FM' )

C     POUR PLUS DE CLARETE
      IF ( CERR(13) .EQ. 'OK' ) THEN
         ZETA = MAT(10)
      ELSE
         ZETA = 1.0D0
      ENDIF
      R02   = MAT(1)
      EU    = MAT(2)
      RM    = MAT(3)
      AI0   = MAT(4)
      ETAIS = MAT(5)
      RR    = MAT(6)
      ALPHA = MAT(7)
      PHI0  = MAT(8)
      KAPPA = MAT(9)

C     CALCUL DE LA PUISSANCE PAR DICHOTOMIE
C       - LA FONCTION EST MONOTONE DECROISSANTE
C       - NORMALISATION PAR R02
C     F(n) = 1.0 - RM*EXP(EU)*((PE+n-EU)**n)/((n**n)*R02)
C     F1 = Limite F(n)       F0 = Limite F(n)
C          n->infini              n->0+
      N0 = EU - PE
      F1 = 1.0D0 - RM*EXP(EU)*EXP(PE-EU)/R02
      F0 = 1.0D0 - RM*EXP(EU)/R02
      IF ( ((N0.GT.0.0D0).AND.(F1.GT.0.0D0)) .OR.
     &     ((N0.LT.0.0D0).AND.(F0*F1.GE.0.0D0)) ) THEN
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
         ELSE
            N0   = 0.0D0
            PASN = PE/10.0D0
         ENDIF
         ITERAT = 0
         N1     = N0
C        WHILE TRUE
20       CONTINUE
            N1 = N1 + PASN
            F1 = 1.0D0 - RM*EXP(EU)*((PE+N1-EU)**N1)/((N1**N1)*R02)
            IF ( ABS(F1) .LE. RELA ) GOTO 22
            ITERAT=ITERAT+1
            IF (ITERAT.GT.ITMAX) THEN
               CALL UTMESS ('F','IRRMAT','LA DICHOTOMIE POUR IRRAD3M'//
     &            ' N A PAS TROUVE DE SOLUTION POUR LE NOMBRE'//
     &            ' D ITERATION DONNE.')
            ENDIF
            IF ( F1*F0 .GT. 0.0D0 ) THEN
               F0 = F1
               N0 = N1
            ELSE
               PASN = PASN * 0.5D0
               N1 = N0
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
      EXPPH = EXP(ALPHA*(PHI0-IRRAF))
      MATERF(6,2) = RR*(1.0D0-EXPPH/(1.0D0+EXPPH))/3.0D0
C     VALEUR DE KAPPA
      MATERF(10,2) = KAPPA
C     VALEUR DE R02
      MATERF(11,2) = R02
C     VALEUR DE ZETA
      MATERF(12,2) = ZETA
C     VALEUR DE PE
      MATERF(15,2) = PE
C     VALEUR DE LA CONTRAINTE EN PE
      MATERF(16,2) = SPE


C     AG   DEPEND :
C           DE L'IRRADIATION
C           DE LA TEMPERATURE
C     ZETA DEPEND
C           DE LA TEMPERATURE
      NOMC(1) = 'R       '
      NOMC(2) = 'ZETA    '
      NOMC(3) = 'ALPHA   '
      NOMC(4) = 'PHI0    '
      VALM(1) = 'TEMP'
      VALM(2) = 'IRRA'
      VALR(1) = (TEMPF + TEMPD)*0.5D0
      VALR(2) = (IRRAF + IRRAD)*0.5D0
      CALL RCVALA(IMAT,' ','IRRAD3M',2,VALM(1),VALR(1),
     +            4,NOMC(1),MAT(1),CERR(1), 'FM' )

      EXPPH = EXP(MAT(3)*(MAT(4)-VALR(2)))
      MATERF(17,2) = MAT(1)*(1.0D0-EXPPH/(1.0D0+EXPPH))/3.0D0
      MATERF(18,2) = MAT(2)

C -   MATERIAU CONSTANT ?
C -   ON NE PEUT PAS SAVOIR A L AVANCE DONC NON
      MATCST = 'NON'

      END
