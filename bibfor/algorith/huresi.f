      SUBROUTINE HURESI(MOD,NMAT,MATER,INDI,DEPS,NR,YD,YF,
     &                  NVI,VIND,R,IRET) 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/04/2013   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C TOLE CRS_1404
      IMPLICIT NONE
C     ----------------------------------------------------------------
C     CALCUL DU VECTEUR RESIDU DU SYSTEME NL
C     ----------------------------------------------------------------
C     IN   MOD    :  TYPE DE MODELISATION
C          NMAT   :  DIMENSION TABLEAU DONNEES MATERIAU
C          MATER  :  DONNEES MATERIAU
C          INDI   :  MECANISMES POTENTIELLEMENT ACTIFS 
C          DEPS   :  INCREMENT DEFORMATION
C          YD     :  VECTEUR SOLUTION A T
C          YF     :  VECTEUR SOLUTION A T+DT?
C          VIND   :  VARIABLES INTERNES A T
C          NVI    :  NOMBRE DE VARIABLES INTERNES
C          NR     :  DIMENSION MAXIMALE DE YD 
C     OUT  R      :  VECTEUR RESIDU DU SYSTEME NL A RESOUDRE
C          IRET   :  CODE RETOUR (>0 -> PB)
C     ----------------------------------------------------------------
      CHARACTER*8   MOD
      REAL*8        MATER(NMAT,2),DEPS(6),YD(NR),YF(NR),VIND(NVI),R(*)
      INTEGER       INDI(7),NR,NVI,IRET,NMAT      
C
      INTEGER       NDT,NDI,I,NBMECA,NBMECT,K,J,KK
      REAL*8        LE(6),CCOND,LEVP,LR(4),PREF,LF(7),HOOKNL(6,6)
      REAL*8        DEPSE(6),CDE(6),COEF0,I1F,N,HOOK(6,6)
      REAL*8        BETA,D,B,PHI,ANGDIL,PCO,ACYC,AMON,CCYC,CMON
      REAL*8        M,MDIL,COEF,PTRAC,ZERO,DEGR,TROIS
      REAL*8        E,NU,AL,DEMU,LA,UN,E1,E2,E3,NU12,NU23,NU13,DEUX
      REAL*8        G1,G2,G3,NU21,NU31,NU32,DENOM,DEPSP(6),DLAMBD(7)
      REAL*8        PSI(42),SIGF(6),SIGDC(9),AD(7),KSI(7),Q(7),P(7)
      REAL*8        RC(7),DPSIDS(6,6),SIGD(3),TH(2),PROD,TOLE1
      REAL*8        YFT(NR),YDT(NR),TRACE,MUL,PS,PK,RTRAC,DPSI
      REAL*8        EPSVP,PC,MATERT(22,2)
      LOGICAL       PROX(4),PROXC(4)
C
      PARAMETER    (NDT   = 6                 )
      PARAMETER    (NDI   = 3                 )         
      PARAMETER    (ZERO  = 0.D0              )
      PARAMETER    (UN    = 1.D0              )
      PARAMETER    (DEUX  = 2.D0              )
      PARAMETER    (TROIS = 3.D0              )
      PARAMETER    (DEGR  = 0.0174532925199D0 )
      PARAMETER    (TOLE1 = 1.D-6             )
C     ----------------------------------------------------------------
C --- REDIMENSIONNEMENT DE YD ET YF POUR S'ADAPTER A HUJJID
C --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
      CALL LCEQVN(NR, YD, YDT)
      CALL LCEQVN(NR, YF, YFT)

      DO 5 I = 1, 22
        MATERT(I,1) = MATER(I,1)
        MATERT(I,2) = MATER(I,2)        
  5   CONTINUE      

      DO 10 I = 1, 6
        YDT(I) = YD(I)*MATER(1,1)
        YFT(I) = YF(I)*MATER(1,1)
  10  CONTINUE      

      NBMECA = 0
      NBMECT = 0
      DO 20 K = 1, 7
        IF (INDI(K) .GT. 0) THEN
          NBMECT = NBMECT + 1
          IF (INDI(K).LE.8) NBMECA = NBMECA + 1
        ENDIF
        DLAMBD(K) = ZERO
        AD(K)     = ZERO
        KSI(K)    = ZERO
        Q(K)      = ZERO
        P(K)      = ZERO
  20  CONTINUE

      DO 30 I = 1, NBMECA
        YDT(NDT+1+I) = YD(NDT+1+I)*MATER(1,1)/ABS(MATER(8,2))
        YFT(NDT+1+I) = YF(NDT+1+I)*MATER(1,1)/ABS(MATER(8,2))
  30  CONTINUE
C ====================================================================
C --- PROPRIETES HUJEUX MATERIAU -------------------------------------
C ====================================================================
      N      = MATER(1,2)
      BETA   = MATER(2,2)
      D      = MATER(3,2)
      B      = MATER(4,2)
      PHI    = MATER(5,2)
      ANGDIL = MATER(6,2)
      PCO    = MATER(7,2)
      PREF   = MATER(8,2)
      ACYC   = MATER(9,2)
      AMON   = MATER(10,2)
      CCYC   = DEUX*MATER(11,2)
      CMON   = MATER(12,2)
      M      = SIN(DEGR*PHI)
      MDIL   = SIN(DEGR*ANGDIL)
      COEF   = MATER(20,2)
      PTRAC  = MATER(21,2)
      RTRAC  = ABS(PREF*1.D-6)
      
C ====================================================================
C --- OPERATEURS ELASTICITE LINEAIRES---------------------------------
C ====================================================================
      CALL LCINMA (ZERO, HOOK)

      IF (MOD(1:2) .EQ. '3D'     .OR.
     &    MOD(1:6) .EQ. 'D_PLAN' )  THEN

        IF (MATER(17,1).EQ.UN) THEN

          E    = MATER(1,1)
          NU   = MATER(2,1)
          AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
          DEMU = E     /(UN+NU)
          LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

          DO 40 I = 1, NDI
            DO 40 J = 1, NDI
              IF (I.EQ.J) HOOK(I,J) = AL
              IF (I.NE.J) HOOK(I,J) = LA
  40      CONTINUE
          DO 50 I = NDI+1, NDT
            HOOK(I,I) = DEMU
  50      CONTINUE

        ELSEIF (MATER(17,1).EQ.DEUX) THEN

          E1   = MATER(1,1)
          E2   = MATER(2,1)
          E3   = MATER(3,1)
          NU12 = MATER(4,1)
          NU13 = MATER(5,1)
          NU23 = MATER(6,1)
          G1   = MATER(7,1)
          G2   = MATER(8,1)
          G3   = MATER(9,1)
          NU21 = MATER(13,1)
          NU31 = MATER(14,1)
          NU32 = MATER(15,1)
          DENOM= MATER(16,1)

          HOOK(1,1) = (UN - NU23*NU32)*E1/DENOM
          HOOK(1,2) = (NU21 + NU31*NU23)*E1/DENOM
          HOOK(1,3) = (NU31 + NU21*NU32)*E1/DENOM
          HOOK(2,2) = (UN - NU13*NU31)*E2/DENOM
          HOOK(2,3) = (NU32 + NU31*NU12)*E2/DENOM
          HOOK(3,3) = (UN - NU21*NU12)*E3/DENOM
          HOOK(2,1) = HOOK(1,2)
          HOOK(3,1) = HOOK(1,3)
          HOOK(3,2) = HOOK(2,3)
          HOOK(4,4) = G1
          HOOK(5,5) = G2
          HOOK(6,6) = G3

        ELSE
          CALL U2MESS('F', 'COMPOR1_38')
        ENDIF
      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN
        CALL U2MESS('F', 'COMPOR1_4')
      ENDIF
C ====================================================================
C --- OPERATEUR ELASTICITE NON LINEAIRE ------------------------------
C ====================================================================
      I1F  = TRACE(NDI,YFT)/TROIS
      IF ( (I1F/PREF) .LT. TOLE1 ) I1F = TOLE1*PREF

      COEF0 = (I1F/PREF) ** N
      DO 60 I = 1, NDT
        DO 60 J = 1, NDT
          HOOKNL(I,J) = COEF0*HOOK(I,J)
  60  CONTINUE
C ====================================================================
C --- AUTRES GRANDEURS UTILES ----------------------------------------
C ====================================================================
      DO 70 I = 1, 4
        PROX(I)  = .FALSE.
        PROXC(I) = .FALSE.
  70  CONTINUE

      DO 80 I = 1, NDT
        SIGF(I) = YFT(I)
        PSI(I) = ZERO
        PSI(NDT+I)  = ZERO
        PSI(2*NDT+I) = ZERO
        PSI(3*NDT+I) = ZERO
        PSI(4*NDT+I) = ZERO
        PSI(5*NDT+I) = ZERO
        PSI(6*NDT+I) = ZERO
  80  CONTINUE

      DO 90 I = 1, 9
        SIGDC(I)=ZERO
  90  CONTINUE

      DO 100 K = 1, NBMECT
        KK = INDI(K)

C        IF(YFT(NDT+1+NBMECA+K).GT.ZERO)THEN
          DLAMBD(K) = YFT(NDT+1+NBMECA+K)
C        ENDIF

        IF (KK.LE.8) RC(K) = YFT(NDT+1+K)

        CALL HUJDDD('PSI   ', INDI(K), MATERT, INDI, YFT, VIND,
     &              PSI((K-1)*NDT+1), DPSIDS, IRET)
        IF (IRET.EQ.1) GOTO 998

        IF (INDI(K) .LT. 4) THEN

          CALL HUJPRJ (INDI(K), SIGF, SIGD, P(K), Q(K))
          IF (P(K) .GE. PTRAC) GOTO 997
          CALL HUJKSI('KSI   ', MATERT, RC(K), KSI(K), IRET)
          IF (IRET.EQ.1) GOTO 998
          AD(K)  = ACYC+KSI(K)*(AMON-ACYC)

        ELSEIF (INDI(K) .EQ. 4) THEN

          KSI(K) = UN
          P(K)   = TRACE(NDI,YFT)/TROIS

        ELSEIF ((INDI(K) .LT. 8) .AND. (INDI(K) .GT. 4)) THEN

          CALL HUJPRC (K, INDI(K)-4, SIGF, VIND, MATERT, YFT,
     &                 P(K), Q(K), SIGDC(3*K-2))
          IF (P(K) .GE. PTRAC) GOTO 997
          CALL HUJKSI('KSI   ', MATERT, RC(K), KSI(K), IRET)
          IF(IRET.EQ.1) GOTO 998
          AD(K) = DEUX*(ACYC+KSI(K)*(AMON-ACYC))

          TH(1) = VIND(4*INDI(K)-9)
          TH(2) = VIND(4*INDI(K)-8)
          PROD  = SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)/DEUX

          IF ((-Q(K)/PREF.LT.TOLE1).OR.((UN+PROD/Q(K)).LT.TOLE1)) THEN
             KK = KK - 4
             CALL HUJPXD(INDI(K),MATERT,SIGF,VIND,PROX(KK),PROXC(KK))
          ELSE
            AD(K) = (ACYC+KSI(K)*(AMON-ACYC))*(UN+PROD/Q(K))
          ENDIF

        ELSEIF (INDI(K) .EQ. 8) THEN

          KSI(K) = UN
          CALL HUJPIC(K, INDI(K),SIGF, VIND, MATERT, YFT, P(K))

        ELSEIF ((INDI(K).GT.8).AND.(INDI(K).LT.12)) THEN
          GOTO 100

        ELSE
          CALL U2MESS('F', 'COMPOR1_8')
        ENDIF

 100  CONTINUE

      EPSVP = YFT(NDT+1)
      PC    = PCO*EXP(-BETA*EPSVP)
      CMON = CMON * PC/PREF
      CCYC = CCYC * PC/PREF

C --- CONDITIONNEMENT DE LA MATRICE JACOBIENNE
      CCOND= MATER(1,1)
C ====================================================================
C ---- CALCUL DE CDE = C*DEPSE ---------------------------------------
C ====================================================================
      DO 110 I = 1,NDT
        DEPSP(I) = ZERO
 110  CONTINUE

      DO 120 K = 1, NBMECT
        KK = (K-1)*NDT
        DO 130 I = 1, NDT
          DEPSP(I) = DEPSP(I) + DLAMBD(K)*PSI(KK+I)
 130     CONTINUE
 120   CONTINUE

      DO 140 I = 1, NDT
         DEPSE(I) = DEPS(I) - DEPSP(I)
 140  CONTINUE

       CALL LCPRMV (HOOKNL, DEPSE, CDE)
C ====================================================================
C --- CALCUL DE LE (6) -----------------------------------------------
C ====================================================================
       DO 150 I = 1, NDT
         LE(I) = YFT(I) - YDT(I) - CDE(I)
 150   CONTINUE


C ====================================================================
C --- CALCUL DE LEVP (1X1) -------------------------------------------
C ====================================================================
        LEVP = YFT(NDT+1) - YDT(NDT+1)

        IF(NBMECA.EQ.0)GOTO 190

        DO 160 K = 1, NBMECT

          KK = INDI(K)
          PK =P(K) -PTRAC

          IF (KK .LT. 4) THEN

            IF ((P(K)/PREF).GT.TOLE1) THEN
              DPSI =MDIL+Q(K)/P(K)
            ELSE
              DPSI =MDIL+Q(K)
            ENDIF
            LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*DPSI

          ELSEIF (KK .EQ. 4) THEN

            LEVP = LEVP + DLAMBD(K)

          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN

            CALL HUJPRJ(KK-4,SIGF,SIGD,COEF0,MUL)
            PS = 2.D0*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3)

            IF ((P(K)/PREF).GT.TOLE1) THEN
              IF ((-Q(K)/PREF).GT.TOLE1) THEN
                DPSI =MDIL+PS/2.D0/P(K)/Q(K)
              ELSE
                DPSI =MDIL
              ENDIF
            ELSE
              IF ((-Q(K)/PREF).GT.TOLE1) THEN
                DPSI = MDIL+PS/2.D-6/PREF/Q(K)
              ELSE
                DPSI = MDIL
              ENDIF
            ENDIF

            LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*DPSI

          ELSEIF (KK .EQ. 8) THEN

            IF(VIND(22).GT.ZERO)THEN
              LEVP = LEVP - DLAMBD(K)
            ELSE
              LEVP = LEVP + DLAMBD(K)
            ENDIF

          ENDIF

 160    CONTINUE

C ====================================================================
C --- CALCUL DE LR (NBMECX1) -----------------------------------------
C ====================================================================
        DO 170 K = 1, 4
          LR(K) = ZERO
 170    CONTINUE

        IF (NBMECA.EQ.0) GOTO 190
        DO 180 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LR(K) = YFT(NDT+1+K) - YDT(NDT+1+K) -
     &              DLAMBD(K)/AD(K)*(UN-RC(K))**DEUX
          ELSEIF (KK .EQ. 4) THEN
            LR(K) = YFT(NDT+1+K) - YDT(NDT+1+K) -
     &              DLAMBD(K)/CMON*(UN-RC(K))**DEUX

          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
            TH(1) = VIND(4*INDI(K)-9)
            TH(2) = VIND(4*INDI(K)-8)
            PROD  = SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)/DEUX

            IF((-Q(K)/PREF.LT.TOLE1).OR.((UN+PROD/Q(K)).LT.TOLE1))THEN
              AD(K) = (ACYC+KSI(K)*(AMON-ACYC))
            ELSE
              AD(K) = (ACYC+KSI(K)*(AMON-ACYC))*(UN+PROD/Q(K))
            ENDIF
            LR(K) = YFT(NDT+1+K) - YDT(NDT+1+K) -
     &              DLAMBD(K)/AD(K)*(UN-RC(K))**DEUX
          ELSEIF (KK .EQ. 8) THEN
            LR(K) = YFT(NDT+1+K) - YDT(NDT+1+K) -
     &              DLAMBD(K)/CCYC*(UN-RC(K))**DEUX

          ENDIF
 180    CONTINUE

 190    CONTINUE

C ====================================================================
C --- CALCUL DE LF (NBMECX1) -----------------------------------------
C ====================================================================
        DO 200 K = 1, 7
          LF(K) = ZERO
 200    CONTINUE

        DO 210 K = 1, NBMECT
          KK = INDI(K)
          PK =P(K) -PTRAC
          IF (KK .LT. 4) THEN
            LF(K) = Q(K) + M*PK*RC(K)*( UN-B*LOG(PK/PC) )
          ELSEIF (KK .EQ. 4) THEN
            LF(K) = ABS(P(K)) + RC(K)*D*PC
          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
            LF(K) = Q(K) + M*PK*RC(K)*( UN-B*LOG(PK/PC) )
          ELSEIF (KK .EQ. 8) THEN
            LF(K) = ABS(P(K)) + RC(K)*D*PC
          ELSEIF (KK .GT. 8) THEN
            CALL HUJPRJ(KK-8,YFT,SIGD,PK,PS)
            LF(K) = PK + DEUX*RTRAC - PTRAC
          ENDIF
 210    CONTINUE

C ====================================================================
C --- ASSEMBLAGE DE R ------------------------------------------------
C ====================================================================
      DO 220 I = 1, NDT
        R(I)  = -LE(I) /CCOND
 220   CONTINUE

      R(NDT+1) = -LEVP

      IF (NBMECA.EQ.0) GOTO 240

      DO 230 K = 1, NBMECA
        R(NDT+1+K)        = -LR(K) /CCOND*ABS(PREF)
        R(NDT+1+NBMECA+K) = -LF(K) /CCOND
 230  CONTINUE

 240  CONTINUE

      IF(NBMECA.LT.NBMECT)THEN
        DO 250 K = 1, NBMECT
         IF(INDI(K).GT.8)THEN
            R(NDT+1+NBMECA+K) = -LF(K)/CCOND
          ENDIF
 250    CONTINUE
      ENDIF

      DO 260 I = NDT+NBMECA+NBMECT+2, 18
        R(I) = ZERO
 260  CONTINUE
  
      GOTO 999
C ====================================================================
C --- TRAITEMENT DES CAS OU IRET DIFFERENT DE ZERO -------------------
C ====================================================================
997   CONTINUE
      IRET = 3

998   CONTINUE

      DO 270 I = 1, 3
        IF(PROX(I))THEN
          VIND(I+4)    = MATER(18,2)
          VIND(23+I)   = UN
          VIND(27+I)   = ZERO
          VIND(4*I+5)  = ZERO
          VIND(4*I+6)  = ZERO
          VIND(4*I+7)  = ZERO
          VIND(4*I+8)  = ZERO
          VIND(5*I+31) = ZERO
          VIND(5*I+32) = ZERO
          VIND(5*I+33) = ZERO
          VIND(5*I+34) = ZERO
          VIND(5*I+35) = MATER(18,2)
          IRET = 2
        ELSEIF(PROXC(I))THEN
          VIND(27+I)   = ZERO
          IRET = 2
       ENDIF
 270  CONTINUE

999   CONTINUE

      END      
