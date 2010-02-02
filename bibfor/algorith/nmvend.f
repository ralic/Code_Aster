      SUBROUTINE NMVEND(FAMI,KPG,KSP,MATERD,MATERF,NMAT,DT1,EPSM,DEPS,
     &         SIGM,VIM,NDIM,CRIT,DAMMAX,ETATF,P,NP,BETA,NB,ITER,IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/02/2010   AUTEUR SELLENET N.SELLENET 
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
C TOLE CRP_7
C TOLE CRP_21
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       KPG,KSP,NMAT, NP , NB, IER, NDIM
      REAL*8        MATERD(NMAT,2), MATERF(NMAT,2)
      REAL*8        P(NP), BETA(NB), VIM(3), DT1,CRIT(*)
      REAL*8        EPSM(6),DEPS(6),SIGM(6)
      CHARACTER*(*) FAMI
      CHARACTER*7   ETATF(3)
C-----------------------------------------------------------------------
C     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
C     CHABOCHE AVEC ENDOMAGEMENT. INTEGRATION EULER IMPLICITE
C     CAS OU ON SE RAMENE A UNE SEULE EQUATION
C-----------------------------------------------------------------------
C-- ARGUMENTS
C------------
C
C IN   MATE    : PARAMETRE MATERIAU A L'INSTANT T
C      IMATE   : ADRESSE DU MATERIAU CODE
C      NMAT    : DIMENSION DE MATE
C      MATCST  : 'OUI' SI MATERIAU CST ENTRE T- ET T
C                'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
C                'NON' SINON
C      HOOK    : OPERATEUR DE HOOK
C      DT      : INCREMENT DE TEMPS
C      NP      : NOMBRE D'INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
C      NB      : NOMBRE D'INCONNUES ASSOCIEES AUX CONTRAINTES
C      RM      : VARIABLES INTERNES A T-
C      DM      : VARIABLES INTERNES A T-
C      EP      : DEFORMATIONS TOTALES ET THERMIQUE A T ET
C                VISCOPLASTIQUE A T-
C OUT  P       : INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
C      BETA    : INCONNUES ASSOCIEES AUX CONTRAINTES
C      IER     : CODE DE RETOUR D'ERREUR
C                0=OK
C                1=NOOK
C
C INFO P(1)=RPOINT,  P(2)=DFOINT
C-----------------------------------------------------------------------
C TOLE CRP_4
      INTEGER     I,NDT,NDI,NITER,IRET,IRET1,IRET2,IRET3,IT2,ITER
      INTEGER*8   IR
      REAL*8      DAMMAX, EPSI, R8PREM, PREC,PRECR,VAL0,DEVSE(6),SIGE(6)
C
      REAL*8  EPST(6),DEPSTH(6), EPSED(6),E,NU,ALPHAP,ALPHAM,DD,DR
      COMMON /TDIM/   NDT  , NDI
      REAL*8   NMFEND,DKOOH(6,6),HOOKF(6,6),XAP,EPSEF(6),DEPSE(6)
      REAL*8   H1SIGF(6),LCNRTS,SEQ1MD,SEQE,TROISK,TROIKM,SIGMMO
      REAL*8   TP,TM,TREF,X(4),Y(4),NMFEDD,R8MIEM
      EXTERNAL NMFEND,NMFEDD
      COMMON /FVENDO/MU,SYVP,KVP,RM,DM,SEQE,AD,DT,RD,IR,UNSURN,UNSURM
      REAL*8 MU,SYVP,KVP,SEQ,AD,DT,UNSURN,UNSURM,RM,DM,RD,NVP
      REAL*8 EM,NUM,DEVSIG(6),DEPSMO,COEF,SIGPMO,DF,VAL1,DEVSM(6),MUM
      REAL*8 DEVEP(6),VALX,DENO,VALP1,DFDS(6), D2FDS(6,6),TEST
C
C-----------------------------------------------------------------------
C-- 1. INITIALISATIONS
C   ===================
      NITER =  INT(CRIT(1))
      PREC =  CRIT(3)
      IER = 0
      DT=DT1
      IT2=0

      RM=VIM(NB+2)
      DM=VIM(NB+3)
      E =MATERF(1,1)
      NU =MATERF(2,1)
      MU=E/2.D0/(1.D0+NU)
      TROISK = E/(1.D0-2.D0*NU)
      EM =MATERD(1,1)
      NUM =MATERD(2,1)
      TROIKM = EM/(1.D0-2.D0*NUM)
      MUM=EM/2.D0/(1.D0+NUM)

        IF (NDIM.EQ.2) THEN
           SIGM(5)=0.D0
           SIGM(6)=0.D0
           DEPS(5)=0.D0
           DEPS(6)=0.D0
        ENDIF
      ALPHAP=MATERF(3,1)
      ALPHAM=MATERD(3,1)
      SYVP = MATERF(1,2)
      NVP  = MATERF(4,2)
      KVP  = 1.D0/MATERF(6,2)   
      RD   = MATERF(7,2)
      AD   = MATERF(8,2)
      UNSURN=1.D0/NVP
      UNSURM=MATERF(5,2)

      CALL LCDEVI(SIGM,DEVSM)
      CALL LCDEVI(DEPS,DEVEP)
        IF (NDIM.EQ.2) THEN
           DEVSM(5)=0.D0
           DEVSM(6)=0.D0
           DEVEP(5)=0.D0
           DEVEP(6)=0.D0
        ENDIF

      IF (DM.GE.1.D0) DM=DAMMAX
      DO 15 I = 1,6
          EPSEF(I)=DEVSM(I)/(1.D0-DM)/2.D0/MUM+DEVEP(I)
 15   CONTINUE
      CALL LCPRSV(2.D0*MU,EPSEF,DEVSE)

C -- TEMPERATURE

      CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TREF,IRET1)
      CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TM,IRET2)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TP,IRET3)
      IRET=IRET1+IRET2+IRET3
      IF ((IRET.EQ.0).AND.((ALPHAP+ALPHAM).EQ.0.D0))
     &   THEN
        CALL U2MESS('F','CALCULEL_31')
      ELSEIF (((ALPHAP+ALPHAM).EQ.0.D0).OR.(IRET.GE.1)) THEN 
        COEF = 0.D0
      ELSE
        COEF = ALPHAP*(TP-TREF)- ALPHAM*(TM-TREF)
      ENDIF


      SEQE= LCNRTS(DEVSE)
      
      IF (SEQE.GT.SYVP)THEN
      
C RESOLUTION DE L'EQUATION EN DR

         VAL0 = NMFEND(0.D0)
         IF (VAL0.GT.0.D0) THEN
           IER=21
           GOTO 9999
         ENDIF

C        PRECISION RELATIVE DE RESOLUTION : F(X) < PREC         
         PRECR = PREC * ABS(VAL0) 

C        APPROXIMATION INITIALE  DE LA BORNE SUPERIEURE
         XAP = SEQE/MU/3.D0
         
   30    CONTINUE         
C        RECHERCHE DE LA BORNE SUPERIEURE
         VAL1 = NMFEND(XAP)
         IF (ABS(VAL1).LT.PRECR) THEN
            DR=XAP
            GOTO 50
         ELSEIF (VAL1.GT.0.D0) THEN
C           LA SOLUTION EST DANS L INTERVALLE (0,XAP)
            GOTO 21
         ELSE         
C           LA BORNE SUPERIEURE DOIT VERIFIER F(XAP) >0
C           ICI F(XAP) <0. SI F'(XAP) >0, XAP EST A AUGMENTER
            VALP1=NMFEDD(XAP)
            IF (VALP1.GT.0.D0) THEN
               XAP=XAP*10.D0
               IT2=IT2+1
               IF (IT2.GT.NITER) THEN
                  IER=22
                  GOTO 9999
               ENDIF
               GOTO 30  
            ELSE      
C              RECHERCHE DE XAP TEL QUE F(XAP) >0 
C              A FAIRE : UNE VRAIE DICHOTOMIE
               DO 22 I = 1, NITER
                 XAP = XAP/2.D0
                 IF (ABS(XAP).LT.R8MIEM()) THEN
                   DR=0.D0
                   GOTO 50
                 ENDIF
                 VAL1 = NMFEND(XAP)
                 IF (VAL1.GT.0.D0) GOTO 21
  22           CONTINUE
               IER=23
               GOTO 9999
            ENDIF
         ENDIF
         
21       CONTINUE
C        RESOLUTION DE L'EQUATION EN DR PAR METHODE DE CORDES
         X(1) = 0.D0
         X(2) = XAP
         Y(1) = VAL0
         Y(2) = VAL1
         X(3) = X(1)
         Y(3) = Y(1)
         X(4) = X(2)
         Y(4) = Y(2)
C
         IF (ABS(Y(4)).LT.PRECR) THEN
            DR=X(4)
            GOTO 50
         ENDIF
         DO 40 ITER = 1, NITER
           IF (Y(1).GT.0 .OR. Y(2).LT.0) THEN
              CALL U2MESS('A','ALGORITH6_78')
              IER=24
              GOTO 9999
           ENDIF
           IF (X(3).EQ.X(4)) THEN
              CALL U2MESS('A','ALGORITH9_84')
              IER=25
              GOTO 9999
           ENDIF
           CALL ZEROCO(X,Y)
           Y(4) = NMFEND(X(4))
C          DOUBLE CRITERE : F<EPSI ET DR/R < EPSI
           IF (ABS(Y(4)).LT.PRECR) THEN
              IF(ABS(RM+X(4)).GT.R8PREM()) THEN
                 TEST=ABS(X(4)-X(3))/ABS(RM+X(4))
              ELSE
                 TEST=ABS(X(4)-X(3))
              ENDIF
              IF (TEST.LT.PRECR) THEN
                 DR=X(4)
                 GOTO 50
              ENDIF
           ENDIF
  40     CONTINUE
         IER=26
         GOTO 9999

  50     CONTINUE

         SEQ1MD=KVP*((DR/DT)**UNSURN)*((RM+DR)**UNSURM)+SYVP
         DD=DT*(SEQ1MD/AD)**RD
         DF=DM+DD

         IF (DF.GE.DAMMAX) THEN
            DD = 0.D0
            DF = DAMMAX
            DR=0.D0
            ETATF(3)='DAMMAXO'
         ENDIF

         SEQ=(1.D0-DF)*SEQE-3.D0*MU*DR
         DENO=1.D0+3.D0*MU*DR/SEQ
         DO 16 I=1,6
            DEVSIG(I)=(1.D0-DF)*DEVSE(I)/DENO
 16      CONTINUE


      ELSE

         DR=0.D0
         DD=0.D0
         SEQ1MD=SYVP
         DF=DM
         CALL R8INIR(6,0.D0,DEVSIG,1)
      ENDIF

      DEPSMO = 0.D0
      DO 13 I=1,3
        DEPSMO = DEPSMO + DEPS(I) -COEF
 13   CONTINUE
      DEPSMO = DEPSMO/3.D0

      SIGMMO = 0.D0
      DO 17 I =1,3
        SIGMMO = SIGMMO + SIGM(I)
 17   CONTINUE
      SIGMMO = SIGMMO /3.D0
      SIGPMO=(SIGMMO/TROIKM/(1.D0-DM)+DEPSMO)*(1.D0-DF)*TROISK
      DO 18 I=1,3
        BETA(I)=DEVSIG(I)+SIGPMO
 18   CONTINUE
      DO 19 I=4,6
        BETA(I)=DEVSIG(I)
 19   CONTINUE

      P(1)=DR/DT
      P(2)=DD/DT

 9999 CONTINUE
      END
