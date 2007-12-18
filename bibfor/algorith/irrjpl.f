      SUBROUTINE  IRRJPL(FAMI,KPG,KSP,MOD,NMAT,MATER,NR,NVI,SIGF,VIND,
     &                   VINF,SIGD,DSDE)

      IMPLICIT NONE
      CHARACTER*(*)     FAMI
      CHARACTER*8       MOD
      INTEGER           NMAT,KPG,KSP,NVI,NR
      REAL*8            MATER(NMAT,2),DSDE(6,6),SIGF(6),VIND(*)
      REAL*8            VINF(*),SIGD(6)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/12/2007   AUTEUR FLEJOU J-L.FLEJOU 
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
C RESPONSABLE FLEJOU J-L.FLEJOU
C       ----------------------------------------------------------------
C       IRRAD3M   :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C                     ELASTO_PLASTIQUE EN VITESSE A T OU T+DT
C       ----------------------------------------------------------------
C       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           NR     :  TAILLE DE LA MATRICE JACOBIENNE
C           SIGF   :  CONTRAINTES A T+DT
C           VIND   :  VARIABLES INTERNES A T
C           VINF   :  VARIABLES INTERNES A T+DT
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
      INTEGER NDT,NDI
C     ------------------------------------------------------------------
      COMMON /TDIM/ NDT,NDI

      REAL*8  YD(NR),YF(NR),DY(NR),DET,MAT(6,6),I4(6,6)
      REAL*8  FKOOH(6,6),IRRAD,IRRAF,DPHI,PF,ETAIF,DP,DETAI,DPI
      REAL*8  AI0,ETAIS,K,N,P0,KAPPA,R02,ZETAF,PENPE,PK,PE,SPE,SR
      REAL*8  DDFDDS(6,6),DRSDS(6,6),SEQUIV,DEV(6),DFDS(6),DRIDS,DRPDP
      REAL*8  R8PREM,LCNRTS

      INTEGER IRET
      LOGICAL LDRPDP
C     ------------------------------------------------------------------
      DATA  I4    /1.0D0   , 0.0D0  , 0.0D0  , 0.0D0  ,0.0D0  ,0.0D0,
     &             0.0D0   , 1.0D0  , 0.0D0  , 0.0D0  ,0.0D0  ,0.0D0,
     &             0.0D0   , 0.0D0  , 1.0D0  , 0.0D0  ,0.0D0  ,0.0D0,
     &             0.0D0   , 0.0D0  , 0.0D0  , 1.0D0  ,0.0D0  ,0.0D0,
     &             0.0D0   , 0.0D0  , 0.0D0  , 0.0D0  ,1.0D0  ,0.0D0,
     &             0.0D0   , 0.0D0  , 0.0D0  , 0.0D0  ,0.0D0  ,1.0D0/
C     ------------------------------------------------------------------

      CALL LCEQVN(NDT,SIGF,YF)
      CALL LCEQVN(NDT,SIGD,YD)

      CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
      CALL LCEQVN ( NVI-1,  VINF , YF(NDT+1) )

      IF ( MOD.EQ.'C_PLAN') THEN
        CALL LCEQVN ( (NR-1),  YF , DY )
        CALL DAXPY ( (NR-1), -1.D0, YD, 1,DY, 1)
      ELSE
        CALL LCEQVN ( NR,  YF , DY )
        CALL DAXPY ( NR, -1.D0, YD, 1,DY, 1)
      ENDIF

C     CALCUL DE LA MATRICE JACOBIENNE ==> methode b (plus sure)
C     a) Faire appel a IRRJAC
C        certaines des équations sont normées   ==> précautions
C        si le jacobien est changé              ==> répercutions
C        CALL IRRJAC (FAMI,KPG,KSP,MOD,NMAT,MATER,YF,DY,NR,DRDY)
C     b) Calcul qui ressemble a IRRJAC
C        independant de IRRJAC
C        duplication de code

      CALL LCOPIL ( 'ISOTROPE' , MOD , MATER(1,1) , FKOOH )
      CALL RCVARC ('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
      CALL RCVARC ('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)
C     ARRET DANS IRRMAT SI  IRRAD .GT. IRRAF*1.00001
      IF ( IRRAD .GT. IRRAF ) THEN
         DPHI = 0.0D0
      ELSE
         DPHI = IRRAF - IRRAD
      ENDIF

C     RECUPERATION DES VARIABLES INTERNES
      PF    = YF(NDT+1)
      ETAIF = YF(NDT+2)
C     RECUPERATION DES INCREMENTS DES VARIABLES INTERNES
      DP    = DY(NDT+1)
      DETAI = DY(NDT+2)
      DPI   = DY(NDT+3)

C     CARACTERISTIQUES MATERIAUX
      AI0   = MATER(4,2)
      ETAIS = MATER(5,2)
      K     = MATER(7,2)
      N     = MATER(8,2)
      P0    = MATER(9,2)
      KAPPA = MATER(10,2)
      R02   = MATER(11,2)
      ZETAF = MATER(12,2)
      PENPE = MATER(13,2)
      PK    = MATER(14,2)
      PE    = MATER(15,2)
      SPE   = MATER(16,2)

C     Calcul de DRSDS : (6,6)
      CALL IRRFSS(SIGF,DDFDDS)
      CALL LCPRSM((DP+DPI),DDFDDS,DRSDS)
      CALL LCSOMA(FKOOH,DRSDS,DRSDS)

C     Calcul de DRPDP : scalaire
C     loi de comportement : Calcul du seuil
      IF      ( PF .LT. PK ) THEN
         SR = KAPPA*R02
      ELSE IF ( PF .LT. PE ) THEN
         SR = PENPE*(PF - PE) + SPE
      ELSE
         SR = K*((PF + P0)**N)
      ENDIF
C     Calcul de Sigma equivalent
      CALL LCDEVI(SIGF,DEV)
      SEQUIV = LCNRTS(DEV)
      LDRPDP = .TRUE.
      IF (((SEQUIV.GE.SR).AND.(DP.GE.0.D0)).OR.(DP.GT.R8PREM())) THEN
         IF      ( PF .LT. PK ) THEN
            LDRPDP = .FALSE.
         ELSE IF ( PF .LT. PE ) THEN
            DRPDP = PENPE
         ELSE
            DRPDP = N*K*((PF+P0)**(N-1.D0))
         ENDIF
      ELSE
         LDRPDP = .FALSE.
      ENDIF
C     Calcul de DRIDS : scalaire
      IF      ( (ETAIF-DETAI) .GT. ETAIS ) THEN
         DRIDS = AI0*DPHI*ZETAF
      ELSE IF ( ETAIF .LE. ETAIS ) THEN
         DRIDS = 0.0D0
      ELSE IF ( DETAI .GT. 0.0D0 ) THEN
         DRIDS = AI0*DPHI*ZETAF*(ETAIF-ETAIS)/DETAI
      ELSE
         DRIDS = 0.0D0
      ENDIF
      IF ( SEQUIV.EQ.0.0D0) THEN
         CALL LCINMA(0.0D0,DDFDDS)
      ELSE
         CALL LCPRSV( 1.5D0/SEQUIV , DEV , DFDS )
         CALL LCPRTE(DFDS , DFDS , DDFDDS )
      ENDIF

      IF ( LDRPDP ) THEN
         CALL LCPRSM(1.0D0/DRPDP + DRIDS,DDFDDS,DDFDDS)
      ELSE
         CALL LCPRSM(DRIDS,DDFDDS,DDFDDS)
      ENDIF

C     Assemblage de DRSDS et DDFDDS : (6,6)
      CALL LCSOMA(DRSDS,DDFDDS,MAT)

C     Inversion de MAT : DSDE(6,6)
      CALL LCEQMA ( I4, DSDE)
      CALL MGAUSS('NFVP',MAT,DSDE,6,NDT,NDT,DET,IRET)

      END
