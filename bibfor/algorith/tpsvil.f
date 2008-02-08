      SUBROUTINE TPSVIL(TPS,S,DPC,TEMP,FLUPHI,A,B,CTPS,ENER,PREC,NITER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
       IMPLICIT NONE
       REAL*8  TPS,S,DPC,TEMP,FLUPHI,A,B,CTPS,ENER,PREC
       INTEGER NITER
C
C---------------------------------------------------------------
C     CALCUL DE TPS (TEMPS) EN FONCTION DE SIGMA,LAMBDA,T
C---------------------------------------------------------------
C IN  S     :R: CONTRAINTE EQUIVALENTE SIGMA
C     DPC   :R: SCALAIRE RESUMANT L'ETAT VISCOPLASTIQUE DU POINT
C               CONSIDERE DU MATERIAU (LAMBDA)
C     TEMP  :R: TEMPERATURE DU POINT CONSIDERE
C     FLUPHI:R: PARAMETRE FLUX_PHI
C     A     :R: PARAMETRE A
C     B     :R: PARAMETRE B
C     CTPS  :R: PARAMETRE CSTE_TPS
C     ENER  :R: PARAMETRE ENER_ACT
C     PREC  :R: PRECISION DE LA RESOLUTION EN TEMPS
C     NITER :I: NOMBRE D'ITERATIONS DE LA RESOLUTION EN TEMPS
C OUT TPS   :R: TEMPS CALCULE
C---------------------------------------------------------------
C            DANS LE CAS DE LA LOI DE FLUAGE AXIAL EN LOG
C        DE LA FLUENCE (PROJET PACHYDERME) TUBE_LOG
C                                                         .
C     POUR L'ELIMINATION (NUMERIQUE) DU TEMPS ENTRE EV ET EV ,
C     CETTE ROUTINE RESOUT EN TEMPS L'EQUATION :
C
C            EV = F(SIGMA,TEMPS,TEMPERATURE)         (EN 1D)
C
C                    OU
C
C            LAMBDA = F(SIGMA,TEMPS,TEMPERATURE)     (EN 3D)
C---------------------------------------------------------------
C
      REAL*8 F1,F2,FP1,FP2,G1,G2,FF,FFP,TPS1,TPSANC,TEST
      INTEGER ITER

      ITER = 0
      IF (DPC.EQ.0.D0) THEN
        TPS = 0.D0
      ELSE
        TPS = 1.D-3
   10   CONTINUE


C----CALCUL DE F1,FP1-------------------------------------------

        CALL ASSERT((1+(CTPS*TPS*FLUPHI)).GT.0.D0)

        F1 = LOG(1+CTPS*TPS*FLUPHI)
        FP1= CTPS*FLUPHI / (1+CTPS*TPS*FLUPHI)
C
C----CALCUL DE F2,FP2-------------------------------------------
C
        F2 = TPS*FLUPHI
        FP2= FLUPHI
C
C----CALCUL DE G1-----------------------------------------------
C
        G1 = A*EXP(-ENER/(TEMP+273.15D0))*S
C
C----CALCUL DE G2-----------------------------------------------
C
        G2 = B*EXP(-ENER/(TEMP+273.15D0))*S
C
C----CALCUL DE F -----------------------------------------------
C
        FF = F1*G1+F2*G2
        IF (FF.GT.DPC) THEN
          FFP  = FP1*G1 + FP2*G2
          TPS1 = TPS - (FF-DPC)/FFP
          IF (TPS1.GT.0.D0) THEN
            TPS = TPS1
            GO TO 20
          ELSE
            TPS = TPS * 0.5D0
            GO TO 10
          ENDIF
        ENDIF

   20   CONTINUE
        ITER = ITER + 1
        IF (ITER.EQ.NITER) THEN
           CALL U2MESS('F','ALGORITH10_92')
        ENDIF

C
C----CALCUL DE F1,FP1-------------------------------------------
C
        CALL ASSERT ((1+(CTPS*TPS*FLUPHI)).GT.0.D0)

        F1 = LOG(1+CTPS*TPS*FLUPHI)
        FP1= CTPS*FLUPHI / (1+CTPS*TPS*FLUPHI)
C
C----CALCUL DE F2,FP2-------------------------------------------
C
        F2 = TPS*FLUPHI
        FP2= FLUPHI
C
C----CALCUL DE G1-----------------------------------------------
C
        G1 = A*EXP(-ENER/(TEMP+273.15D0))*S
C
C----CALCUL DE G2-----------------------------------------------
C
        G2 = B*EXP(-ENER/(TEMP+273.15D0))*S
C
C----CALCUL DE F -----------------------------------------------
C
        FF  = F1*G1+F2*G2
        FFP = FP1*G1 + FP2*G2
        TPSANC = TPS
        TPS  = (TPS*FFP-FF+DPC)/FFP
        TEST = ABS((TPS-TPSANC)/TPSANC)


        IF (TEST.GT.PREC) GO TO 20
      ENDIF

  299 CONTINUE
      END
