      SUBROUTINE TPSCYR(TPS,S,DPC,TEMP,EPSFAB,TPREC,FLUPHI,PREC,NITER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      REAL*8  TPS,S,DPC,TEMP,EPSFAB,TPREC,FLUPHI,PREC
      INTEGER NITER
C
CDEB
C---------------------------------------------------------------
C     CALCUL DE TPS (TEMPS) EN FONCTION DE SIGMA,LAMBDA,T
C---------------------------------------------------------------
C IN  S     :R: CONTRAINTE EQUIVALENTE SIGMA
C     DPC   :R: SCALAIRE RESUMANT L'ETAT VISCOPLASTIQUE DU POINT
C               CONSIDERE DU MATERIAU (LAMBDA)
C     TEMP  :R: TEMPERATURE DU POINT CONSIDERE
C     EPSFAB:R: PARAMETRE EPS_FAB
C     TPREC :R: PARAMETRE TEMP_RECUIT
C     FLUPHI:R: PARAMETRE FLUX_PHI
C     PREC  :R: PRECISION DE LA RESOLUTION EN TEMPS
C     NITER :I: NOMBRE D'ITERATIONS DE LA RESOLUTION EN TEMPS
C OUT TPS   :R: TEMPS CALCULE
C---------------------------------------------------------------
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
CFIN
C
      REAL*8 R8GAEM,R3S2
      REAL*8 CTH,CTPS,FREC,CTPS2,ATH,XN,XK,AIRR
      REAL*8 S1,TPS1,TPSANC,TEST
      REAL*8 F1,F2,FP1,FP2,G1,G2,FF,FFP
      INTEGER ITER

      ITER = 0
C
      R3S2 = 0.5D0*SQRT(3.D0)
C
      IF (DPC.EQ.0.D0) THEN
        TPS = 0.D0
      ELSE
        TPS = 1.D-3
   10   CONTINUE
C
C---------------------------------------------------------------
C---------------------------------------------------------------
C
C----CALCUL DE F1,FP1-------------------------------------------
C
        CTH  = 4450.D0
        CTPS = 4.5 D-3
        FREC = 1.816D-4*EXP(6400.D0/(TPREC+273.15D0))
C
C ---      ATTENTION : MESSAGE D'ERREUR SI EXP(X<-LOG(R8GAEM()))
C                      D'OU LE TEST SUIVANT SUR CTPS2
C
        CTPS2=CTPS*TPS
        IF (CTPS2.GE.LOG(R8GAEM())) THEN
           F1 = (CTH*EPSFAB+TPS)*FREC
           FP1= FREC
        ELSE
           IF (ABS(CTPS2).GE.1.D-12) THEN
              F1 = (CTH*EPSFAB*(1.D0-EXP(-CTPS2))+TPS)*FREC
           ELSE
              F1 = (CTH*EPSFAB*CTPS2+TPS)*FREC
           ENDIF
           FP1 = (CTH*EPSFAB*CTPS*EXP(-CTPS2)+1.D0)*FREC
        ENDIF
C
C----CALCUL DE F2,FP2-------------------------------------------
C
        CTH = 4000.D0
        CTPS = 3. D-3
C
C ---      ATTENTION : MESSAGE D'ERREUR SI EXP(X<-LOG(R8GAEM()))
C                      D'OU LE TEST SUIVANT SUR CTPS2
C
        CTPS2=CTPS*TPS
        IF (CTPS2.GE.LOG(R8GAEM())) THEN
           F2 = (CTH*EPSFAB+TPS)*FREC
           FP2= FREC
        ELSE
           IF (ABS(CTPS2).GE.1.D-12) THEN
              F2 = (CTH*EPSFAB*(1.D0-EXP(-CTPS2))+TPS)*FREC
           ELSE
              F2 = (CTH*EPSFAB*CTPS2+TPS)*FREC
           ENDIF
           FP2 = (CTH*EPSFAB*CTPS*EXP(-CTPS2)+1.D0)*FREC
        ENDIF
C
C----CALCUL DE G1-----------------------------------------------
C
C       LES CONTRAINTES DOIVENT ETRE CONVERTIES DE N/CM2 EN MPA
C       S1=S/100.D0
        S1=S
        S1=S1/R3S2
C
        ATH = 9.529D17
        XN = EXP(2.304D-3*S1-0.413D0)
        XK = 39000.D0
        G1 = ATH*EXP(-XK/(TEMP+273.15D0))*S1**XN
        G1 = G1/R3S2
C
C----CALCUL DE G2-----------------------------------------------
C
C       LES CONTRAINTES DOIVENT ETRE CONVERTIES DE N/CM2 EN MPA
C       S1=S/100.D0
C       S1=S1/R3S2
C
        AIRR = 1.2D-22
        G2 = AIRR*FLUPHI*S1/R3S2
C---------------------------------------------------------------
C---------------------------------------------------------------
C
        FF = F1*G1+F2*G2
        IF (FF.GT.DPC) THEN
          FFP = FP1*G1 + FP2*G2
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
C---------------------------------------------------------------
C---------------------------------------------------------------
C
C----CALCUL DE F1,FP1-------------------------------------------
C
        CTH = 4450.D0
        CTPS = 4.5 D-3
        FREC = 1.816D-4*EXP(6400.D0/(TPREC+273.15D0))
C
C ---      ATTENTION : MESSAGE D'ERREUR SI EXP(X<-LOG(R8GAEM()))
C                      D'OU LE TEST SUIVANT SUR CTPS2
C
        CTPS2=CTPS*TPS
        IF (CTPS2.GE.LOG(R8GAEM())) THEN
           F1 = (CTH*EPSFAB+TPS)*FREC
           FP1= FREC
        ELSE
           IF (ABS(CTPS2).GE.1.D-12) THEN
              F1 = (CTH*EPSFAB*(1.D0-EXP(-CTPS2))+TPS)*FREC
           ELSE
              F1 = (CTH*EPSFAB*CTPS2+TPS)*FREC
           ENDIF
           FP1 = (CTH*EPSFAB*CTPS*EXP(-CTPS2)+1.D0)*FREC
        ENDIF
C
C----CALCUL DE F2,FP2-------------------------------------------
C
        CTH = 4000.D0
        CTPS = 3. D-3
C
C ---      ATTENTION : MESSAGE D'ERREUR SI EXP(X<-LOG(R8GAEM()))
C                      D'OU LE TEST SUIVANT SUR CTPS2
C
        CTPS2=CTPS*TPS
        IF (CTPS2.GE.LOG(R8GAEM())) THEN
           F2 = (CTH*EPSFAB+TPS)*FREC
           FP2= FREC
        ELSE
           IF (ABS(CTPS2).GE.1.D-12) THEN
              F2 = (CTH*EPSFAB*(1.D0-EXP(-CTPS2))+TPS)*FREC
           ELSE
              F2 = (CTH*EPSFAB*CTPS2+TPS)*FREC
           ENDIF
           FP2 = (CTH*EPSFAB*CTPS*EXP(-CTPS2)+1.D0)*FREC
        ENDIF
C
C----CALCUL DE G1-----------------------------------------------
C
C       LES CONTRAINTES DOIVENT ETRE CONVERTIES DE N/CM2 EN MPA
C       S1=S/100.D0
        S1=S
        S1=S1/R3S2
C
        ATH = 9.529D17
        XN = EXP(2.304D-3*S1-0.413D0)
        XK = 39000.D0
        G1 = ATH*EXP(-XK/(TEMP+273.15D0))*S1**XN
        G1 = G1/R3S2
C
C----CALCUL DE G2-----------------------------------------------
C
C       LES CONTRAINTES DOIVENT ETRE CONVERTIES DE N/CM2 EN MPA
C       S1=S/100.D0
C       S1=S1/R3S2
C
        AIRR = 1.2D-22
        G2 = AIRR*FLUPHI*S1/R3S2
C---------------------------------------------------------------
C---------------------------------------------------------------
C
        FF = F1*G1+F2*G2
        FFP = FP1*G1 + FP2*G2
        TPSANC = TPS
        TPS = (TPS*FFP-FF+DPC)/FFP
        TEST = ABS((TPS-TPSANC)/TPSANC)
        IF (TEST.GT.PREC) GO TO 20
      ENDIF
      END
