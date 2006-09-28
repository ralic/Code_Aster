      SUBROUTINE TPSEPR(TPS,S,DPC,TEMP,FLUPHI,VALDRP,TTAMAX,PREC,NITER)
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
C
      IMPLICIT NONE
      REAL*8  TPS,S,DPC,TEMP,FLUPHI,VALDRP,TTAMAX,PREC
      INTEGER NITER
C
C---------------------------------------------------------------
C     CALCUL DE TPS (TEMPS) EN FONCTION DE SIGMA,LAMBDA,T
C---------------------------------------------------------------
C IN  S     :R: CONTRAINTE EQUIVALENTE SIGMA
C     DPC   :R: SCALAIRE RESUMANT L'ETAT VISCOPLASTIQUE DU POINT
C               CONSIDERE DU MATERIAU (LAMBDA)
C     TEMP  :R: TEMPERATURE DU POINT CONSIDERE
C     FLUPHI:R: FLUX NEUTRONIQUE
C     VALDRP:R: LIMITE ELASTIQUE R_P
C     TTAMAX:R: ANGLE THETA_MAX
C     PREC  :R: PRECISION DE LA RESOLUTION EN TEMPS
C     NITER :I: NOMBRE D'ITERATIONS DE LA RESOLUTION EN TEMPS
C OUT TPS   :R: TEMPS CALCULE
C---------------------------------------------------------------
C                                                         .
C     POUR L'ELIMINATION (NUMERIQUE) DU TEMPS ENTRE EV ET EV ,
C     CETTE ROUTINE RESOUT EN TEMPS L'EQUATION :
C
C            EV = F(SIGMA,TEMPS,TEMPERATURE)        (EN 1D)
C
C                    OU
C
C            LAMBDA = F(SIGMA,TEMPS,TEMPERATURE)    (EN 3D)
C---------------------------------------------------------------
CFIN
C
C---------------------------------------------------------------
      REAL*8 A1,A2,A3,A4,A5,A6,A7,B1,B2,B3,B4,B5,B6,B7
      REAL*8 F1,F2,FP1,FP2,G1,G2,FF,FFP
      REAL*8 TPS1,TPSANC,TEST
      INTEGER ITER

      ITER = 0
C---------------------------------------------------------------
C     ECRITURE DE LA LOI DE FLUAGE EN CONTRAINTE ET
C      DEFORMATION VISQUEUSE EQUIVALENTES (AU LIEU DE
C      CONTRAINTE ET DEFORMATION VISQUEUSE CIRCONFERENTIELLES),
C     CE QUI SE TRADUIT PAR LA MODIFICATION DES COEF A1,A2 ET B1 :
C
C     A1 = 1.388D+8/R3S2
C     A2 = 3.29D-5/(R3S2**A3)
C
C     B1 = 2.35D-21/(R3S2**(B4+1))
C
      A1 = 1.603D+8
      A2 = 4.567D-5
      A3 = 2.28D0
      A4 = 0.997D0
      A5 = 0.77D0
      A6 = 0.956D0
      A7 = 23000.D0
      B1 = 3.296D-21
      B2 = 0.811D0
      B3 = 0.595D0
      B4 = 1.352D0
      B5 = 22.91D0
      B6 = 1.58D0
      B7 = 2.228D0
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
        F1 = EXP(A5*LOG(TPS))
        FP1= A5*F1/TPS
C
C----CALCUL DE F2,FP2-------------------------------------------
C
        F2 = EXP(B2*LOG(TPS))
        FP2= B2*F2/TPS
C
C----CALCUL DE G1-----------------------------------------------
C
        G1 = A1*EXP(A4*LOG(SINH(A2*EXP(A3*LOG(S))))+A6*LOG(VALDRP)
     &       -A7/(TEMP+273.15D0))
C
C----CALCUL DE G2-----------------------------------------------
C
        IF (FLUPHI.EQ.0.D0) THEN
          G2 = 0.D0
        ELSE
          G2 = B1*EXP(B3*LOG(FLUPHI)+B4*LOG(S)-B5/(TEMP+273.15D0)
     &         +B6*LOG(VALDRP)+B7*LOG(COS(TTAMAX)))
        ENDIF
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
        F1 = EXP(A5*LOG(TPS))
        FP1= A5*F1/TPS
C
C----CALCUL DE F2,FP2-------------------------------------------
C
        F2 = EXP(B2*LOG(TPS))
        FP2= B2*F2/TPS
C
C----CALCUL DE G1-----------------------------------------------
C
        G1 = A1*EXP(A4*LOG(SINH(A2*EXP(A3*LOG(S))))+A6*LOG(VALDRP)
     &       -A7/(TEMP+273.15D0))
C
C----CALCUL DE G2-----------------------------------------------
C
        IF (FLUPHI.EQ.0.D0) THEN
          G2 = 0.D0
        ELSE
          G2 = B1*EXP(B3*LOG(FLUPHI)+B4*LOG(S)-B5/(TEMP+273.15D0)
     &         +B6*LOG(VALDRP)+B7*LOG(COS(TTAMAX)))
        ENDIF
C---------------------------------------------------------------
C---------------------------------------------------------------
C
        FF = F1*G1+F2*G2
        FFP = FP1*G1 + FP2*G2
        TPSANC = TPS
        TPS = TPS - (FF-DPC)/FFP
        TEST = ABS((TPS-TPSANC)/TPSANC)
        IF (TEST.GT.PREC) GO TO 20
      ENDIF
      END
