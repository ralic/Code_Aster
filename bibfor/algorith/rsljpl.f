        SUBROUTINE RSLJPL(FAMI,KPG,KSP,LOI,IMAT,NMAT,MATER,SIG,VIN,
     &                    VIND,DEPS,THETA,DT,DSDE)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
C       ----------------------------------------------------------------
C       ROUSSELIER :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C            COHERENT ELASTO-PLASTIQUE EN VITESSE A T+DT OU T
C       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           SIG    :  CONTRAINTES
C           VIN    :  VARIABLES INTERNES
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
        INTEGER         KPG,KSP,IMAT , NMAT, IRET
C
        REAL*8          V1(6), V2(6), I2(6)
        REAL*8          M1(6,6), M2(6,6), M3(6,6), DSDE(6,6), I4(6,6)
        REAL*8          VIN(*), VIND(*)
        REAL*8          DEPS(6), SIG(6), RIG(6), RIGDV(6)
        REAL*8          RIGMO, RIGEQ, RIGEQ2, EPS0, SIG0, MEXPO, ACC
        REAL*8          Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, EXPO, EXPE
        REAL*8          X1, X2, Y1, Y2, Y3, Y4, Y5, A1, A2, A3, A4
        REAL*8          A5, A6, D , S1, F, FD, F0, P, RHO, UNF
        REAL*8          NU, E, DEUXMU, MU, TROIMU, TROISK, K
        REAL*8          MATER(NMAT,2) , TEMP , RP , DRDP, DP, DPM
        REAL*8          ZERO, UN , DEUX, TROIS, ANN, PD, FTOT, FDTOT
        REAL*8          NDEPS, NSIG, LCNRTS, LCNRTE, THETA
        REAL*8          DPUISS, PUISS, GP, DT
C
        CHARACTER*16    LOI
        CHARACTER*(*)     FAMI
C
        PARAMETER       ( ZERO  = 0.D0   )
        PARAMETER       ( UN    = 1.D0   )
        PARAMETER       ( DEUX  = 2.D0   )
        PARAMETER       ( TROIS = 3.D0   )
C
        DATA  I4        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
        DATA I2         /UN     , UN    , UN    , ZERO  ,ZERO  ,ZERO/
C       ----------------------------------------------------------------
C
C -- INITIALISATION----------------------------------------------
C
      P  = VIN(1)
      F  = VIN(2)
      PD = VIND(1)
      FD = VIND(2)
      E  = MATER(1,1)
      NU = MATER(2,1)
      D  = MATER(1,2)
      S1 = MATER(2,2)
      F0 = MATER(3,2)
      IF (LOI(1:10).EQ.'ROUSS_VISC') THEN
        ANN = 0.D0
        SIG0  = MATER(9,2)
        EPS0  = MATER(10,2)
        MEXPO = MATER(11,2)
      ELSEIF (LOI(1:10).EQ.'ROUSS_PR') THEN
        ANN = MATER(8,2)
        SIG0  = 0.D0
        EPS0  = 0.D0
        MEXPO = 0.D0
      END IF
      FTOT = F + ANN*P
      FDTOT = FD + ANN*PD

C
C -- CAS DU MATERIAU CASSE---------------------------------------
      IF (FDTOT .GE. MATER(6,2)) THEN
        NDEPS = LCNRTE(DEPS)
        NSIG  = LCNRTS(SIG )
        IF ((NDEPS*NSIG) .EQ. ZERO) THEN
          CALL LCINMA(ZERO,DSDE)
        ELSE
          A1    = -DEUX*MATER(7,2)*E/(NDEPS*NSIG*TROIS)
          CALL LCPRTE(SIG, DEPS, DSDE)
          CALL LCPRSM(A1 , DSDE, DSDE)
        ENDIF
C
C -- CAS DU MATERIAU NON CASSE-----------------------------------
      ELSE
C
        CALL RSLISO ( FAMI,KPG,KSP,'-',IMAT, P, RP, DRDP )
C
        UNF= UN-F
        RHO = (UNF-ANN*P)/(UN-F0)
        CALL  LCPRSV (UN/RHO, SIG, RIG)
        CALL LCHYDR(RIG,RIGMO)
        CALL LCSOMH(RIG,-RIGMO,RIGDV)
        RIGEQ = LCNRTS(RIGDV)
        RIGEQ2= RIGEQ*RIGEQ
C
        DEUXMU = E/(UN+NU)
        MU     = DEUXMU/DEUX
        TROIMU = TROIS*MU
        TROISK = E/(UN-DEUX*NU)
        K      = TROISK/TROIS
C
C -- SI LA POROSITE EST ACCELERE----
        IF (FDTOT .GE. MATER(4,2)) THEN
          ACC = MATER(5,2)
C -- SINON-----------------------
        ELSE
          ACC = UN
        ENDIF
C -- ----------------------------
C
        DP  = P-VIND(1)
        IF (DP.GT.ZERO) THEN
          DPM = THETA*(F-FD)/(TROIS*UNF*ACC)
          EXPO = D*EXP(RIGMO/S1)
          EXPE = EXPO*S1*FTOT
C
          IF (LOI(1:10).EQ.'ROUSS_VISC') THEN
            PUISS = (DP/(DT*EPS0))**(UN/MEXPO)
            DPUISS = ((DP/(DT*EPS0))**(UN/MEXPO-UN))/(MEXPO*(DT*EPS0))
            DRDP = DRDP + SIG0*DPUISS/SQRT(UN+PUISS**2)/THETA
          END IF
C
          Z1 = UN+TROIS*DPM*ACC
          Z2 = TROIMU + DRDP
          Z3 = K*FTOT*Z1 - S1*UNF*ACC
          Z4 = DP*THETA*DRDP - RIGEQ
          Z5 = TROIMU*THETA*DP + RIGEQ
C
C
          X1 = EXPE*EXPO*Z3 + EXPO*Z2*Z3*THETA*DP + Z1*Z2*S1
     +         - ANN*Z1*EXPO*S1*S1
          X2 = -(EXPE*EXPO*Z3*DP + EXPO*Z3*Z4*THETA*DP + Z1*S1*Z4 )
     +         + ANN*Z1*EXPO*S1*S1*DP
C
          Y1 = -TROISK*EXPO*Z1*FTOT/(X1*RIGEQ)
          Y2 = -TROIMU/(X1*Z5*RIGEQ2)
          Y3 = -TROISK*EXPO*Z1*ANN*THETA*DP/(X1*RIGEQ)
C
          A1 = TROISK + Y1*K*RIGEQ*(EXPE+DP*THETA*Z2)
          A2 = MU*(Y1+Y3)*S1
          A3 = DEUXMU*(UN+Y2*X1*DP*THETA*RIGEQ2)
          A4 = Y2*TROIMU*X2
          A5 = Y2*TROISK*EXPE*Z1*Z5*RIGEQ
C
          Z6 = EXPO
          Z7 = Z6*S1*FTOT
          Z8 = UNF / (UNF - ANN*P)
          Z9 = ANN / ( UNF - ANN*P)
          A6 = TROIMU*K*THETA*DP - A2*RIGEQ*S1
          Y4 = ACC*Z8/Z1 + Z9*S1/(Z7+Z2*THETA*DP)
          Y5 = ACC*A2*Z8/Z1 - Z9*A6 / ( RIGEQ*(Z7+Z2*THETA*DP) )
C
          CALL LCPRSM(A3, I4, M1)
          CALL LCPRSV((A1-A3)/TROIS, I2, V1)
          CALL LCPRSV(A2, RIGDV, V2)
          CALL LCSOVE(V1, V2, V1)
          CALL LCPRTE(I2, V1, M2)
          CALL LCPRSV(A4, RIGDV, V1)
          CALL LCPRSV(A5/TROIS, I2, V2)
          CALL LCSOVE(V1, V2, V1)
          CALL LCPRTE(RIGDV, V1, M3)
          CALL LCSOMA(M1, M2  , DSDE)
          CALL LCSOMA(M3, DSDE, DSDE)
C
C A CE STADE DSDE EST LE TENSEUR TANGENT COHERENT
C ENTRE D(SIG/RHO) ET DEPS
C
          CALL LCPRSV(A1-TROISK, I2, V1)
          CALL LCPRSV(TROIS*Y5/Y4, RIGDV, V2)
          CALL LCSOVE(V1, V2, V1)
          CALL LCPRTE(RIG, V1, M1)
          CALL LCPRSM(Y4/TROISK, M1, M1)
          CALL LCSOMA(DSDE, M1 , DSDE)
          CALL LCPRSM(RHO, DSDE, DSDE)
C -- CAS DP=0
        ELSE
          CALL LCPRTE(I2, I2, M1)
          CALL LCPRSM(UN/TROIS, M1, M1)
          CALL LCPRSM(RHO*TROISK, M1, M2)
          CALL LCPRSM(-UN, M1, M1)
          CALL LCSOMA(I4, M1, M1)
          CALL LCPRSM(RHO*DEUXMU, M1, M1)
          CALL LCSOMA(M1, M2, DSDE)
        ENDIF
      ENDIF
C ------------------------------------------------------------------
      END
