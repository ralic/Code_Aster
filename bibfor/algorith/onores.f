        SUBROUTINE ONORES ( MOD, NMAT, MATERD, MATERF,
     1                      YD , YF, DEPS, DY, R)
        IMPLICIT REAL*8 (A-H,O-Z)

C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C       OHNO   : CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = R(DY)
C                    DY = ( DSIG DX1 DX2 DX3 DX4 DX5  DP (DEPS3) )
C                    Y  = ( SIG  X1  X2 X3 X4 X5   P  (EPS3)  )
C                    R  = ( G    L   J   F  (Q)     )
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YD     :  VARIABLES A T       = ( SIGD VIND (EPSD3)  )
C           YF     :  VARIABLES A T + DT  = ( SIGF VINF (EPSF3)  )
C           DY     :  SOLUTION ESSAI      = ( DSIG DVIN (DEPS3) )
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT R      :  SYSTEME NL A T + DT
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT
        REAL*8          UN  , D23
        PARAMETER       ( D23  =  .66666666666666D0 )
        PARAMETER       ( UN   = 1.D0   )
C
        REAL*8      HOOKF(6,6), DKOOH(6,6)
        REAL*8      SIGF(6)   , DSIG(6) ,    SIGD(6) ,    DFDS(6)
        REAL*8      DEPS(6)   , DEPSP(6),    DEPSE(6)
        REAL*8      EPSED(6)  , EPSEF(6)
        REAL*8      X1(6)     , DX1(6)
        REAL*8      X2(6)     , DX2(6)
        REAL*8      P         , DP
        REAL*8      GF(6)     , LF(6),     JF(6),   FF,   QF
        REAL*8      R(*)      , DY(*),     YD(*),   YF(*)
        REAL*8      VTMP1(6)  , VTMP2(6)
        REAL*8      H1,  H2   , ONOCIN
C
        REAL*8      MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8      SEUIL
        REAL*8      B,  PHI,  A1,  A2,  C1,  C2, NU
        REAL*8      GAMMA1, GAMMA2, M1, M2, XI1, XI2, NORMX1, NORMX2
        REAL*8      SCR1, SCR2
C
        REAL*8      X3(6) , DX3(6)
        REAL*8      X4(6) , DX4(6)
        REAL*8      X5(6) , DX5(6)
        REAL*8      IF(6)  , MF(6),  NF(6)
        REAL*8      A3, C3, GAMMA3, M3, XI3, NORMX3, SCR3, J1
        REAL*8      A4, C4, GAMMA4, M4, XI4, NORMX4, SCR4, J2
        REAL*8      A5, C5, GAMMA5, M5, XI5, NORMX5, SCR5, J3
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        CALL LCEQVN ( NDT , YF(1)       , SIGF)
        CALL LCEQVN ( NDT , YD(1)       , SIGD)
        CALL LCEQVN ( NDT , YF(NDT+1)   , X1  )
        CALL LCEQVN ( NDT , YF(2*NDT+1) , X2  )
        CALL LCEQVN ( NDT , YF(3*NDT+1) , X3  )
        CALL LCEQVN ( NDT , YF(4*NDT+1) , X4  )
        CALL LCEQVN ( NDT , YF(5*NDT+1) , X5  )
        P  = YF(6*NDT+1)
        CALL LCEQVN ( NDT , DY(1)      , DSIG )
        CALL LCEQVN ( NDT , DY(NDT+1)  , DX1  )
        CALL LCEQVN ( NDT , DY(2*NDT+1), DX2  )
        CALL LCEQVN ( NDT , DY(3*NDT+1), DX3  )
        CALL LCEQVN ( NDT , DY(4*NDT+1), DX4  )
        CALL LCEQVN ( NDT , DY(5*NDT+1), DX5  )
        DP = DY(6*NDT+1)
C
C        RI     = MATERF(1,2)
C        R0     = MATERF(2,2)
        B      = MATERF(3,2)
        PHI    = MATERF(4,2)
        A1     = MATERF(5,2)
        A2     = MATERF(6,2)
        A3     = MATERF(7,2)
        A4     = MATERF(8,2)
        A5     = MATERF(9,2)
        GAMMA1 = MATERF(10,2)
        GAMMA2 = MATERF(11,2)
        GAMMA3 = MATERF(12,2)
        GAMMA4 = MATERF(13,2)
        GAMMA5 = MATERF(14,2)
        M1     = MATERF(15,2)
        M2     = MATERF(16,2)
        M3     = MATERF(17,2)
        M4     = MATERF(18,2)
        M5     = MATERF(19,2)

        XI1 = A1/(GAMMA1*ONOCIN(PHI,B,P))
        XI2 = A2/(GAMMA2*ONOCIN(PHI,B,P))
        XI3 = A3/(GAMMA3*ONOCIN(PHI,B,P))
        XI4 = A4/(GAMMA4*ONOCIN(PHI,B,P))
        XI5 = A5/(GAMMA5*ONOCIN(PHI,B,P))
C
        CALL ONOFS(SIGF,X1, X2, X3, X4, X5, DFDS)
C
        CALL LCPRSC ( X1, X1, NORMX1 )
        NORMX1 = SQRT (1.5D0 * NORMX1)
        CALL LCPRSN (NDT, DFDS, X1, SCR1)
C
        CALL LCPRSC ( X2, X2, NORMX2 )
        NORMX2 = SQRT (1.5D0 * NORMX2)
        CALL LCPRSN (NDT, DFDS, X2, SCR2)
C
        CALL LCPRSC ( X3, X3, NORMX3 )
        NORMX3 = SQRT (1.5D0 * NORMX3)
        CALL LCPRSN (NDT, DFDS, X3, SCR3)
C
        CALL LCPRSC ( X4, X4, NORMX4 )
        NORMX4 = SQRT (1.5D0 * NORMX4)
        CALL LCPRSN (NDT, DFDS, X4, SCR4)
C
        CALL LCPRSC ( X5, X5, NORMX5 )
        NORMX5 = SQRT (1.5D0 * NORMX5)
        CALL LCPRSN (NDT, DFDS, X5, SCR5)
C
        IF (NORMX1.NE.0.D0) THEN
        C1 = GAMMA1*ONOCIN(PHI,B,P)*(1.D0/XI1)**M1*(NORMX1**(M1-UN))
        C1 = C1*SCR1
        ELSE
        C1 = 0.D0
        ENDIF
C
        IF (NORMX2.NE.0.D0) THEN
        C2 = GAMMA2*ONOCIN(PHI,B,P)*(1.D0/XI2)**M2*(NORMX2**(M2-UN))
        C2 = C2*SCR2
        ELSE
        C2 = 0.D0
        ENDIF
C
        IF (NORMX3.NE.0.D0) THEN
        C3 = GAMMA3*ONOCIN(PHI,B,P)*(1.D0/XI3)**M3*(NORMX3**(M3-UN))
        C3 = C3*SCR3
        ELSE
        C3 = 0.D0
        ENDIF
C
        IF (NORMX4.NE.0.D0) THEN
        C4 = GAMMA4*ONOCIN(PHI,B,P)*(1.D0/XI4)**M4*(NORMX4**(M4-UN))
        C4 = C4*SCR4
        ELSE
        C4 = 0.D0
        ENDIF
C
        IF (NORMX5.NE.0.D0) THEN
        C5 = GAMMA5*ONOCIN(PHI,B,P)*(1.D0/XI5)**M5*(NORMX5**(M5-UN))
        C5 = C5*SCR5
        ELSE
        C5 = 0.D0
        ENDIF
C
C                   -1                     -1
C -     HOOKF, HOOKD , DFDS , EPSEF = HOOKD  SIGD + DEPS - DEPSP
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
        CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
C
        CALL ONOFS   ( SIGF, X1, X2, X3,X4, X5, DFDS )

         H1 = - A1 * D23
         H2 = - A2 * D23
       J1 = - A3 * D23
       J2 = - A4 * D23
       J3 = - A5 * D23
C
        CALL LCPRSV ( DP   ,   DFDS  , DEPSP )
        CALL LCPRMV ( DKOOH,   SIGD  , EPSED )
        CALL LCDIVE ( DEPS ,   DEPSP , DEPSE )
        CALL LCSOVE ( EPSED,   DEPSE , EPSEF )
C
C - GF  (T+DT)
C
        CALL LCPRMV  ( HOOKF , EPSEF , GF   )
        CALL LCDIVE  ( GF    , SIGF  , GF   )
C
C - LF (T+DT)
C
        CALL LCPRSV ( -C1*DP , X1    , LF )
        CALL LCPRSV ( -H1*DP , DFDS  , VTMP1 )
        CALL LCSOVE (  LF    , VTMP1 , LF )
        CALL LCDIVE (  LF    , DX1   , LF )
C
C - JF (T+DT)
C
        CALL LCPRSV ( -C2*DP , X2    , JF )
C          CALL LCPRSV ( -DP , X2    , JF )
        CALL LCPRSV ( -H2*DP , DFDS  , VTMP1)
        CALL LCSOVE (  JF    , VTMP1 , JF )
         CALL LCDIVE (  JF    , DX2   , JF )
C
C - IF (T+DT)
C
        CALL LCPRSV ( -C3*DP , X3    , IF )
        CALL LCPRSV ( -J1*DP , DFDS  , VTMP1)
        CALL LCSOVE (  IF    , VTMP1 , IF )
        CALL LCDIVE (  IF    , DX3   , IF )
C
C - MF (T+DT)
C
        CALL LCPRSV ( -C4*DP , X4    , MF )
        CALL LCPRSV ( -J2*DP , DFDS  , VTMP1)
        CALL LCSOVE (  MF    , VTMP1 , MF )
        CALL LCDIVE (  MF    , DX4   , MF )
C
C - NF (T+DT)
C
        CALL LCPRSV ( -C5*DP , X5    , NF )
        CALL LCPRSV ( -J3*DP , DFDS  , VTMP1)
        CALL LCSOVE (  NF    , VTMP1 , NF )
        CALL LCDIVE (  NF    , DX5   , NF )
C
C
C - FF (T+DT)
C
        CALL ONOCVX (NMAT, MATERF,SIGF,YF(NDT+1),SEUIL)
        FF = - SEUIL
C
C - QF  (T+DT)  EN CP
C
                IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
                QF = - HOOKF(3,3) *  EPSEF(3)
     1               - HOOKF(3,1) *  EPSEF(1)
     2               - HOOKF(3,2) *  EPSEF(2)
     3               - HOOKF(3,4) *  EPSEF(4)
                ENDIF
C
C - R (T+DT) = ( GF LF JF FF (QF) )
C
        CALL LCEQVN ( NDT , GF     , R(1) )
        CALL LCEQVN ( NDT , LF     , R(NDT+1) )
        CALL LCEQVN ( NDT , JF     , R(2*NDT+1) )
        CALL LCEQVN ( NDT , IF     , R(3*NDT+1) )
        CALL LCEQVN ( NDT , MF     , R(4*NDT+1) )
        CALL LCEQVN ( NDT , NF     , R(5*NDT+1) )
        R(6*NDT+1) = FF
          IF ( MOD(1:6).EQ.'C_PLAN' ) R(6*NDT+2) = QF


        END
