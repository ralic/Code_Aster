      SUBROUTINE ONOINI ( TYPESS, ESSAI, MOD, NMAT,
     &                      MATERF, YD,  DEPS, DY  )
      IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C  OHNO : CALCUL SOLUTION ESSAI DY=(DSIG DX1 DX2 DX3 DX4 DX5 DP (DEPS3))
C          AVEC     Y  = ( SIG  X1  X2 X3 X4 X5  P  (EPS3))
C       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YD     :  VARIABLES A T   = ( SIG  VIN  (EPS3)  )
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C           TYPESS :  TYPE DE SOLUTION D ESSAI
C                               0 = NUL(0)
C                               1 = ELASTIQUE
C                               2 = EXPLICITE (=-1 INITIALEMENT)
C                               3 = ESSAI
C       OUT DY     :  SOLUTION ESSAI  = ( DSIG DVIN (DEPS3) )
C       ----------------------------------------------------------------
C
        INTEGER         NDT , NDI , TYPESS , NMAT
        REAL*8          UN   , D23 , D32 , ZERO
        PARAMETER       ( D23  =  .66666666666666D0 )
        PARAMETER       ( D32  = 1.5D0  )
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
C
        REAL*8          YD(*)     , DY(*),  ESSAI
        REAL*8          HOOK(6,6) , DFDS(6)
        REAL*8          DEPS(6)
        REAL*8          SIG(6)    , DSIG(6)
        REAL*8          X1(6)     , DX1(6)
        REAL*8          X2(6)     , DX2(6)
        REAL*8          X3(6)     , DX3(6)
        REAL*8          X4(6)     , DX4(6)
        REAL*8            X5(6)     , DX5(6)
        REAL*8          P         , DP
        REAL*8          VTMP(6)   , VTMP1(6), VTMP2(6)
        REAL*8          XX,  YY, ZZ, S
        REAL*8          RI,  R0, B,  PHI, A1, A2, C1, C2
        REAL*8          H1,  H2, NUN, GAMMA1, GAMMA2, M1, M2
        REAL*8          XI1, XI2, NORMX1, NORMX2, SCR1, SCR2
        REAL*8          VTMP3(6), VTMP4(6), VTMP5(6)
        REAL*8          A3,  C3, H3, GAMMA3, M3, XI3, NORMX3
        REAL*8          SCR3
        REAL*8          A4,  C4, H4, GAMMA4, M4, XI4, NORMX4
        REAL*8          SCR4
        REAL*8          A5,  C5, H5, GAMMA5, M5, XI5, NORMX5
        REAL*8          SCR5
        REAL*8          H6
        REAL*8          MATERF(NMAT,2)
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
        REAL*8          ONOCIN
C       ----------------------------------------------------------------
C
        IF ( TYPESS .EQ. -1 ) TYPESS = 2
C
C
        CALL LCEQVN ( NDT , YD(1)       , SIG )
        CALL LCEQVN ( NDT , YD(NDT+1)   , X1  )
        CALL LCEQVN ( NDT , YD(2*NDT+1) , X2  )
        CALL LCEQVN ( NDT , YD(3*NDT+1) , X3  )
        CALL LCEQVN ( NDT , YD(4*NDT+1) , X4  )
        CALL LCEQVN ( NDT , YD(5*NDT+1) , X5  )
        P = YD(6*NDT+1)
C
        RI     = MATERF(1,2)
        R0     = MATERF(2,2)
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
C
        XI1 = A1/(GAMMA1*ONOCIN(PHI,B,P))
        XI2 = A2/(GAMMA2*ONOCIN(PHI,B,P))
        XI3 = A3/(GAMMA3*ONOCIN(PHI,B,P))
        XI4 = A4/(GAMMA4*ONOCIN(PHI,B,P))
        XI5 = A5/(GAMMA5*ONOCIN(PHI,B,P))
C
        CALL ONOFS(SIG, X1, X2, X3, X4, X5,DFDS)
C
        CALL LCPRSC ( X1, X1, NORMX1 )
        NORMX1 = SQRT (1.5D0 * NORMX1)
        CALL LCPRSN (NDT, X1, DFDS, SCR1)
C
        CALL LCPRSC ( X2, X2, NORMX2 )
        NORMX2 = SQRT (1.5D0 * NORMX2)
        CALL LCPRSN (NDT, X2, DFDS, SCR2)
C
        CALL LCPRSC ( X3, X3, NORMX3 )
        NORMX3 = SQRT (1.5D0 * NORMX3)
        CALL LCPRSN (NDT, X3, DFDS, SCR3)
C
        CALL LCPRSC ( X4, X4, NORMX4 )
        NORMX4 = SQRT (1.5D0 * NORMX4)
        CALL LCPRSN (NDT, X4, DFDS, SCR4)
C
        CALL LCPRSC ( X5, X5, NORMX5 )
        NORMX5 = SQRT (1.5D0 * NORMX5)
        CALL LCPRSN (NDT, X5, DFDS, SCR5)
C
C
        IF (NORMX1.NE.0.D0) THEN
        C1 =GAMMA1*ONOCIN(PHI,B,P)*(1.D0/XI1)**M1*(NORMX1**(M1-UN))
        C1 = C1*SCR1
        ELSE
        C1 = 0.D0
        ENDIF
C
        IF (NORMX2.NE.0.D0) THEN
        C2 =GAMMA2*ONOCIN(PHI,B,P)*(1.D0/XI2)**M2*(NORMX2**(M2-UN))
        C2 = C2*SCR2
        ELSE
        C2=0.D0
        ENDIF
C
        IF (NORMX3.NE.0.D0) THEN
        C3 =GAMMA3*ONOCIN(PHI,B,P)*(1.D0/XI3)**M3*(NORMX3**(M3-UN))
        C3 = C3*SCR3
        ELSE
        C3=0.D0
        ENDIF
C
        IF (NORMX4.NE.0.D0) THEN
        C4 =GAMMA4*ONOCIN(PHI,B,P)*(1.D0/XI4)**M4*(NORMX4**(M4-UN))
        C4 = C4*SCR4
        ELSE
        C4=0.D0
        ENDIF
C
        IF (NORMX5.NE.0.D0) THEN
        C5 =GAMMA5*ONOCIN(PHI,B,P)*(1.D0/XI5)**M5*(NORMX5**(M5-UN))
        C5 = C5*SCR5
        ELSE
        C5=0.D0
        ENDIF

        NUN = MATERF(2,1) / (UN-MATERF(2,1))
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
C
C - SOLUTION INITIALE = NUL
C
                IF ( TYPESS .EQ. 0 .OR. TYPESS .EQ. 1 ) THEN
                CALL LCTRMA (HOOK , HOOK)
                CALL LCINVN( 6*NDT+2 , ZERO , DY )
                IF(MOD(1:6).EQ.'C_PLAN')THEN
                DEPS(3) = ZERO
                ENDIF
C
C - SOLUTION INITIALE = EXPLICITE
C
                ELSEIF ( TYPESS .EQ. 2 ) THEN
C
               H1 =  A1 * D23
               H2 =  A2 * D23
               H3 =  A3 * D23
               H4 =  A4 * D23
               H5 =  A5 * D23
               H6 = H1 + H2 + H3 + H4 + H5
C
C - S
                CALL LCDEVI ( SIG   , VTMP)
                CALL LCDIVE ( VTMP  , X1 , VTMP)
                CALL LCDIVE ( VTMP  , X2 , VTMP)
                CALL LCDIVE ( VTMP  , X3 , VTMP)
                CALL LCDIVE ( VTMP  , X4 , VTMP)
                CALL LCDIVE ( VTMP  , X5 , VTMP)
                CALL LCNRVE ( VTMP  , S )
                S =  SQRT ( 1.5D0 ) * S
C - DP
C
                        IF ( S .EQ. ZERO)THEN
                        DP   = ZERO
                        DO 1, I=1,6
                          DFDS(I) = ZERO
 1                      CONTINUE
                        ELSE
                   CALL ONOFS  ( SIG , X1,  X2, X3,X4,X5, DFDS )
                   ZZ = ( B * (RI - R0) * S ) / D32 * EXP(B*P)
C
                        CALL LCPRSV ( -C1   , X1   , VTMP1 )
                        CALL LCPRSV ( -C2   , X2   , VTMP2 )
                        CALL LCPRSV ( -C3   , X3   , VTMP3 )
                        CALL LCPRSV ( -C4   , X4   , VTMP4 )
                        CALL LCPRSV ( -C5   , X5   , VTMP5 )
C
                        CALL LCSOVE ( VTMP1 , VTMP2, VTMP2 )
                        CALL LCSOVE ( VTMP2 , VTMP3, VTMP2 )
                        CALL LCSOVE ( VTMP2 , VTMP4, VTMP2 )
                        CALL LCSOVE ( VTMP2 , VTMP5, VTMP2 )
C
                        CALL LCPRSV ( H6, DFDS , VTMP1 )
                        CALL LCSOVE ( VTMP1 , VTMP2, VTMP2 )
                        CALL LCPRMV ( HOOK  , DFDS , VTMP1 )
                        CALL LCSOVE ( VTMP1 , VTMP2, VTMP2 )
                        CALL LCPRSN ( NDT   , VTMP , VTMP2, XX )
                        XX = XX + ZZ
                        CALL LCPRMV ( HOOK  , DEPS , VTMP1 )
                        CALL LCPRSC ( VTMP  , VTMP1, YY )
                        DP = YY / XX
                        ENDIF
C - (DEPS(3))
                IF(MOD(1:6).EQ.'C_PLAN')THEN
                DEPS(3) = NUN * (DP*(DFDS(1)+DFDS(2))-DEPS(1)-DEPS(2))
     &                    + DFDS(3)*DP
                ENDIF
C - DX1
                CALL LCPRSV ( H1*DP , DFDS , DX1  )
                CALL LCPRSV ( -C1*DP, X1   , VTMP )
                CALL LCSOVE ( VTMP  , DX1  , DX1  )
C - DX2
                CALL LCPRSV ( H2*DP , DFDS , DX2  )
                CALL LCPRSV ( -C2*DP, X2   , VTMP )
                CALL LCSOVE ( VTMP  , DX2  , DX2  )
C - DX3
                CALL LCPRSV ( H3*DP , DFDS , DX3  )
                CALL LCPRSV ( -C3*DP, X3   , VTMP )
                CALL LCSOVE ( VTMP  , DX3  , DX3  )
C - DX4
                CALL LCPRSV ( H4*DP , DFDS , DX4  )
                CALL LCPRSV ( -C4*DP, X4   , VTMP )
                CALL LCSOVE ( VTMP  , DX4  , DX4  )

C - DX5
                CALL LCPRSV ( H5*DP , DFDS , DX5  )
                CALL LCPRSV ( -C5*DP, X5   , VTMP )
                CALL LCSOVE ( VTMP  , DX5  , DX5  )

C - DSIG
                CALL LCPRSV ( -DP   , DFDS , VTMP )
                CALL LCSOVE ( DEPS  , VTMP , VTMP )
                CALL LCPRMV ( HOOK  , VTMP , DSIG )
C - DY
                CALL LCEQVN ( NDT , DSIG   , DY(1) )
                CALL LCEQVN ( NDT , DX1    , DY(NDT+1))
                CALL LCEQVN ( NDT , DX2    , DY(2*NDT+1))
                CALL LCEQVN ( NDT , DX3    , DY(3*NDT+1))
                CALL LCEQVN ( NDT , DX4    , DY(4*NDT+1))
                CALL LCEQVN ( NDT , DX5    , DY(5*NDT+1))
                DY(6*NDT+1) = DP
C
                        IF(MOD(1:6).EQ.'C_PLAN')THEN
                        DY(6*NDT+2) = DEPS(3)
                        DY(3)       = ZERO
                        ENDIF
C
C - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
C
                ELSEIF ( TYPESS .EQ. 3 ) THEN
                CALL LCINVN ( 6*NDT+2  , ESSAI , DY )
                        IF ( MOD(1:6).EQ.'C_PLAN' )THEN
                        DEPS(3) = ESSAI
                        DY(3)   = ZERO
                        ENDIF
                ENDIF
        END
