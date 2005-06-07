      SUBROUTINE CHBINI ( TYPESS, ESSAI, MOD, NMAT,
     &                      MATERF, YD,  DEPS, DY  )
      IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C       CHABOCHE : CALCUL SOLUTION ESSAI DY = ( DSIG DX1 DX2 DP (DEPS3))
C                               AVEC     Y  = ( SIG  X1  X2  P  (EPS3))
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
        REAL*8          UN  , D23 , D32 , ZERO
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
        REAL*8          P         , DP
        REAL*8          VTMP(6)   , VTMP1(6), VTMP2(6)
        REAL*8          XX,  YY, ZZ, S
        REAL*8          RI,  RO, B,  K, W, A1, A2, C1, C2
        REAL*8          H1,  H2, NUN
C
        REAL*8          MATERF(NMAT,2)
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
        REAL*8          CHBCIN
C       ----------------------------------------------------------------
C
        IF ( TYPESS .EQ. -1 ) TYPESS = 2
C
C
        CALL LCEQVN ( NDT , YD(1)       , SIG )
        CALL LCEQVN ( NDT , YD(NDT+1)   , X1  )
        CALL LCEQVN ( NDT , YD(2*NDT+1) , X2  )
        P = YD(3*NDT+1)
C
        RI = MATERF(1,2)
        RO = MATERF(2,2)
        B  = MATERF(3,2)
        K  = MATERF(4,2)
        W  = MATERF(5,2)
        A1 = MATERF(6,2)
        A2 = MATERF(7,2)
        C1 = MATERF(8,2)
        C2 = MATERF(9,2)
        NUN = MATERF(2,1) / (UN-MATERF(2,1))
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
C
C - SOLUTION INITIALE = NUL
C
                IF ( TYPESS .EQ. 0 .OR. TYPESS .EQ. 1 ) THEN
                CALL LCTRMA (HOOK , HOOK)
                CALL LCINVN( 3*NDT+2 , ZERO , DY )
                  IF(MOD(1:6).EQ.'C_PLAN')THEN
                  DEPS(3) = ZERO
                  ENDIF
C
C - SOLUTION INITIALE = EXPLICITE
C
                ELSEIF ( TYPESS .EQ. 2 ) THEN
                H1 = C1 * A1 * CHBCIN(K,W,P) * D23
                H2 = C2 * A2 * CHBCIN(K,W,P) * D23
C - S
                CALL LCDEVI ( SIG   , VTMP)
                CALL LCDIVE ( VTMP  , X1 , VTMP)
                CALL LCDIVE ( VTMP  , X2 , VTMP)
                CALL LCNRVE ( VTMP  , S )
                S =  SQRT ( 1.5D0 ) * S
C - DP
                        IF ( S .EQ. ZERO)THEN
                        DP   = ZERO
                        DO 1, I=1,6
                          DFDS(I) = ZERO
 1                      CONTINUE
                        ELSE
                        CALL CHBFS  ( SIG   , X1,    X2, DFDS )
                        ZZ = ( B * (RI - RO) * S ) / D32 * EXP(B*P)
                        CALL LCPRSV ( -C1   , X1   , VTMP1 )
                        CALL LCPRSV ( -C2   , X2   , VTMP2 )
                        CALL LCSOVE ( VTMP1 , VTMP2, VTMP2 )
                        CALL LCPRSV ( H1+H2 , DFDS , VTMP1 )
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
C - DSIG
                CALL LCPRSV ( -DP   , DFDS , VTMP )
                CALL LCSOVE ( DEPS  , VTMP , VTMP )
                CALL LCPRMV ( HOOK  , VTMP , DSIG )
C - DY
                CALL LCEQVN ( NDT , DSIG   , DY(1) )
                CALL LCEQVN ( NDT , DX1    , DY(NDT+1) )
                CALL LCEQVN ( NDT , DX2    , DY(2*NDT+1) )
                DY(3*NDT+1) = DP
                        IF(MOD(1:6).EQ.'C_PLAN')THEN
                        DY(3*NDT+2) = DEPS(3)
                        DY(3)       = ZERO
                        ENDIF
C
C - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
C
                ELSEIF ( TYPESS .EQ. 3 ) THEN
                CALL LCINVN ( 3*NDT+2  , ESSAI , DY )
                        IF ( MOD(1:6).EQ.'C_PLAN' )THEN
                        DEPS(3) = ESSAI
                        DY(3)   = ZERO
                        ENDIF
                ENDIF
C
        END
