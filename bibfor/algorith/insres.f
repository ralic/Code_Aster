        SUBROUTINE INSRES ( MOD, NMAT, MATERD, MATERF,
     1                      YD ,  YF,   DEPS,   DY,     R )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ============================================================
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
C       ------------------------------------------------------------
C    NADAI_BETON   : CALCUL DES TERMES DU SYSTEME NL A RESOUDRE R(DY)
C                    DY = ( DSIG   DP   (DEPS3) )
C                    Y  = ( SIG    P    (EPS3)  )
C                    R  = ( G      F    (Q)     )
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YD     :  VARIABLES A T       = ( SIGD VIND (EPSD3)  )
C           YF     :  VARIABLES A T + DT  = ( SIGF VINF (EPSF3)  )
C           DY     :  SOLUTION ESSAI      = ( DSIG DVIN (DEPS3) )
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT R      :  SYSTEME NL A T + DT
C       -------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT
C
        REAL*8          HOOKF(6,6), DKOOH(6,6)
        REAL*8          SIGF(6)   , DSIG(6) ,    SIGD(6) ,  DFDS(6)
        REAL*8          DEPS(6)   , DEPSP(6),    DEPSE(6)
        REAL*8          EPSED(6)  , EPSEF(6),    DEV(6)
        REAL*8          KAPA      , DP , KRUP
        REAL*8          GF(6)     , FF      , QF    , KF
        REAL*8          R(*)      , DY(*)   , YD(*) , YF(*)
C
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          SEUIL , LCS, KPIC, R0
        REAL*8          I1, J2, J3, RCOS3T, HP, TAU , RAC2
C
        CHARACTER*8     MOD
C       ------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ------------------------------------------------------------
C
        LCS     = MATERF(1,2)
        R0      = MATERF(3,2)
        KPIC    = MATERF(4,2)
        KRUP    = MATERF(5,2)
        KAPA    = YF(NDT+1)
C
        CALL LCEQVN ( NDT , YF       , SIGF)
        CALL LCEQVN ( NDT , YD       , SIGD)
        CALL LCEQVN ( NDT , DY       , DSIG )
        CALL INSINV ( SIGF , I1, J2, J3, RCOS3T )
        CALL INSISO (LCS, KPIC, R0, KRUP, KAPA, HP, TAU)
        DP = DY(NDT+1)
C
        CALL INSFS (SIGF , DFDS  , DEV )
C
C                   -1                     -1
C -     HOOKF, HOOKD , DFDS , EPSEF = HOOKD  SIGD + DEPS - DEPSP
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
        CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
        CALL LCPRSV ( DP    ,   DFDS  , DEPSP )
        CALL LCPRMV ( DKOOH ,   SIGD  , EPSED )
        CALL LCDIVE ( DEPS  ,   DEPSP , DEPSE )
        CALL LCSOVE ( EPSED ,   DEPSE , EPSEF )
C
C - GF  (T+DT)
C
        CALL LCPRMV  ( HOOKF , EPSEF , GF   )
        CALL LCDIVE  ( GF    , SIGF  , GF   )
C
C - FF (T+DT)
C
        CALL INSNAD ( I1 , J2 , TAU , SEUIL )
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
        CALL LCEQVN ( NDT , GF     , R(1) )
        R(NDT+1) = FF
        IF ( MOD(1:6).EQ.'C_PLAN' ) R(NDT+2) = QF
C
      END
