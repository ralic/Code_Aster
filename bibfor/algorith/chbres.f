        SUBROUTINE CHBRES ( MOD, IMAT, NMAT, MATERD, MATERF, MATCST,
     1                      TEMPF, YD ,  YF,   DEPS,   DY,     R )
        IMPLICIT REAL*8 (A-H,O-Z)

C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       CHABOCHE   : CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = R(DY)
C                    DY = ( DSIG DX1 DX2 DP (DEPS3) )
C                    Y  = ( SIG  X1  X2  P  (EPS3)  )
C                    R  = ( G    L   J   F  (Q)     )
C       IN  MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           MATCST :  'OUI' SI MATERIAU CONSTANT SUR DT
C           TEMPF  :  TEMPERATURE A T + DT
C           YD     :  VARIABLES A T       = ( SIGD VIND (EPSD3)  )
C           YF     :  VARIABLES A T + DT  = ( SIGF VINF (EPSF3)  )
C           DY     :  SOLUTION ESSAI      = ( DSIG DVIN (DEPS3) )
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT R      :  SYSTEME NL A T + DT
C       ----------------------------------------------------------------
        INTEGER         IMAT, NDT , NDI , NMAT
        REAL*8          D23
        PARAMETER       ( D23  =  .66666666666666D0 )
C
        REAL*8          HOOKF(6,6), DKOOH(6,6)
        REAL*8          SIGF(6)   , DSIG(6) ,    SIGD(6) ,    DFDS(6)
        REAL*8          DEPS(6)   , DEPSP(6),    DEPSE(6)
        REAL*8          EPSED(6)  , EPSEF(6)
        REAL*8          X1(6)     , DX1(6)
        REAL*8          X2(6)     , DX2(6)
        REAL*8          P         , DP
        REAL*8          GF(6)     , LF(6),     JF(6),   FF,   QF
        REAL*8          R(*)      , DY(*),     YD(*),   YF(*)
        REAL*8          VTMP1(6)  , VTMP2(6)
        REAL*8          H1,  H2   , CHBCIN
C
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          SEUIL , TEMPF
        REAL*8          RI,  RO,  B,  K,  W,  A1,  A2,  C1,  C2, NU
C
        CHARACTER*8     MOD
        CHARACTER*3     MATCST
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        CALL LCEQVN ( NDT , YF(1)       , SIGF)
        CALL LCEQVN ( NDT , YD(1)       , SIGD)
        CALL LCEQVN ( NDT , YF(NDT+1)   , X1  )
        CALL LCEQVN ( NDT , YF(2*NDT+1) , X2  )
        P  = YF(3*NDT+1)
        CALL LCEQVN ( NDT , DY(1)      , DSIG )
        CALL LCEQVN ( NDT , DY(NDT+1)  , DX1  )
        CALL LCEQVN ( NDT , DY(2*NDT+1), DX2  )
        DP = DY(3*NDT+1)
C
        K  = MATERF(4,2)
        W  = MATERF(5,2)
        A1 = MATERF(6,2)
        A2 = MATERF(7,2)
        C1 = MATERF(8,2)
        C2 = MATERF(9,2)
C
C                   -1                     -1
C -     HOOKF, HOOKD , DFDS , EPSEF = HOOKD  SIGD + DEPS - DEPSP
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
        CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
C
        CALL CHBFS   ( SIGF, X1,   X2, DFDS )
        H1 = -C1 * A1 * CHBCIN(K,W,P) * D23
        H2 = -C2 * A2 * CHBCIN(K,W,P) * D23
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
        CALL LCPRSV ( -H2*DP , DFDS  , VTMP1 )
        CALL LCSOVE (  JF    , VTMP1 , JF )
        CALL LCDIVE (  JF    , DX2   , JF )
C
C - FF (T+DT)
C
        CALL CHBCVX ( IMAT, NMAT, MATERF, TEMPF, SIGF , YF(NDT+1),SEUIL)
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
        R(3*NDT+1) = FF
          IF ( MOD(1:6).EQ.'C_PLAN' )R(3*NDT+2) = QF
        END
