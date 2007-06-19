        SUBROUTINE CVMJAC ( MOD, NMAT, MATERF, TIMED,
     1                      TIMEF, YF, DY, NMOD, EPSD, DEPS,DRDY )
        IMPLICIT NONE
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
C TOLE CRP_20
C       ----------------------------------------------------------------
C       VISCOCHABOCHE   :
C           CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C                   DY  = ( DSIG DX1 DX2 DP DR 0 (DEPS3))
C                   Y   = ( SIG  X1  X2  P  R  Q (EPS3) )
C
C       DRDY  = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  0   (DGDE3) )
C               ( DLDS  DLDX1  DLDX2  DLDP  DLDR  0   (DLDE3) )
C               ( DJDS  DJDX1  DJDX2  DJDP  DJDR  0   (DJDE3) )
C               ( DKDS  DKDX1  DKDX2  DKDP  DKDR  0   (DKDE3) )
C               ( DRDS  DRDX1  DRDX2  DRDP  DRDR  0   (DRDE3) )
C               ( 0     0      0      0     0     1   (0)     )
C               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(0)  (DQDE3) )
C                                                     ( SI IOPTIO = 0 )
C
C                   DY  = ( DSIG DX1 DX2 DP DR DQ DXXI (DEPS3))
C                   Y   = ( SIG  X1  X2  P  R  Q  XXI  (EPS3) )
C
C       DRDY  = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  DGDQ  DGDXXI (DGDE3) )
C               ( DLDS  DLDX1  DLDX2  DLDP  DLDR  DLDQ  DLDXXI (DLDE3) )
C               ( DJDS  DJDX1  DJDX2  DJDP  DJDR  DJDQ  DJDXXI (DJDE3) )
C               ( DKDS  DKDX1  DKDX2  DKDP  DKDR  DKDQ  DKDXXI (DKDE3) )
C               ( DRDS  DRDX1  DRDX2  DRDP  DRDR  DRDQ  DRDXXI (DRDE3) )
C               ( DTDS  DTDX1  DTDX2  DTDP  DTDR  DTDQ  DTDXXI (DTDE3) )
C               ( DXIDS DXIDX1 DXIDX2 DXIDP DXIDR DXIDQ DXIDXI(DXIDE3))
C               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(DQDQ)(DQDXXI)(DQDE3) )
C                                                     ( SI IOPTIO = 2 )
C
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YF     :  VARIABLES A T + DT = ( SIGF  X1F X2F PF RF QF(..))
C           DY     :  SOLUTION ESSAI     = ( DSIG  DX1 DX2 DP DR DQ(..))
C           NMOD   :  DIMENSION DECLAREE DRDY
C           EPSD   :  DEFORMATION A T
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       ----------------------------------------------------------------
C
        INTEGER         NDT , NDI , NMAT , NMOD
        INTEGER         IOPTIO   , IDNR , NOPT
C
        REAL*8          UN  , ZERO , D23 , D13, MUN
        PARAMETER       ( UN   =  1.D0      )
        PARAMETER       ( MUN  = -1.D0      )
        PARAMETER       ( ZERO =  0.D0      )
        PARAMETER       ( D23  =  2.D0/3.D0 )
        PARAMETER       ( D13  = -1.D0/3.D0 )
C
        REAL*8          HOOK(6,6) , DDFDDS(6,6) , DDFDSX(6,6) , I6(6,6)
        REAL*8          DEPS(6)   , EPSD(6)     , FKOOH(6,6)  , ID(6,6)
        REAL*8          DFDS(6)
        REAL*8          SIG(6)    , DSIG(6)
        REAL*8          X1(6)     , DX1(6)  , X2(6)  , DX2(6)
        REAL*8          XXI(6)    , DXXI(6) , P      , DP
        REAL*8          R         , Q
        REAL*8          YF(*)     , DY(*)   , DRDY(NMOD,NMOD)
C
        REAL*8          DGDS(6,6),  DGDX1(6,6),  DGDX2(6,6),  DGDR(6)
        REAL*8          DLDS(6,6),  DLDX1(6,6),  DLDX2(6,6),  DLDR(6)
        REAL*8          DJDS(6,6),  DJDX1(6,6),  DJDX2(6,6),  DJDR(6)
        REAL*8          DKDS(6),    DKDX1(6),    DKDX2(6),    DKDR
        REAL*8          DRDS(6),    DRDX1(6),    DRDX2(6),    DRDR
        REAL*8          DTDS(6),    DTDX1(6),    DTDX2(6),    DTDR
        REAL*8          DXIDS(6,6), DXIDX1(6,6), DXIDX2(6,6), DXIDR(6)
        REAL*8          DQDS(6),    DQDX1(6),    DQDX2(6),    DQDR
C
        REAL*8          DGDQ(6),  DGDP(6),  DGDXXI(6,6),   DGDE3(6)
        REAL*8          DLDQ(6),  DLDP(6),  DLDXXI(6,6),   DLDE3(6)
        REAL*8          DJDQ(6),  DJDP(6),  DJDXXI(6,6),   DJDE3(6)
        REAL*8          DKDQ,     DKDP   ,  DKDXXI(6),     DKDE3
        REAL*8          DRDQ,     DRDP   ,  DRDXXI(6),     DRDE3
        REAL*8          DTDQ,     DTDP   ,  DTDXXI(6),     DTDE3
        REAL*8          DXIDQ(6), DXIDP(6), DXIDXI(6,6),  DXIDE3(6)
        REAL*8          DQDQ,     DQDP   ,  DQDXXI(6),     DQDE3
C
        REAL*8          MATERF(NMAT,2), SEUIL , SEUIL2
        REAL*8          TIMED, TIMEF, DT
C
        REAL*8          K0  , AK , AR  , N   , ALP  , WW
        REAL*8          B    , MR , GR  , MU   , QM , Q0
        REAL*8          QR0 , ETA , AI
        REAL*8          M1  , D1  , GX1 , G10 , C1
        REAL*8          M2  , D2  , GX2 , G20 , C2
        REAL*8          CCIN , XX  , YY   , ZZ
        REAL*8          GRQ  , QR
        REAL*8          C1D , C2D , DIFC1  , DIFC2
        REAL*8          VTMP(6)  , VTMP1(6) , EPSP(6)  , EPXI(6)
        REAL*8          DEDE3(6) , MTMP(6,6), MTMP1(6,6)
        REAL*8          X1DF , JX1 , X2DF , JX2  , DCIN
        REAL*8          JEPXI, EPXINO(6), NNET
        INTEGER         N1   , N2  , N3   , N4   , N5  , N6,  N7,  N8
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
        COMMON /OPTI/   IOPTIO , IDNR
        COMMON /COED/   C1D , C2D
C       ----------------------------------------------------------------
        DATA DEDE3      /ZERO   , ZERO  , MUN   , ZERO , ZERO , ZERO/
        DATA  I6        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
        DATA ID         / D23   , D13   , D13   , ZERO , ZERO , ZERO ,
     &                    D13   , D23   , D13   , ZERO , ZERO , ZERO ,
     &                    D13   , D13   , D23   , ZERO , ZERO , ZERO ,
     &                    ZERO  , ZERO  , ZERO  , UN   , ZERO , ZERO ,
     &                    ZERO  , ZERO  , ZERO  , ZERO , UN   , ZERO ,
     &                    ZERO  , ZERO  , ZERO  , ZERO , ZERO , UN /
C
        CALL LCEQVN ( NDT , YF(1)       , SIG )
        CALL LCEQVN ( NDT , YF(NDT+1)   , X1  )
        CALL LCEQVN ( NDT , YF(2*NDT+1) , X2  )
        P  = YF(3*NDT+1)
        R  = YF(3*NDT+2)
        Q  = YF(3*NDT+3)
        CALL LCEQVN ( NDT , DY(1)       , DSIG)
        CALL LCEQVN ( NDT , DY(NDT+1)   , DX1 )
        CALL LCEQVN ( NDT , DY(2*NDT+1) , DX2 )
        DP = DY(3*NDT+1)
C
        K0     = MATERF(1,2)
        AK     = MATERF(2,2)
        AR     = MATERF(3,2)
        N       = MATERF(5,2)
        ALP     = MATERF(6,2)
        B       = MATERF(7,2)
        MR     = MATERF(8,2)
        GR     = MATERF(9,2)
        MU      = MATERF(10,2)
        QM     = MATERF(11,2)
        Q0     = MATERF(12,2)
        QR0    = MATERF(13,2)
        ETA     = MATERF(14,2)
        C1      = MATERF(15,2)
        M1     = MATERF(16,2)
        D1      = MATERF(17,2)
        GX1    = MATERF(18,2)
        G10    = MATERF(19,2)
        C2      = MATERF(20,2)
        M2     = MATERF(21,2)
        D2      = MATERF(22,2)
        GX2    = MATERF(23,2)
        G20    = MATERF(24,2)
        AI     = MATERF(25,2)
C
        NOPT = 0
        IF ( IOPTIO .EQ. 2 ) NOPT = IDNR
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
        CALL CHBFS   ( SIG, X1, X2, DFDS )
        CALL CHBFSS  ( SIG, X1, X2, ID, DDFDDS )
        CALL CHBFSX  ( SIG, X1, X2, I6, DDFDSX )
C
        CALL CVMCVX ( NMAT, MATERF,SIG,  YF(NDT+1), SEUIL)
        IF ( SEUIL .LT. 0.D0 ) SEUIL = 0.D0
        CCIN = AI + (1.D0-AI) * EXP( -B*P )
        DCIN = B * (AI-1.D0) * EXP( -B*P )
        DT    = TIMEF - TIMED
C
C       ----------------------------------------------------------------
C       CALCUL DU JACOBIEN DU SYSTEME ( SIG  X1  X2  P  R  (EPS3) )
C       ----------------------------------------------------------------
C
C - DGDS(T+DT)
        CALL LCPRMM ( HOOK    , DDFDDS , DGDS  )
        CALL LCPRSM ( DP      , DGDS   , DGDS  )
        CALL LCSOMA ( I6      , DGDS   , DGDS  )
C
C - DGDX1(T+DT)
        CALL LCPRMM ( HOOK    , DDFDSX , DGDX1 )
        CALL LCPRSM ( DP      , DGDX1  , DGDX1 )
C
C - DGDX2(T+DT)
        CALL LCEQMA ( DGDX1   , DGDX2          )
C
C - DGDP(T+DT)
        IF ( SEUIL .LT. 0.D0 ) THEN
            CALL LCINVE ( 0.D0    , DGDP           )
        ELSE
            CALL LCPRMV ( HOOK    , DFDS   , DGDP  )
        ENDIF
C
C - DGDR(T+DT)
        CALL LCINVE ( 0.D0    , DGDR           )
C
C - DGDQ(T+DT)
        CALL LCINVE ( 0.D0    , DGDQ           )
C
C
C - DLDS(T+DT)
        CALL LCPRSC ( X1      , X1      , JX1  )
        JX1 = SQRT( JX1 * 3.D0/2.D0)
        XX = C1 * DP * 2.D0/3.D0
        YY = G10 * CCIN * (1.D0 - D1) * DP * 2.D0/3.D0
        IF ( JX1 .LE. 0.D0 ) THEN
          ZZ= 1.D0 + DP * G10 * CCIN * D1
          WW=0.D0
        ELSE
          ZZ = 1.D0 + DP * G10 * CCIN * D1
     1     + DT * GX1 * JX1**(M1-1.D0)
          WW = GX1 * DT * (M1-1.D0) * JX1**(M1-3.D0) * 3.D0/2.D0
        ENDIF
        CALL LCPRMV ( DDFDDS  , X1     , VTMP  )
        CALL LCPRTE ( VTMP    , DFDS   , MTMP  )
        CALL LCPRSC ( X1      , DFDS   , X1DF  )
        CALL LCPRSM ( X1DF    , DDFDDS , MTMP1 )
        CALL LCSOMA ( MTMP    , MTMP1  , MTMP  )
        CALL LCPRSM ( YY      , MTMP   , DLDS  )
        CALL LCPRSM ( XX      , DDFDDS , MTMP  )
        CALL LCDIMA ( DLDS    , MTMP   , DLDS  )
C
C - DLDX1(T+DT)
        CALL LCPRMV ( DDFDSX  , X1     , VTMP  )
        CALL LCPRTE ( VTMP    , DFDS   , MTMP1 )
        CALL LCPRSM ( X1DF    , DDFDSX , MTMP  )
        CALL LCSOMA ( MTMP    , MTMP1  , MTMP1 )
        CALL LCPRTE ( DFDS    , DFDS   , MTMP  )
        CALL LCSOMA ( MTMP    , MTMP1  , MTMP  )
        CALL LCPRSM ( YY      , MTMP   , DLDX1 )
        CALL LCPRTE ( X1      , X1     , MTMP  )
        CALL LCPRSM ( WW      , MTMP   , MTMP  )
        CALL LCSOMA ( DLDX1   , MTMP   , DLDX1 )
        CALL LCPRSM ( ZZ      , I6     , MTMP  )
        CALL LCSOMA ( DLDX1   , MTMP   , DLDX1 )
        CALL LCPRSM ( XX      , DDFDSX , MTMP  )
        CALL LCDIMA ( DLDX1   , MTMP   , DLDX1 )
C
C - DLDX2(T+DT)
        CALL LCPRSM ( YY      , MTMP1  , DLDX2 )
        CALL LCDIMA ( DLDX2   , MTMP   , DLDX2 )
C
C -- CAS ANISOTHERME
        IF (C1 .NE. 0.D0) THEN
          DIFC1 = (C1-C1D)/C1
          CALL LCPRSM ( DIFC1  , I6  , MTMP )
          CALL LCDIMA ( DLDX1   , MTMP   , DLDX1 )
        ENDIF
        IF (C2 .NE. 0.D0) THEN
          DIFC2 = (C2-C2D)/C2
          CALL LCPRSM ( DIFC2  , I6  , MTMP )
          CALL LCDIMA ( DLDX2   , MTMP   , DLDX2 )
        ENDIF
C
C - DLDP(T+DT)
        YY = G10 * ( CCIN + DCIN * DP ) * D1
        ZZ = G10 * ( CCIN + DCIN * DP ) * (1.D0 - D1) * 2.D0/3.D0
        XX =  X1DF * ZZ - C1 * 2.D0/3.D0
        CALL LCPRSV ( XX      , DFDS   , VTMP  )
        CALL LCPRSV ( YY      , X1     , DLDP  )
        CALL LCSOVE ( DLDP    , VTMP   , DLDP  )
C
C - DLDR(T+DT)
        CALL LCINVE ( 0.D0    , DLDR           )
C
C - DLDQ(T+DT)
        CALL LCINVE ( 0.D0    , DLDQ           )
C
C
C - DJDS(T+DT)
        CALL LCPRSC ( X2      , X2      , JX2  )
        JX2 = SQRT( JX2 * 3.D0/2.D0)
        XX = C2 * DP * 2.D0/3.D0
        YY = G20 * CCIN * (1.D0 - D2) * DP * 2.D0/3.D0
        IF ( JX2 .LE. 0.D0 ) THEN
          ZZ= 1.D0 + DP * G20 * CCIN * D2
          WW=0.D0
        ELSE
          ZZ = 1.D0 + DP * G20 * CCIN * D2
     1     + DT * GX2 * JX2**(M2-1.D0)
          WW = GX2 * DT * (M2-1.D0) * JX2**(M2-3.D0) * 3.D0/2.D0
        ENDIF
        CALL LCPRMV ( DDFDDS  , X2     , VTMP  )
        CALL LCPRTE ( VTMP    , DFDS   , MTMP  )
        CALL LCPRSC ( X2      , DFDS   , X2DF  )
        CALL LCPRSM ( X2DF    , DDFDDS , MTMP1 )
        CALL LCSOMA ( MTMP    , MTMP1  , MTMP  )
        CALL LCPRSM ( YY      , MTMP   , DJDS  )
        CALL LCPRSM ( XX      , DDFDDS , MTMP  )
        CALL LCDIMA ( DJDS    , MTMP   , DJDS  )
C
C - DJDX2(T+DT)
        CALL LCPRMV ( DDFDSX  , X2     , VTMP  )
        CALL LCPRTE ( VTMP    , DFDS   , MTMP1 )
        CALL LCPRSM ( X2DF    , DDFDSX , MTMP  )
        CALL LCSOMA ( MTMP    , MTMP1  , MTMP1 )
        CALL LCPRTE ( DFDS    , DFDS   , MTMP  )
        CALL LCSOMA ( MTMP    , MTMP1  , MTMP  )
        CALL LCPRSM ( YY      , MTMP   , DJDX2 )
        CALL LCPRTE ( X2      , X2     , MTMP  )
        CALL LCPRSM ( WW      , MTMP   , MTMP  )
        CALL LCSOMA ( DJDX2   , MTMP   , DJDX2 )
        CALL LCPRSM ( ZZ      , I6     , MTMP  )
        CALL LCSOMA ( DJDX2   , MTMP   , DJDX2 )
        CALL LCPRSM ( XX      , DDFDSX , MTMP  )
        CALL LCDIMA ( DJDX2   , MTMP   , DJDX2 )
C
C - DJDX1(T+DT)
        CALL LCPRSM ( YY      , MTMP1  , DJDX1 )
        CALL LCDIMA ( DJDX1   , MTMP   , DJDX1 )
C
C - DJDP(T+DT)
        YY = G20 * ( CCIN + DCIN * DP ) * D2
        ZZ = G20 * ( CCIN + DCIN * DP ) * (1.D0 - D2) * 2.D0/3.D0
        XX = X2DF * ZZ - C2  * 2.D0/3.D0
        CALL LCPRSV ( XX      , DFDS   , VTMP  )
        CALL LCPRSV ( YY      , X2     , DJDP  )
        CALL LCSOVE ( DJDP    , VTMP   , DJDP  )
C
C - DJDR(T+DT)
        CALL LCINVE ( 0.D0    , DJDR           )
C
C - DJDQ(T+DT)
        CALL LCINVE ( 0.D0    , DJDQ           )
C
C
C - DKDS(T+DT)
        XX = SEUIL / ( K0 + AK * R)
        IF ( XX .LT. 0.D0 ) XX = 0.D0
        ZZ = DT * ( (XX**(N-1.D0)) * (N + ALP*(N+1)*XX**(N+1)) )
     1     * EXP( ALP*(XX**(N+1)) ) / ( K0 + AK * R)
        CALL LCPRSV ( -ZZ     , DFDS   , DKDS  )
C
C - DKDX1(T+DT)
        CALL LCPRSV (  ZZ     , DFDS   , DKDX1  )
C
C - DKDX2(T+DT)
        CALL LCEQVE (  DKDX1  , DKDX2           )
C
C - DKDP(T+DT)
        DKDP = 1.D0
C
C - DKDR(T+DT)
C       DKDR = ZZ * (AR * K0 + SEUIL - AK * K) / (K0 + AK * R)
        DKDR = ZZ * (AR * (K0 + AK * R) + AK * SEUIL )
     1       / (K0 + AK * R)
C
C - DKDQ(T+DT)
        DKDQ = 0.D0
C
C
C - DRDS(T+DT)
        CALL LCINVE ( 0.D0    , DRDS           )
C
C - DRDX1(T+DT)
        CALL LCINVE ( 0.D0    , DRDX1          )
C
C - DRDX2(T+DT)
        CALL LCINVE ( 0.D0    , DRDX2          )
C
C - DRDP(T+DT)
        GRQ = Q0 + ( QM - Q0 ) * ( 1.D0 - EXP(-2.D0*MU*Q) )
        QR  = GRQ - QR0 * (1.D0 - ((QM-GRQ)/QM)**2)
        DRDP = B * ( R - GRQ )
C
C - DRDR(T+DT)
        DRDR = 1.D0 + B*DP + GR*DT*MR
     1       * (ABS(QR - R))**(MR-1.D0)
C
C - DRDQ(T+DT)
        DRDQ = 0.D0
C
C
C - DTDS(T+DT)
        CALL LCINVE ( 0.D0    , DTDS           )
C
C - DTDX1(T+DT)
        CALL LCINVE ( 0.D0    , DTDX1          )
C
C - DTDX2(T+DT)
        CALL LCINVE ( 0.D0    , DTDX2          )
C
C - DTDP(T+DT)
        DTDP = 0.D0
C
C - DTDR(T+DT)
        DTDR = 0.D0
C
C - DTDQ(T+DT)
        DTDQ = 1.D0
C
C - CONTRAINTES PLANES -------------------------------------------------
C
        IF( MOD(1:6).EQ.'C_PLAN' ) THEN
C
C - DGDE3(T+DT)
           CALL LCPRMV ( HOOK    , DEDE3   , DGDE3 )
C
C - DLDE3(T+DT)
           CALL LCINVE ( 0.D0    , DLDE3           )
C
C - DJDE3(T+DT)
           CALL LCINVE ( 0.D0    , DJDE3           )
C
C - DKDE3(T+DT)
           DKDE3   = 0.D0
C
C - DRDE3(T+DT)
           DRDE3   = 0.D0
C
C - DTDE3(T+DT)
           DTDE3   = 0.D0
C
C - DQDE3(T+DT)
           DQDE3   = HOOK(3,3)
C
C - DQDS (T+DT)
          DQDS(1) = - DP*(HOOK(3,3)*DDFDDS(3,1) + HOOK(3,1)*DDFDDS(1,1)+
     &                    HOOK(3,2)*DDFDDS(2,1) + HOOK(3,4)*DDFDDS(4,1))
          DQDS(2) = - DP*(HOOK(3,3)*DDFDDS(3,2) + HOOK(3,1)*DDFDDS(1,2)+
     &                    HOOK(3,2)*DDFDDS(2,2) + HOOK(3,4)*DDFDDS(4,2))
          DQDS(3) = - DP*(HOOK(3,3)*DDFDDS(3,3) + HOOK(3,1)*DDFDDS(1,3)+
     &                    HOOK(3,2)*DDFDDS(2,3) + HOOK(3,4)*DDFDDS(4,3))
          DQDS(4) = - DP*(HOOK(3,3)*DDFDDS(3,4) + HOOK(3,1)*DDFDDS(1,4)+
     &                    HOOK(3,2)*DDFDDS(2,4) + HOOK(3,4)*DDFDDS(4,4))
C
C - DQDX1 (T+DT)
          DQDX1(1)= - DP*(HOOK(3,3)*DDFDSX(3,1) + HOOK(3,1)*DDFDSX(1,1)+
     &                    HOOK(3,2)*DDFDSX(2,1) + HOOK(3,4)*DDFDSX(4,1))
          DQDX1(2)= - DP*(HOOK(3,3)*DDFDSX(3,2) + HOOK(3,1)*DDFDSX(1,2)+
     &                    HOOK(3,2)*DDFDSX(2,2) + HOOK(3,4)*DDFDSX(4,2))
          DQDX1(3)= - DP*(HOOK(3,3)*DDFDSX(3,3) + HOOK(3,1)*DDFDSX(1,3)+
     &                    HOOK(3,2)*DDFDSX(2,3) + HOOK(3,4)*DDFDSX(4,3))
          DQDX1(4)= - DP*(HOOK(3,3)*DDFDSX(3,4) + HOOK(3,1)*DDFDSX(1,4)+
     &                    HOOK(3,2)*DDFDSX(2,4) + HOOK(3,4)*DDFDSX(4,4))
C
C - DQDX2 (T+DT)
           CALL LCEQVE ( DQDX1   , DQDX2          )
C
C - DQDP (T+DT)
           DQDP    = - HOOK(3,1)*DFDS(1) - HOOK(3,2)*DFDS(2)
     &               - HOOK(3,3)*DFDS(3) - HOOK(3,4)*DFDS(4)
C
C - DQDR (T+DT)
           DQDR    = 0.D0
C
C - DQDQ (T+DT)
           DQDQ    = 0.D0
C
        ENDIF
C
C
C - ASSEMBLAGE ---------------------------------------------------------
C
C - DRDY (T+DT) = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  0   (DGDE3) )
C                 ( DLDS  DLDX1  DLDX2  DLDP  DLDR  0   (DLDE3) )
C                 ( DJDS  DJDX1  DJDX2  DJDP  DJDR  0   (DJDE3) )
C                 ( DKDS  DKDX1  DKDX2  DKDP  DKDR  0   (DKDE3) )
C                 ( DRDS  DRDX1  DRDX2  DRDP  DRDR  0   (DRDE3) )
C                 ( 0     0      0      0     0     1   (0)     )
C                 ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(0)  (DQDE3) )
C
C
        N1 = 1
        N2 = NDT + 1
        N3 = 2*NDT + 1
        N4 = 3*NDT + 1
        N5 = 3*NDT + 2
        N6 = 3*NDT + 3
        N8 = 3*NDT + 4 + NOPT
C
        CALL LCICMA (DGDS , 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N1,N1     )
        CALL LCICMA (DGDX1, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N1,N2     )
        CALL LCICMA (DGDX2, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N1,N3     )
        CALL LCICMA (DGDP , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N1,N4     )
        CALL LCICMA (DGDR , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N1,N5     )
        CALL LCICMA (DGDQ , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N1,N6     )
C
        CALL LCICMA (DLDS , 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N2,N1     )
        CALL LCICMA (DLDX1, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N2,N2     )
        CALL LCICMA (DLDX2, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N2,N3     )
        CALL LCICMA (DLDP , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N2,N4     )
        CALL LCICMA (DLDR , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N2,N5     )
        CALL LCICMA (DLDQ , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N2,N6     )
C
        CALL LCICMA (DJDS , 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N3,N1     )
        CALL LCICMA (DJDX1, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N3,N2     )
        CALL LCICMA (DJDX2, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N3,N3     )
        CALL LCICMA (DJDP , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N3,N4     )
        CALL LCICMA (DJDR , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N3,N5     )
        CALL LCICMA (DJDQ , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N3,N6     )
C
        CALL LCICMA (DKDS , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N4,N1     )
        CALL LCICMA (DKDX1, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N4,N2     )
        CALL LCICMA (DKDX2, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N4,N3     )
        DRDY(N4,N4) = DKDP
        DRDY(N4,N5) = DKDR
        DRDY(N4,N6) = DKDQ
C
        CALL LCICMA (DRDS , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N5,N1     )
        CALL LCICMA (DRDX1, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N5,N2     )
        CALL LCICMA (DRDX2, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N5,N3     )
        DRDY(N5,N4) = DRDP
        DRDY(N5,N5) = DRDR
        DRDY(N5,N6) = DRDQ
C
        CALL LCICMA (DTDS , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N6,N1     )
        CALL LCICMA (DTDX1, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N6,N2     )
        CALL LCICMA (DTDX2, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N6,N3     )
        DRDY(N6,N4) = DTDP
        DRDY(N6,N5) = DTDR
        DRDY(N6,N6) = DTDQ
C
        IF( MOD(1:6).EQ.'C_PLAN' ) THEN
C
           CALL LCICMA (DGDE3, 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N1,N8  )
           CALL LCICMA (DLDE3, 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N2,N8  )
           CALL LCICMA (DJDE3, 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N3,N8  )
           DRDY(N4,N8) = DKDE3
           DRDY(N5,N8) = DRDE3
           DRDY(N6,N8) = DTDE3
C
           CALL LCICMA (DQDS , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N8,N1  )
           CALL LCICMA (DQDX1, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N8,N2  )
           CALL LCICMA (DQDX2, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N8,N3  )
           DRDY(N8,N4) = DQDP
           DRDY(N8,N5) = DQDR
           DRDY(N8,N6) = DQDQ
           DRDY(N8,N8) = DQDE3
        ENDIF
C
C       ----------------------------------------------------------------
C       CALCUL DU JACOBIEN DU SYSTEME (SIG  X1  X2  P  R  Q XXI (EPS3))
C       ----------------------------------------------------------------
C
        IF ( IOPTIO .EQ. 2 ) THEN
C
           CALL LCEQVN ( NDT     , YF(3*NDT+4) , XXI  )
           CALL LCEQVN ( NDT     , DY(3*NDT+4) , DXXI )
C
C - DGDQ(T+DT)
           CALL LCINVE ( 0.D0         , DGDQ          )
C
C - DLDQ(T+DT)
           CALL LCINVE ( 0.D0         , DLDQ          )
C
C - DJDQ(T+DT)
           CALL LCINVE ( 0.D0         , DJDQ          )
C
C - DKDQ(T+DT)
           DKDQ   = 0.D0
C
C - DRDQ(T+DT)
           XX = GR * MR * ((ABS(QR-R))**(MR-1.D0))
     1        * ( 1.D0 -2.D0 * (QM-GRQ) * QR0 / (QM**2) )
           DRDQ = (Q0-QM) *2.D0 * MU * EXP( -2.D0*MU*Q )
     1          * ( B * DP - XX * DT)
C
C - DTDQ(T+DT)
           DTDQ   = 1.D0
C
C
C - DGDXXI(T+DT)
           CALL LCINMA ( 0.D0         , DGDXXI          )
C
C - DLDXXI(T+DT)
           CALL LCINMA ( 0.D0         , DLDXXI          )
C
C - DJDXXI(T+DT)
           CALL LCINMA ( 0.D0         , DJDXXI          )
C
C - DKDXXI(T+DT)
           CALL LCINVE ( 0.D0         , DKDXXI          )
C
C - DRDXXI(T+DT)
           CALL LCINVE ( 0.D0         , DRDXXI          )
C
C
C
C ---- EPSP
           CALL LCOPIL ( 'ISOTROPE'   , MOD , MATERF(1,1) , FKOOH )
           CALL LCPRMV ( FKOOH        , SIG    , VTMP   )
           CALL LCSOVE ( EPSD         , DEPS   , EPSP   )
           CALL LCDIVE ( EPSP         , VTMP   , EPSP   )
C ---- JEPXI
           CALL LCDIVE ( EPSP         , XXI    , EPXI   )
           CALL LCPRSC ( EPXI         , EPXI   , XX     )
           JEPXI = SQRT( XX * 3.D0/2.D0 )
C
C --- H(F)=SEUIL2
C
C           SEUIL2 = 2.D0/3.D0 * JEPXI - Q
C --- NNET
C
           IF( JEPXI .EQ. 0.D0 ) THEN
             NNET = 0.D0
           ELSE
             CALL LCPRSV ( 1.D0/JEPXI , EPXI  , EPXINO  )
             CALL LCPRSC ( DFDS       , EPXINO  , NNET  )
           ENDIF
C
C -MEMORISATION
C
           IF (JEPXI .GT. 0.D0)  THEN
C
             ZZ = -ETA / JEPXI
             XX =  ZZ * DP
             CALL LCDIVE ( EPSP    , XXI   , VTMP    )
             CALL LCPRSV ( DP      , DFDS  , VTMP1   )
             CALL LCSOVE ( VTMP1   , VTMP  , VTMP    )
             YY = -DP * NNET * (3.D0/2.D0) / JEPXI
             CALL LCPRSV ( YY      , EPXI  , VTMP1   )
             CALL LCSOVE ( VTMP1   , VTMP  , VTMP    )
             CALL LCPRSV ( XX      , VTMP  , VTMP1   )
C
C - DTDS(T+DT)
C
             CALL LCPRMV ( DDFDDS  , VTMP1, DTDS     )
C
C - DTDX1(T+DT)
             CALL LCPRMV ( DDFDSX  , VTMP1, DTDX1    )
C
C - DTDX2(T+DT)
             CALL LCEQVE ( DTDX1   , DTDX2           )
C
C - DTDP(T+DT)
C
             CALL LCPRSV ( ZZ      , VTMP , VTMP     )
             CALL LCPRSC ( DFDS    , VTMP , DTDP     )
C
C - DTDXXI(T+DT)
             YY = -NNET * (3.D0/2.D0) / JEPXI
             CALL LCPRSV ( YY            , EPXI   , VTMP   )
             CALL LCSOVE ( VTMP          , DFDS   , VTMP   )
             CALL LCPRSV ( -XX           , VTMP   , DTDXXI )
C
C
C - DXIDS(T+DT)
             ZZ = 3.D0/2.D0 * ( 1.D0 - ETA )
     1          * ( 1.D0 - DP * NNET / JEPXI * 3.D0 )
             XX = 3.D0/2.D0 * ( 1.D0 - ETA ) * DP / JEPXI
             CALL LCPRSV ( -ZZ*DP        , EPXI   , VTMP   )
             CALL LCPRSV ( -XX*DP        , DFDS   , VTMP1  )
             CALL LCSOVE ( VTMP          , VTMP1  , VTMP   )
             CALL LCPRMV ( DDFDDS        , VTMP   , VTMP1  )
             CALL LCPRTE ( VTMP1         , EPXI   , MTMP   )
             CALL LCPRSM ( -XX*DP*NNET   , DDFDDS , MTMP1  )
             CALL LCSOMA ( MTMP1         , MTMP   , DXIDS  )
C
C - DXIDX1(T+DT)
             CALL LCPRMV ( DDFDSX        , VTMP   , VTMP1  )
             CALL LCPRTE ( VTMP1         , EPXI   , MTMP   )
             CALL LCPRSM ( -XX*DP*NNET   , DDFDSX , MTMP1  )
             CALL LCSOMA ( MTMP1         , MTMP   , DXIDX1 )
C
C - DXIDX2(T+DT)
             CALL LCEQMA ( DXIDX1        , DXIDX2          )
C
C - DXIDP(T+DT)
             CALL LCPRSC ( DFDS         , DFDS   , YY     )
             CALL LCPRSV ( ZZ*NNET+XX*YY, EPXI   , VTMP   )
             CALL LCPRSV ( -XX*NNET     , DFDS   , VTMP1  )
             CALL LCDIVE ( VTMP1        , VTMP   , DXIDP  )
C
C - DXIDXI(T+DT)
             CALL LCPRSM ( 1.D0+XX*NNET , I6     , MTMP   )
             CALL LCPRSV ( 3.D0*XX*NNET , EPXI   , VTMP   )
             CALL LCPRSV ( XX           , DFDS   , VTMP1  )
             CALL LCDIVE ( VTMP1        , VTMP   , VTMP   )
             CALL LCPRTE ( VTMP         , EPXI   , MTMP1  )
             CALL LCSOMA ( MTMP1        , MTMP   , DXIDXI)
C
           ELSE
             CALL LCINMA ( 0.D0         , DGDXXI          )
C
C - DTDS(T+DT)
             CALL LCINVE ( 0.D0    , DTDS           )
C
C - DTDX1(T+DT)
             CALL LCINVE ( 0.D0    , DTDX1          )
C
C - DTDX2(T+DT)
             CALL LCINVE ( 0.D0    , DTDX2          )
C
C - DTDP(T+DT)
             DTDP = 0.D0
C
C - DTDXXI(T+DT)
             CALL LCINVE ( 0.D0 , DTDXXI        )
C
C - DXIDS(T+DT)
             CALL LCINMA ( 0.D0         , DXIDS          )
C
C - DXIDX1(T+DT)
             CALL LCINMA ( 0.D0         , DXIDX1         )
C
C - DXIDX2(T+DT)
             CALL LCINMA ( 0.D0         , DXIDX2         )
C
C - DXIDP(T+DT)
             CALL LCINVE ( 0.D0         , DXIDP          )
C
C - DXIDXI(T+DT)
             CALL LCINMA ( 0.D0         , DXIDXI         )
C
           ENDIF
C
C - DTDR(T+DT)
           DTDR = 0.D0
C
C - DTDQ(T+DT)
           DTDQ = 1.D0
C
C - DXIDR(T+DT)
           CALL LCINVE ( 0.D0         , DXIDR           )
C
C - DXIDQ(T+DT)
           CALL LCINVE ( 0.D0         , DXIDQ           )
C
C
           IF( MOD(1:6).EQ.'C_PLAN' ) THEN
C
C - DQDXXI(T+DT)
              CALL LCINVE ( 0.D0    , DQDXXI           )
C
C - DXIDE3(T+DT)
              CALL LCINVE ( 0.D0    , DXIDE3           )
C
C - DQDQ(T+DT)
              DQDQ   = 0.D0
C
C - DTDE3(T+DT)
              DTDE3   = 0.D0
           ENDIF
C
C
C - ASSEMBLAGE ---------------------------------------------------------
C
C       DRDY  = (                                 DGDQ  DGDXXI (DGDE3) )
C               (         (...)                   DLDQ  DLDXXI (DLDE3) )
C               (         (...)                   DJDQ  DJDXXI (DJDE3) )
C               (                                 DKDQ  DKDXXI (DKDE3) )
C               (                                 DRDQ  DRDXXI (DRDE3) )
C               ( DTDS  DTDX1  DTDX2  DTDP  DTDR  DTDQ  DTDXXI (DTDE3) )
C               ( DXIDS DXIDX1 DXIDX2 DXIDP DXIDR DXIDQ DXIDXI(DXIDE3))
C               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(DQDQ)(DQDXXI)(DQDE3) )
C
C
           N7 = 3*NDT + 4
C
           CALL LCICMA (DGDQ  , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N1,N6 )
           CALL LCICMA (DGDXXI, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N1,N7 )
C
           CALL LCICMA (DLDQ  , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N2,N6 )
           CALL LCICMA (DLDXXI, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N2,N7 )
C
           CALL LCICMA (DJDQ  , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N3,N6 )
           CALL LCICMA (DJDXXI, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N3,N7 )
C
           DRDY(N4,N6) = DKDQ
           CALL LCICMA (DKDXXI, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N4,N7 )
C
           DRDY(N5,N6) = DRDQ
           CALL LCICMA (DRDXXI, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N5,N7 )
C
           CALL LCICMA (DTDS  , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N6,N1 )
           CALL LCICMA (DTDX1 , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N6,N2 )
           CALL LCICMA (DTDX2 , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N6,N3 )
           DRDY(N6,N4) = DTDP
           DRDY(N6,N5) = DTDR
           DRDY(N6,N6) = DTDQ
           CALL LCICMA (DTDXXI, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N6,N7 )
C
           CALL LCICMA (DXIDS , 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N7,N1 )
           CALL LCICMA (DXIDX1, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N7,N2 )
           CALL LCICMA (DXIDX2, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N7,N3 )
           CALL LCICMA (DXIDP , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N7,N4 )
           CALL LCICMA (DXIDR , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N7,N5 )
           CALL LCICMA (DXIDQ , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N7,N6 )
           CALL LCICMA (DXIDXI,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD, N7,N7 )
C
           IF( MOD(1:6).EQ.'C_PLAN' ) THEN
           DRDY(N8,N6) = DQDQ
           CALL LCICMA (DQDXXI , 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD, N8,N7)
           CALL LCICMA (DXIDE3 , 6,1,NDT,  1,1,1,DRDY,NMOD,NMOD, N7,N8)
           DRDY(N6,N8) = DTDE3
           ENDIF
C
        ENDIF
C
        END
