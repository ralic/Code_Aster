        SUBROUTINE CVMRES ( MOD, NMAT, MATERD, MATERF,
     1                      TIMED, TIMEF, YD , YF, EPSD, DEPS,
     2                      DY, RES )
      IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     VISCOCHABOCHE :
C            CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = RES(DY)
C                   DY  = ( DSIG DX1 DX2 DP DR 0  (DEPS3))
C                   Y   = ( SIG  X1  X2  P  R  Q  (EPS3) )
C                   RES = ( GF   LF  JF  KF RF 0  (F)    )
C                                                      (SI IOPTIO = 0)
C
C                   DY =  ( DSIG DX1 DX2 DP DR DQ DXXI (DEPS3))
C                   Y  =  ( SIG  X1  X2  P  R  Q  XXI  (EPS3) )
C                   RES = ( GF   LF  JF  KF RF TF XIF  (FF)   )
C                                                      (SI IOPTIO = 2)
C
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YD     :  VARIABLES A T      = ( SIGD  X1D X2D PD RD QD(..))
C           YF     :  VARIABLES A T + DT = ( SIGF  X1F X2F PF RF QF(..))
C           DY     :  SOLUTION ESSAI     = ( DSIG  DX1 DX2 DP DR DQ(..))
C           EPSD   :  DEFORMATION A T
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT RES    :  SYSTEME NL A T + DT
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT
        INTEGER         IOPTIO   , IDNR , NOPT
C
        REAL*8          HOOKF(6,6), DKOOH(6,6) , FKOOH(6,6)
        REAL*8          SIGF(6)   , DSIG(6) , SIGD(6) , DFDS(6)
        REAL*8          DEPS(6)   , DEPSP(6), DEPSE(6)
        REAL*8          EPSED(6)  , EPSEF(6), EPSD(6)
        REAL*8          X1(6)     , DX1(6)  , X2(6)  , DX2(6)
        REAL*8          XXI(6)    , DXXI(6) , P      , DP
        REAL*8          R         , DR      , Q      , DQ
        REAL*8          GF(6)     , LF(6)   , JF(6)  , KF
        REAL*8          RF        , TF      , XIF(6) , FF
        REAL*8          RES(*)    , DY(*)   ,  YD(*)  , YF(*)
        REAL*8          TIMED   , TIMEF   , DT
C
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          SEUIL
C
        REAL*8          K0  , AK   , N   , ALP
        REAL*8          B    , MR , GR  , MU   , QM , Q0
        REAL*8          QR0 , ETA , AI
        REAL*8          M1  , D1  , GX1 , G10 , C1 , C1D
        REAL*8          M2  , D2  , GX2 , G20 , C2 , C2D
        REAL*8          DIFC1  , DIFC2
        REAL*8          CCIN , XX  , YY   , ZZ    , SGN
        REAL*8          GRQ  , QR
        REAL*8          VTMP(6) , VTMP1(6) , EPSP(6) , EPXI(6)
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
        COMMON /OPTI/   IOPTIO , IDNR
C       ----------------------------------------------------------------
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
        CALL LCEQVN ( NDT , YD(1)       , SIGD)
        CALL LCEQVN ( NDT , YF(1)       , SIGF)
        CALL LCEQVN ( NDT , YF(NDT+1)   , X1  )
        CALL LCEQVN ( NDT , YF(2*NDT+1) , X2  )
        P  = YF(3*NDT+1)
        R  = YF(3*NDT+2)
        Q  = YF(3*NDT+3)
        CALL LCEQVN ( NDT , DY(1)       , DSIG)
        CALL LCEQVN ( NDT , DY(NDT+1)   , DX1 )
        CALL LCEQVN ( NDT , DY(2*NDT+1) , DX2 )
        DP = DY(3*NDT+1)
        DR = DY(3*NDT+2)
        DQ = DY(3*NDT+3)
C
C
        K0     = MATERF(1,2)
        AK     = MATERF(2,2)
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
        C1D    = MATERD(15,2)
        C2D    = MATERD(20,2)
C
        NOPT = 0
        IF ( IOPTIO .EQ. 2 ) NOPT = IDNR
C
C                   -1                     -1
C -     HOOKF, HOOKD , DFDS , EPSEF = HOOKD  SIGD + DEPS - DEPSP
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
        CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
C
        CALL CHBFS  ( SIGF , X1 , X2 , DFDS )
        CALL CVMCVX ( NMAT, MATERF,SIGF, YF(NDT+1), SEUIL)
        CCIN = AI + (1.D0-AI) * EXP( -B*P )
        DT    = TIMEF - TIMED
C
C -     CALCUL DU RESIDU POUR ( SIG  X1  X2  P  R  0  (EPS3))
C
C - GF  (T+DT)
C
        CALL LCPRSV ( DP    , DFDS    , DEPSP )
        CALL LCPRMV ( DKOOH , SIGD    , EPSED )
        CALL LCDIVE ( DEPS  , DEPSP   , DEPSE )
        CALL LCSOVE ( EPSED , DEPSE   , EPSEF )
        CALL LCPRMV ( HOOKF , EPSEF   , GF    )
        CALL LCDIVE ( GF    , SIGF    , GF    )
C
C - LF (T+DT)
C
        CALL LCPRSC ( X1    , DFDS    , ZZ    )
        CALL LCPRSC ( X1    , X1      , YY    )
        ZZ = ZZ * (1.D0-D1) * G10 * CCIN * DP * 2.D0/3.D0
        XX = C1 * DP * 2.D0/3.D0 - ZZ
        YY = GX1 * DT * ( SQRT(YY*3.D0/2.D0) )**(M1-1.D0)
     1     + G10 * CCIN * D1 * DP
        CALL LCPRSV ( XX     , DFDS   , LF    )
        CALL LCPRSV ( YY     , X1     , VTMP  )
        CALL LCDIVE ( LF     , VTMP   , LF    )
        CALL LCDIVE ( LF     , DX1    , LF    )
C
        IF (C1D .NE. 0.D0) THEN
          DIFC1 = (C1-C1D)/C1
          CALL LCPRSV ( DIFC1  , X1    , VTMP  )
          CALL LCSOVE ( LF     , VTMP  , LF    )
        ENDIF
C
C - JF (T+DT)
C
        CALL LCPRSC ( X2     , DFDS   , ZZ    )
        CALL LCPRSC ( X2     , X2     , YY    )
        ZZ = ZZ * (1.D0-D2) * G20 * CCIN * DP * 2.D0/3.D0
        XX = C2 * DP * 2.D0/3.D0 - ZZ
        YY = GX2 * DT * ( SQRT(YY*3.D0/2.D0) )**(M2-1.D0)
     1     + G20 * CCIN * D2 * DP
        CALL LCPRSV ( XX    , DFDS    , JF    )
        CALL LCPRSV ( YY    , X2      , VTMP  )
        CALL LCDIVE ( JF    , VTMP    , JF    )
        CALL LCDIVE ( JF    , DX2     , JF    )
C
C - CAS ANISOTHERME
C
        IF (C2 .NE. 0.D0) THEN
          DIFC2 = (C2-C2D)/C2
          CALL LCPRSV ( DIFC2  , X2    , VTMP  )
          CALL LCSOVE ( JF     , VTMP  , JF    )
        ENDIF
C
C - KF (T+DT)
C
        ZZ = SEUIL / ( K0 + AK * R)
        IF ( ZZ .LT. 0.D0 ) ZZ = 0.D0
        KF = DT * (ZZ**N) * EXP( ALP*(ZZ**(N+1)) ) - DP
C
C - RF (T+DT)
C
        GRQ = Q0 + ( QM - Q0 ) * ( 1.D0 - EXP(-2.D0*MU*Q) )
        QR  = GRQ - QR0 * (1.D0 - ((QM-GRQ)/QM)**2)
        SGN = 1.D0
        IF ( (QR - R) .LT. 0.D0 ) SGN = - 1.D0
        RF = B*(GRQ - R)*DP + SGN*GR*DT*(ABS(QR - R))**MR - DR
C
C - FF (T+DT)
C
        IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
            FF = - HOOKF(3,3) *  EPSEF(3) - HOOKF(3,1) *  EPSEF(1)
     1           - HOOKF(3,2) *  EPSEF(2) - HOOKF(3,4) *  EPSEF(4)
        ENDIF
C
C - RES (T+DT) = ( GF LF JF KF RF 0 (FF) )
C
        CALL LCEQVN ( NDT   , GF      , RES(1) )
        CALL LCEQVN ( NDT   , LF      , RES(NDT+1) )
        CALL LCEQVN ( NDT   , JF      , RES(2*NDT+1) )
        RES(3*NDT+1)  = KF
        RES(3*NDT+2)  = RF
        RES(3*NDT+3)  = 0.D0
        IF ( MOD(1:6).EQ.'C_PLAN' ) RES(3*NDT+4+NOPT) = FF
C
C
C -     CALCUL DU RESIDU POUR ( SIG  X1  X2  P  R  Q XXI (EPS3))
        IF ( IOPTIO .EQ. 2 ) THEN
        CALL LCEQVN ( NDT , YF(3*NDT+4) , XXI )
        CALL LCEQVN ( NDT , DY(3*NDT+4) , DXXI)
C
C - EPSP
C
            CALL LCOPIL ( 'ISOTROPE', MOD , MATERF(1,1) , FKOOH )
            CALL LCPRMV ( FKOOH   , SIGF  , VTMP1 )
            CALL LCSOVE ( EPSD    , DEPS  , EPSP  )
            CALL LCDIVE ( EPSP    , VTMP1 , EPSP  )
C
C N-ETOILE
C
            CALL LCDIVE ( EPSP    , XXI   , VTMP )
            CALL LCPRSC ( VTMP    , VTMP  , XX   )
            XX = SQRT( XX * 3.D0/2.D0 )

C H(F)
            ZZ = 2.D0/3.D0 * XX - Q
C
            IF( ZZ .LT. 0.D0 ) THEN
              TF = 0.D0
              CALL LCINVE ( 0.D0 , XIF   )
            ELSE
C
              IF( XX .EQ. 0.D0 ) THEN
                CALL LCINVE ( 0.D0    , EPXI          )
              ELSE
                CALL LCPRSV ( 1.D0/XX , VTMP  , EPXI  )
              ENDIF
C
C N *  N-ETOILE
C
              CALL LCPRSC ( DFDS    , EPXI  , ZZ   )
C
              IF ( ZZ .LE. 0.D0 ) THEN
                TF = 0.D0
                CALL LCINVE ( 0.D0 , XIF   )
              ELSE
C
C - TF (T+DT)
C
                CALL LCPRSC ( DFDS    , EPXI  , XX    )
                TF = DP * ETA * XX - DQ
C
C - XIF (T+DT)
C
C                CALL LCPRSC ( DFDS    , EPXI  , ZZ   )
                XX =ZZ * (1.D0 - ETA ) *DP * 3.D0/2.D0
                CALL LCPRSV ( XX     , EPXI   , VTMP )
                CALL LCDIVE ( VTMP   , DXXI   , XIF  )
C
              ENDIF
            ENDIF
C
C - RES (T+DT) = ( GF LF JF KF RF TF XIF (FF) )
C
C        PRINT *," TF ",TF
C        PRINT *," XIF ",XIF
C
           RES(3*NDT+3)  = TF
           CALL LCEQVN ( NDT     , XIF     , RES(3*NDT+4) )
C
        ENDIF
C
C
        END
