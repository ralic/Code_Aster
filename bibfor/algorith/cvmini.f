        SUBROUTINE CVMINI (TYPESS, ESSAI, MOD, NMAT, MATERF,
     1                      TIMED,  TIMEF, YD, EPSD,DEPS,DY)
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
C     VISCOCHABOCHE : CALCUL SOLUTION ESSAI ( SI IOPTIO = 0 )
C                       DY = ( DSIG DX1 DX2 DP DR 0  (DEPS3))
C          AVEC         Y =  ( SIG  X1  X2  P  R  Q  (EPS3) )
C     VISCOCHABOCHE : CALCUL SOLUTION ESSAI ( SI IOPTIO = 2 )
C                       DY = ( DSIG DX1 DX2 DP DR DQ DXXI (DEPS3))
C          AVEC         Y =  ( SIG  X1  X2  P  R  Q  XXI  (EPS3) )
C       ----------------------------------------------------------------
C       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TIMED  :  TEMPS A T
C           TIMEF  :  TEMPS A T+DT
C           YD     :  VARIABLES A T   =  ( SIG X1 X2 P R (EPS3) )
C                     OU (SI IOPTIO =2) ( SIG X1 X2 P R Q XXI (EPS3) )
C           EPSD   :  DEFORMATION A T
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C           TYPESS :  TYPE DE SOLUTION D ESSAI
C                               0 = NUL(0)
C                               1 = ELASTIQUE
C                               2 = EXPLICITE (=-1 INITIALEMENT)
C                               3 = ESSAI
C       OUT DY     :  SOLUTION ESSAI  = ( DSIG DX1 DX2 DP DR (DEPS3))
C                            OU  ( SIG  X1  X2  P  R  Q  XXI  (EPS3))
C       ----------------------------------------------------------------
C
        INTEGER         NDT    , NDI , TYPESS , NMAT
        INTEGER         IOPTIO  , IDNR  , NOPT
C
C
        REAL*8          YD(*)     , DY(*)   , ESSAI
        REAL*8          HOOK(6,6) , DFDS(6) , FKOOH(6,6)
        REAL*8          EPSD(6)   , DEPS(6)
        REAL*8          SIG(6)    , DSIG(6)
        REAL*8          X1(6)     , DX1(6)  , X2(6)  , DX2(6)
        REAL*8          XXI(6)    , DXXI(6) , P      , DP
        REAL*8          R         , DR      , Q      , DQ
C
        REAL*8          TIMED     , TIMEF   , DT    , SEUIL
        REAL*8          MATERF(NMAT,2)
C
        REAL*8          K0   , AK  , N   , ALP
        REAL*8          B    , MR  , GR   , MU  , QM , Q0
        REAL*8          QR0  , ETA , AI
        REAL*8          M1   , D1  , GX1  , G10 , C1 , C1D
        REAL*8          M2   , D2  , GX2  , G20 , C2 , C2D
        REAL*8          CCIN , XX  , YY   , ZZ  , NUN , SGN
        REAL*8          GRQ  , QR
        REAL*8          VTMP(6) , VTMP1(6) , EPSP(6) , EPXI(6)
C
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
        COMMON /OPTI/   IOPTIO , IDNR
        COMMON /COED/   C1D , C2D
C       ----------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I 
      REAL*8 DIFC1 ,DIFC2 
C-----------------------------------------------------------------------
        IF ( TYPESS .EQ. -1 ) TYPESS = 2
C
C
        CALL LCEQVN ( NDT , YD(1)       , SIG )
        CALL LCEQVN ( NDT , YD(NDT+1)   , X1  )
        CALL LCEQVN ( NDT , YD(2*NDT+1) , X2  )
        P = YD(3*NDT+1)
        R = YD(3*NDT+2)
        Q = YD(3*NDT+3)
C
C
        K0      = MATERF(1,2)
        AK      = MATERF(2,2)
        N       = MATERF(5,2)
        ALP     = MATERF(6,2)
        B       = MATERF(7,2)
        MR      = MATERF(8,2)
        GR      = MATERF(9,2)
        MU      = MATERF(10,2)
        QM      = MATERF(11,2)
        Q0      = MATERF(12,2)
        QR0     = MATERF(13,2)
        ETA     = MATERF(14,2)
        C1      = MATERF(15,2)
        M1      = MATERF(16,2)
        D1      = MATERF(17,2)
        GX1     = MATERF(18,2)
        G10     = MATERF(19,2)
        C2      = MATERF(20,2)
        M2      = MATERF(21,2)
        D2      = MATERF(22,2)
        GX2     = MATERF(23,2)
        G20     = MATERF(24,2)
        AI      = MATERF(25,2)
C
        NOPT = 0
        IF ( IOPTIO .EQ. 2 ) NOPT = IDNR
C
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
C
C - SOLUTION INITIALE = NUL
C
        IF ( TYPESS .EQ. 0 ) THEN
            CALL VECINI ( 4*NDT+4 , 0.D0 , DY   )
C
C - SOLUTION INITIALE = ELASTIQUE
C
        ELSEIF ( TYPESS .EQ. 1 ) THEN
            CALL VECINI ( 4*NDT+4 , 0.D0 , DY   )
            CALL LCPRMV ( HOOK    , DEPS , DSIG )
            CALL LCEQVN ( NDT     , DSIG , DY(1))
C
C - SOLUTION INITIALE = EXPLICITE
C
        ELSEIF ( TYPESS .EQ. 2 ) THEN
C -     SOLUTION D'ESSAI POUR  ( SIG  X1  X2  P  R  0  (EPS3))
C
            CCIN = AI + (1.D0-AI) * EXP( -B*P )
            DT    = TIMEF - TIMED
C - DP
            CALL CVMCVX ( NMAT, MATERF,SIG, YD(NDT+1), SEUIL)
            IF ( SEUIL .LE. 0.D0)THEN
              DP   = 0.D0
              DO 1, I=1,6
                DFDS(I) = 0.D0
 1            CONTINUE
            ELSE
              CALL CHBFS  ( SIG  , X1  , X2 , DFDS )
              ZZ = SEUIL / ( K0 + AK * R)
C              IF ( ZZ .LT. 0.D0 ) ZZ = 0.D0
              DP = DT * (ZZ**N) * EXP( ALP*(ZZ**(N+1)) )
            ENDIF
C
C - DSIG
            CALL LCPRSV ( -DP   , DFDS , VTMP )
            CALL LCSOVE ( DEPS  , VTMP , VTMP )
            CALL LCPRMV ( HOOK  , VTMP , DSIG )
C
C - DX1
            CALL LCPRSC ( X1    , DFDS , ZZ   )
            CALL LCPRSC ( X1    , X1   , YY   )
            ZZ = ZZ * (1.D0-D1) * G10 * CCIN * DP * 2.D0/3.D0
            XX = C1 * DP * 2.D0/3.D0 - ZZ
            IF ( YY .LE. 0.D0 ) THEN
              YY=0.D0
            ELSE
              YY = GX1 * DT * ( SQRT(YY*3.D0/2.D0) )**(M1-1.D0)
     1         + G10 * CCIN * D1 * DP
            ENDIF
            CALL LCPRSV ( YY    , X1   , VTMP  )
            CALL LCPRSV ( XX    , DFDS , DX1   )
            CALL LCDIVE ( DX1   , VTMP , DX1   )
C
C - CAS ANISOTHERME
C
        IF (C1 .NE. 0.D0) THEN
          DIFC1 = (C1-C1D)/C1
          CALL LCPRSV ( DIFC1  , X1    , VTMP  )
          CALL LCSOVE ( DX1    , VTMP  , DX1   )
        ENDIF
C
C - DX2
            CALL LCPRSC ( X2    , DFDS , ZZ   )
            CALL LCPRSC ( X2    , X2   , YY   )
            ZZ = ZZ * (1.D0-D2) * G20 * CCIN * DP * 2.D0/3.D0
            XX = C2 * DP * 2.D0/3.D0 - ZZ
            IF ( YY .LE. 0.D0 ) THEN
              YY=0.D0
            ELSE
              YY = GX2 * DT * ( SQRT(YY*3.D0/2.D0) )**(M2-1.D0)
     1         + G20 * CCIN * D2 * DP
            ENDIF
            CALL LCPRSV ( YY    , X2   , VTMP  )
            CALL LCPRSV ( XX    , DFDS , DX2   )
            CALL LCDIVE ( DX2   , VTMP , DX2   )
C
C - CAS ANISOTHERME
C
        IF (C2 .NE. 0.D0) THEN
          DIFC2 = (C2-C2D)/C2
          CALL LCPRSV ( DIFC2  , X2    , VTMP  )
          CALL LCSOVE ( DX2    , VTMP  , DX2   )
        ENDIF
C
C - DR
            GRQ = Q0 + ( QM - Q0 ) * ( 1.D0 - EXP(-2.D0*MU*Q) )
            QR  = GRQ - QR0 * (1.D0 - ((QM-GRQ)/QM)**2)
            SGN = 1.D0
            IF ( (QR - R) .LT. 0.D0 ) SGN = - 1.D0
            DR = B * (GRQ - R) * DP + SGN * GR * DT
     1         * (ABS(QR - R))**MR
C
C - DEPS(3)
            IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
               NUN = MATERF(2,1) / (1.D0 - MATERF(2,1))
               DEPS(3) = NUN * (DP*(DFDS(1)+DFDS(2))-DEPS(1)-DEPS(2))
     &                    + DFDS(3)*DP
            ENDIF

C
C - DY
            CALL LCEQVN ( NDT , DSIG   , DY(1)       )
            CALL LCEQVN ( NDT , DX1    , DY(NDT+1)   )
            CALL LCEQVN ( NDT , DX2    , DY(2*NDT+1) )
            DY(3*NDT+1) = DP
            DY(3*NDT+2) = DR
            DY(3*NDT+3) = 0.D0
            IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
              DY(3*NDT+4+NOPT) = DEPS(3)
              DY(3)       = 0.D0
            ENDIF
C
C
C -         SOLUTION D'ESSAI POUR ( SIG  X1  X2  P  R  Q  XXI  (EPS3))
            IF ( IOPTIO .EQ. 2 ) THEN
               CALL LCEQVN ( NDT , YD(3*NDT+4)   , XXI )
C
C - EPSP
               CALL LCOPIL ( 'ISOTROPE', MOD , MATERF(1,1) , FKOOH )
               CALL LCSOVE ( SIG     , DSIG  , VTMP  )
               CALL LCPRMV ( FKOOH   , VTMP  , VTMP1 )
               CALL LCSOVE ( EPSD    , DEPS  , EPSP  )
               CALL LCDIVE ( EPSP    , VTMP1 , EPSP  )
C
C N-ETOILE
               CALL LCDIVE ( EPSP    , XXI   , VTMP  )
               CALL LCPRSC ( VTMP    , VTMP  , XX    )
               XX = SQRT( XX * 3.D0/2.D0 )

C H(F)
               ZZ = 2.D0/3.D0 * XX - Q
C
               IF( ZZ .LT. 0.D0 ) THEN
                 DQ = 0.D0
                 CALL LCINVE ( 0.D0 , DXXI   )
               ELSE

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
                   DQ = 0.D0
                   CALL LCINVE ( 0.D0 , DXXI   )
                 ELSE
C
C - DXXI
C                   CALL LCPRSC ( DFDS    , EPXI  , ZZ    )
                   XX =ZZ * (1.D0 - ETA ) *DP * 3.D0/2.D0
                   CALL LCPRSV ( XX , EPXI , DXXI  )
C
C - DQ
                   CALL LCPRSC ( DFDS    , EPXI  , XX    )
                   DQ = DP * ETA * XX
                 ENDIF
               ENDIF
C
C - DY
               DY(3*NDT+3) = DQ
               CALL LCEQVN ( NDT    , DXXI  , DY(3*NDT+4) )
            ENDIF
C
C - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
C
        ELSEIF ( TYPESS .EQ. 3 ) THEN
            CALL VECINI ( 4*NDT+4  , ESSAI , DY     )
            IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
               DEPS(3) = ESSAI
               DY(3)   = 0.D0
            ENDIF
C
        ENDIF
C
C
        END
