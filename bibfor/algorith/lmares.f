        SUBROUTINE LMARES ( MOD,   IMAT, NMAT, MATERD, MATERF, MATCST,
     1                      TEMPF, TIMED, TIMEF, YD, YF, DEPS, DY, RES )
        IMPLICIT REAL*8 (A-H,O-Z)
C
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C                  : CALCUL DES TERMES DU SYSTEME NL A RESOUDRE
C                                                            = RES(DY)
C                    DY   = ( DSIG DX  DX1 DX2 DV )
C                    Y    = ( SIG  X   X1  X2  V  )
C                    RES  = ( G    L   J   F   K  )
C       IN  MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           MATCST :  'OUI' SI MATERIAU CONSTANT SUR DT
C           TEMPF  :  TEMPERATURE A T + DT
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT  T+DT
C           YD     :  VARIABLES A T       = ( SIGD VIND )
C           YF     :  VARIABLES A T + DT  = ( SIGF VINF )
C           DY     :  SOLUTION ESSAI      = ( DSIG DVIN )
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT RES    :  SYSTEME NL A T + DT
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT , IMAT
C
        REAL*8          HOOKF(6,6), DKOOH(6,6)
        REAL*8          SIGF(6)   , DSIG(6) ,    SIGD(6) ,    DFDS(6)
        REAL*8          DEPS(6)   , DEPSP(6),    DEPSE(6)
        REAL*8          EPSED(6)  , EPSEF(6)
        REAL*8          X(6)      , DX(6)
        REAL*8          X1(6)     , DX1(6)
        REAL*8          X2(6)     , DX2(6)
        REAL*8          V         , DV
        REAL*8          GF(6)     , LF(6),   JF(6),  FF(6),  KF
        REAL*8          RES(*)    , DY(*),   YD(*),  YF(*)
        REAL*8          VTMP1(6)  ,VTMP2(6), VTMP(6)
        REAL*8          LMACIN    , YV
C
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          TIMED          ,TIMEF,  DT
        REAL*8          SEUIL, TEMPF ,   NU ,   NORMX ,     ZZ
        REAL*8          DE0, N,   K,   B,   A0
        REAL*8          RM,    RM1,  M,   M1,    P,     P1,  P2
C
        CHARACTER*8     MOD
        CHARACTER*3     MATCST
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        CALL LCEQVN ( NDT , YD(1)       , SIGD)
C
        CALL LCEQVN ( NDT , YF(1)       , SIGF)
        CALL LCEQVN ( NDT , YF(NDT+1)   , X   )
        CALL LCEQVN ( NDT , YF(2*NDT+1) , X1  )
        CALL LCEQVN ( NDT , YF(3*NDT+1) , X2  )
        V  = YF(4*NDT+1)
C
        CALL LCEQVN ( NDT , DY(1)      , DSIG )
        CALL LCEQVN ( NDT , DY(NDT+1)  , DX   )
        CALL LCEQVN ( NDT , DY(2*NDT+1), DX1  )
        CALL LCEQVN ( NDT , DY(3*NDT+1), DX2  )
        DV = DY(4*NDT+1)
C
        DE0   = MATERF(1,2)
        N      = MATERF(3,2)
        K      = MATERF(4,2)
        A0    = MATERF(8,2)
        RM     = MATERF(9,2)
        M      = MATERF(10,2)
        P      = MATERF(11,2)
        P1     = MATERF(12,2)
        P2     = MATERF(13,2)
C
C                   -1                     -1
C -     HOOKF, HOOKD , DFDS , EPSEF = HOOKD  SIGD + DEPS - DEPSP
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
        CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERD(1,1) , DKOOH )
C
        DT = TIMEF - TIMED
        YV = LMACIN ( IMAT , NMAT , MATERF , V )
        CALL LMAFS  ( IMAT , NMAT , MATERF , SIGF , X , DFDS )
        CALL LMACVX ( IMAT , NMAT , MATERF , TEMPF, SIGF,
     1                           YF(NDT+1), SEUIL )
C       CAS DE DIVERGENCE
        IF ( SEUIL .GE. 1.D6 ) SEUIL = 1.D6
C
C - GF  (T+DT)
C
        CALL LCPRSV ( DV          , DFDS  , DEPSP )
        CALL LCPRMV ( DKOOH       , SIGD  , EPSED )
        CALL LCDIVE ( DEPS        , DEPSP , DEPSE )
        CALL LCSOVE ( EPSED       , DEPSE , EPSEF )
        CALL LCPRMV ( HOOKF       , EPSEF , GF    )
        CALL LCDIVE ( GF          , SIGF  , GF    )
C
C - LF (T+DT)
C
        CALL LCPRMV ( MATERF(52,2), X     , VTMP  )
        CALL LCPRSC ( VTMP        , X     , NORMX )
        NORMX = SQRT( 1.5D0 * NORMX )
C        ZZ = -DT * (RM*(NORMX/A0)**M + RM1*(NORMX/A0)**M1)
        ZZ = -DT * RM * SINH((NORMX/A0)**M )
        IF ( NORMX .NE. 0.D0 ) THEN
            CALL LCPRSV ( ZZ/NORMX    , X     , VTMP  )
            CALL LCPRMV ( MATERF(52,2), VTMP  , VTMP2 )
            CALL LCPRMV ( MATERF(52,1), VTMP2 , VTMP  )
        ELSE
            CALL LCINVE ( 0.D0        , VTMP          )
        ENDIF
        CALL LCDIVE ( VTMP        , DX    , LF    )
        CALL LCPRSV ( 2.D0/3.D0*YV*DV, DFDS  , VTMP  )
        CALL LCPRMV ( MATERF(52,1), VTMP  , VTMP1 )
        CALL LCPRSV ( P           , VTMP1 , VTMP  )
        CALL LCSOVE ( LF          , VTMP  , LF    )
        CALL LCDIVE ( X           , X1    , VTMP  )
        CALL LCPRSV ( -P*DV       , VTMP  , VTMP  )
        CALL LCPRMV ( MATERF(16,2), VTMP  , VTMP2 )
        CALL LCSOVE ( LF          , VTMP2 , LF    )
C
C - JF (T+DT)
        CALL LCPRSV ( P1          , VTMP1 , JF    )
        CALL LCDIVE ( X1          , X2    , VTMP  )
        CALL LCPRSV ( DV*P1       , VTMP  , VTMP  )
        CALL LCPRMV ( MATERF(16,2), VTMP  , VTMP2 )
        CALL LCDIVE ( JF          , VTMP2 , JF    )
        CALL LCDIVE ( JF          , DX1   , JF    )
C
C - FF (T+DT)
C
        CALL LCPRSV ( P2          , VTMP1 , FF    )
        CALL LCPRSV ( DV*P2       , X2    , VTMP  )
        CALL LCPRMV ( MATERF(16,2), VTMP  , VTMP2 )
        CALL LCDIVE ( FF          , VTMP2 , FF    )
        CALL LCDIVE ( FF          , DX2   , FF    )
C
C - KF (T+DT)
C
C        PRINT*,'SEUIL',SEUIL
        KF = DE0 * DT * ( SINH(SEUIL/K) )**N - DV
C
C
C - RES (T+DT) = ( GF LF JF FF KF )
C
        CALL LCEQVN ( NDT        , GF     , RES(1)       )
        CALL LCEQVN ( NDT        , LF     , RES(NDT+1)   )
        CALL LCEQVN ( NDT        , JF     , RES(2*NDT+1) )
        CALL LCEQVN ( NDT        , FF     , RES(3*NDT+1) )
        RES(4*NDT+1) = KF
C
C
        END
