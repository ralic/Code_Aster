        SUBROUTINE LMAJAC ( MOD, IMAT, NMAT, MATERF, TEMPF, TIMED,
     1                      TIMEF, YF,   DY,   NMOD,  DRDY)
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C                   : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C              DY    = ( DSIG  DX    DX1    DX2    DV    )
C              Y     = ( SIG   X     X1     X2     V     )
C              DRDY  = ( DGDS  DGDX  DGDX1  DGDX2  DGDV  )
C                      ( DLDS  DLDX  DLDX1  DLDX2  DLDV  )
C                      ( DJDS  DJDX  DJDX1  DJDX2  DJDV  )
C                      ( DIDS  DIDX  DIDX1  DIDX2  DIDV  )
C                      ( DKDS  DKDX  DKDX1  DKDX2  DKDV  )
C       IN  MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TEMPF  :  TEMPERATURE A T+DT
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT  T+DT
C           YF     :  VARIABLES A T + DT =   ( SIGF XF X1F X2F VF )
C           DY     :  SOLUTION           =   ( DSIG DX DX1 DX2 DV )
C           NMOD   :  DIMENSION DECLAREE DRDY
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT , IMAT , NMOD
        REAL*8          UN  , ZERO , D23 , D13
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
        PARAMETER       ( D23  =  2.D0/3.D0 )
        PARAMETER       ( D13  = -1.D0/3.D0 )
C
        REAL*8          HOOK(6,6) , DDFDDS(6,6)  , DDFDSX(6,6)
        REAL*8          DFDS(6) ,   DFDDS1(6,6) , DFDSX1(6,6)
        REAL*8          ID(6,6)   , I4(6,6)
        REAL*8          SIG(6)    , X(6)   , X1(6)  ,    X2(6)
        REAL*8          DSIG(6)   , DX(6)  , DX1(6) ,    DX2(6)
        REAL*8          V         , DV
        REAL*8          YF(*)     , DY(*)  , DRDY(NMOD,NMOD)
C
        REAL*8          DGDS(6,6) , DGDX(6,6) , DGDX1(6,6)
        REAL*8          DLDS(6,6) , DLDX(6,6) , DLDX1(6,6)
        REAL*8          DJDS(6,6) , DJDX(6,6) , DJDX1(6,6)
        REAL*8          DIDS(6,6) , DIDX(6,6) , DIDX1(6,6)
        REAL*8          DKDS(6)   , DKDX(6)   , DKDX1(6)
C
        REAL*8          DGDX2(6,6), DGDV(6)
        REAL*8          DLDX2(6,6), DLDV(6)
        REAL*8          DJDX2(6,6), DJDV(6)
        REAL*8          DIDX2(6,6), DIDV(6)
        REAL*8          DKDX2(6)  , DKDV
C
        REAL*8          VTMP(6)   , VTMP1(6), VTMP2(6), VTMP3(6)
        REAL*8          MTMP(6,6) , MTMP1(6,6)
        REAL*8          MATERF(NMAT,2),  TEMPF,  TIMED ,  TIMEF
        REAL*8          DE0, N,   K,   B,   A0
        REAL*8          RM,       M,      P,     P1,  P2
        REAL*8          H1,    NU,  ZZ,  NORMX
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        REAL*8          LMACIN,   LMACID,   YV
C       LOI D'ECROUISSAGE CINEMATIQUE Y(V) ET SA DERIVEE
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
        DATA  I4        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
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
C
        CALL LCEQVN ( NDT , YF(1)       , SIG )
        CALL LCEQVN ( NDT , YF(NDT+1)   , X   )
        CALL LCEQVN ( NDT , YF(2*NDT+1) , X1  )
        CALL LCEQVN ( NDT , YF(3*NDT+1) , X2  )
        CALL LCEQVN ( NDT , DY(1)       , DSIG )
        CALL LCEQVN ( NDT , DY(NDT+1)   , DX   )
        CALL LCEQVN ( NDT , DY(2*NDT+1) , DX1  )
        CALL LCEQVN ( NDT , DY(3*NDT+1) , DX2  )
        V  = YF(4*NDT+1)
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
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
        CALL LMAFS  ( IMAT , NMAT , MATERF , SIG , X , DFDS )
        CALL LMAFSS ( IMAT , NMAT , MATERF , SIG , X ,ID,DDFDDS )
        CALL LMAFSX ( IMAT , NMAT , MATERF , SIG , X , DDFDSX )
        CALL LMACVX ( IMAT , NMAT , MATERF , TEMPF, SIG,
     1                           YF(NDT+1), SEUIL )
        DT = TIMEF - TIMED
        YV = LMACIN ( IMAT , NMAT , MATERF , V )
        H1 = 2.D0/3.D0 * ( YV + DV * LMACID(IMAT,NMAT,MATERF,V) )
C
C - DGDS(T+DT)
        CALL LCPRMM ( HOOK        , DDFDDS      , DGDS  )
        CALL LCPRSM ( DV          , DGDS        , DGDS  )
        CALL LCSOMA ( I4          , DGDS        , DGDS  )
C
C - DGDX(T+DT)
        CALL LCPRMM ( HOOK        , DDFDSX      , DGDX  )
        CALL LCPRSM ( DV          , DGDX        , DGDX  )
C
C - DGDX1(T+DT)
        CALL LCINMA( 0.D0         , DGDX1               )
C
C - DGDX2(T+DT)
        CALL LCINMA( 0.D0         , DGDX2               )
C
C - DGDV(T+DT)
        CALL LCPRMV( HOOK         , DFDS        , DGDV  )
C
C - DLDS(T+DT)
        CALL LCPRMM ( MATERF(52,1), DDFDDS      , DFDDS1 )
        CALL LCPRSM ( 2.D0/3.D0*YV*DV, DFDDS1    , DFDDS1)
        CALL LCPRSM ( -P          , DFDDS1      , DLDS   )
C
C - DLDX(T+DT)
        CALL LCPRMM ( MATERF(52,1), DDFDSX      , DFDSX1)
        CALL LCPRSM ( 2.D0/3.D0*YV*DV,DFDSX1    , DFDSX1)
        CALL LCPRSM ( -P          , DFDSX1      , DLDX  )
        CALL LCSOMA ( I4          , DLDX        , DLDX  )
        CALL LCPRSM ( P*DV        , MATERF(16,2), MTMP  )
        CALL LCSOMA ( MTMP        , DLDX        , DLDX  )
        CALL LCPRMV ( MATERF(52,2), X           , VTMP1 )
        CALL LCPRSC ( X           , VTMP1       , NORMX )
        NORMX = SQRT ( 1.5D0 * NORMX )
        IF ( NORMX .NE. 0.D0 ) THEN
C        ZZ = RM  * (M -1.D0) * ((NORMX/A0)**(M -2.D0))
C     1     + RM1 * (M1-1.D0) * ((NORMX/A0)**(M1-2.D0))
C        ZZ = 1.5D0 * DT * ZZ / ( NORMX * A0**2 )
        ZZ = -RM  * SINH((NORMX/A0)**M)*(1.D0/NORMX)
     1     + RM * (M/(A0**M)) * (NORMX**(M-1.D0))
     1     * COSH((NORMX/A0)**M)
        ZZ = 1.5D0 * DT * ZZ / ( NORMX**2 )
        ELSE
        ZZ = 0.D0
        ENDIF
        CALL LCPRMV ( MATERF(52,1), VTMP1       , VTMP  )
        CALL LCPRTE ( VTMP1       , VTMP        , MTMP  )
        CALL LCPRSM ( ZZ          , MTMP        , MTMP  )
        CALL LCSOMA ( MTMP        , DLDX        , DLDX  )
        IF ( NORMX .NE. 0.D0 ) THEN
C        ZZ = RM  * ((NORMX/A0)**(M -1.D0))
C     1     + RM1 * ((NORMX/A0)**(M1-1.D0))
C        ZZ = DT * ZZ / A0
        ZZ = RM  * SINH((NORMX/A0)**M)
        ZZ = DT * ZZ / NORMX
        ELSE
        ZZ = 0.D0
        ENDIF
        CALL LCPRMM ( MATERF(52,1), MATERF(52,2), MTMP  )
        CALL LCPRSM ( ZZ          , MTMP        , MTMP  )
        CALL LCSOMA ( MTMP        , DLDX        , DLDX  )
C
C - DLDX1(T+DT)
        CALL LCPRSM ( - P*DV      , MATERF(16,2), DLDX1 )
C
C - DLDX2(T+DT)
        CALL LCINMA ( 0.D0        , DLDX2               )
C
C - DLDV(T+DT)
        CALL LCDIVE ( X           , X1          , VTMP  )
        CALL LCPRSV ( P           , VTMP        , VTMP  )
        CALL LCPRMV ( MATERF(16,2), VTMP        , DLDV  )
        CALL LCPRMV ( MATERF(52,1), DFDS        , VTMP  )
        CALL LCPRSV ( H1          , VTMP        , VTMP2 )
        CALL LCPRSV ( P           , VTMP2       , VTMP  )
        CALL LCDIVE ( DLDV        , VTMP        , DLDV  )
C
C - DJDS(T+DT)
        CALL LCPRSM ( -P1         , DFDDS1      , DJDS  )
C
C - DJDX(T+DT)
        CALL LCPRSM ( -P1         , DFDSX1      , DJDX  )
C
C - DJDX1(T+DT)
        CALL LCPRSM ( P1*DV       , MATERF(16,2), MTMP  )
        CALL LCSOMA ( I4          , MTMP        , DJDX1 )
C
C - DJDX2(T+DT)
        CALL LCPRSM ( -P1*DV      , MATERF(16,2), DJDX2 )
C
C - DJDV(T+DT)
        CALL LCPRSV ( P1          , VTMP2       , VTMP  )
        CALL LCDIVE ( X1          , X2          , VTMP1 )
        CALL LCPRSV ( P1          , VTMP1       , VTMP1 )
        CALL LCPRMV ( MATERF(16,2), VTMP1       , VTMP3 )
        CALL LCDIVE ( VTMP3       , VTMP        , DJDV  )
C
C - DIDS(T+DT)
        CALL LCPRSM ( -P2         , DFDDS1      , DIDS  )
C
C - DIDX(T+DT)
        CALL LCPRSM ( -P2         , DFDSX1      , DIDX  )
C
C - DIDX1(T+DT)
        CALL LCINMA( 0.D0         , DIDX1               )
C
C - DIDX2(T+DT)
        CALL LCPRSM ( P2*DV       , MATERF(16,2), MTMP  )
        CALL LCSOMA ( MTMP        , I4          , DIDX2 )
C
C - DIDV(T+DT)
        CALL LCPRMV ( MATERF(16,2), X2          , VTMP  )
        CALL LCPRSV ( P2          , VTMP        , DIDV  )
        CALL LCPRSV ( P2          , VTMP2       , VTMP  )
        CALL LCDIVE ( DIDV        , VTMP        , DIDV  )
C
C - DKDS(T+DT)
        ZZ = DE0 * DT * N * (SINH(SEUIL/K)**(N-1))
     1     * COSH(SEUIL/K) / K
C
        CALL LCPRSV ( -ZZ         , DFDS        , DKDS  )
C
C - DKDX(T+DT)
        CALL LCPRSV ( ZZ          , DFDS        , DKDX  )
C
C - DKDX1(T+DT)
        CALL LCINVE ( 0.D0       , DKDX1                )
C
C - DKDX2(T+DT)
        CALL LCINVE ( 0.D0       , DKDX2                )
C
C - DKDV(T+DT)
        DKDV = 1.D0
C
C - ASSEMBLAGE ---------------------------------------------------------
C
C - DRDY(T+DT)  =  DGDS   DGDX   DGDX1   DGDX2   DGDV
C                  DLDS   DLDX   DLDX1   DLDX2   DLDV
C                  DJDS   DJDX   DJDX1   DJDX2   DJDV
C                  DIDS   DIDX   DIDX1   DIDX2   DIDV
C                  DKDS   DKDX   DKDX1   DKDX2   DKDV
C
       CALL LCICMA(DGDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,    1        )
       CALL LCICMA(DGDX, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,    NDT+1    )
       CALL LCICMA(DGDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,    2*NDT+1  )
       CALL LCICMA(DGDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,    3*NDT+1  )
       CALL LCICMA(DGDV, 6,1,NDT,1  ,1,1,DRDY,NMOD,NMOD,1,    4*NDT+1  )
C
       CALL LCICMA(DLDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1        )
       CALL LCICMA(DLDX, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,NDT+1    )
       CALL LCICMA(DLDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,2*NDT+1  )
       CALL LCICMA(DLDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,3*NDT+1  )
       CALL LCICMA(DLDV, 6,1,NDT,1  ,1,1,DRDY,NMOD,NMOD,NDT+1,4*NDT+1  )
C
       CALL LCICMA(DJDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,1      )
       CALL LCICMA(DJDX, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,NDT+1  )
       CALL LCICMA(DJDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,2*NDT+1)
       CALL LCICMA(DJDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,3*NDT+1)
       CALL LCICMA(DJDV, 6,1,NDT,1  ,1,1,DRDY,NMOD,NMOD,2*NDT+1,4*NDT+1)
C
       CALL LCICMA(DIDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,1      )
       CALL LCICMA(DIDX, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,NDT+1  )
       CALL LCICMA(DIDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,2*NDT+1)
       CALL LCICMA(DIDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,3*NDT+1)
       CALL LCICMA(DIDV, 6,1,NDT,1  ,1,1,DRDY,NMOD,NMOD,3*NDT+1,4*NDT+1)
C
       CALL LCICMA(DKDS, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,1      )
       CALL LCICMA(DKDX, 1,6,  1,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,NDT+1  )
       CALL LCICMA(DKDX1,1,6,  1,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,2*NDT+1)
       CALL LCICMA(DKDX2,1,6,  1,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,3*NDT+1)
       DRDY(4*NDT+1,4*NDT+1) = DKDV
C
       END
