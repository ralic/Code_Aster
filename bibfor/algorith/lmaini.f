        SUBROUTINE LMAINI ( TYPESS, ESSAI, MOD,   IMAT,  NMAT,
     &                      MATERF, TEMPD, TIMED, TIMEF, YD,  DEPS, DY)
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
C               CALCUL SOLUTION ESSAI DY =( DSIG DX DX1 DX2 DV )
C                           AVEC       Y = ( SIG  X  X1  X2  V )
C       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
C           MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TEMPD  :  TEMPERATURE A T
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT  T+DT
C           YD     :  VARIABLES A T   = ( SIG  VIN  )
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C           TYPESS :  TYPE DE SOLUTION D ESSAI
C                               0 = NUL(0)
C                               1 = ELASTIQUE
C                               2 = EXPLICITE (=-1 INITIALEMENT)
C                               3 = ESSAI
C       OUT DY     :  SOLUTION ESSAI  = ( DSIG DVIN )
C       ----------------------------------------------------------------
C
        INTEGER         NDT , NDI , TYPESS , NMAT , IMAT
C
        REAL*8          YD(*)     , DY(*),  ESSAI
        REAL*8          HOOK(6,6) , DFDS(6)
        REAL*8          DEPS(6)
        REAL*8          SIG(6)    , DSIG(6)
        REAL*8          X(6)      , DX(6)
        REAL*8          X1(6)     , DX1(6)
        REAL*8          X2(6)     , DX2(6)
        REAL*8          V         , DV
        REAL*8          VTMP1(6)  , VTMP2(6)  , VTMP(6)
        REAL*8          DE0,   N,   K,    B,   A0
        REAL*8          RM,  M,    P,     P1,  P2
        REAL*8          ZZ,         NORMX,      SEUIL
C
        REAL*8          MATERF(NMAT,2) , TEMPD , TIMED , TIMEF
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
        REAL*8          LMACIN,   LMACID,   YV
C       LOI D'ECROUISSAGE CINEMATIQUE Y(V) ET SA DERIVEE
C       ----------------------------------------------------------------
C
        IF ( TYPESS .EQ. -1 ) TYPESS = 2
C
C
        CALL LCEQVN ( NDT , YD(1)       , SIG )
        CALL LCEQVN ( NDT , YD(NDT+1)   , X   )
        CALL LCEQVN ( NDT , YD(2*NDT+1) , X1  )
        CALL LCEQVN ( NDT , YD(3*NDT+1) , X2  )
        V = YD(4*NDT+1)
C
        DE0    = MATERF(1,2)
        N      = MATERF(3,2)
        K      = MATERF(4,2)
        A0     = MATERF(8,2)
        RM     = MATERF(9,2)
        M      = MATERF(10,2)
        P      = MATERF(11,2)
        P1     = MATERF(12,2)
        P2     = MATERF(13,2)
C
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
C
C - SOLUTION INITIALE = NUL
C
                IF ( TYPESS .EQ. 0 ) THEN
                   CALL LCINVN ( 4*NDT+1 , 0.D0 , DY )
C
C - SOLUTION INITIALE = ELASTIQUE
C
                ELSEIF ( TYPESS .EQ. 1 ) THEN
                   CALL LCINVN ( 4*NDT+1 , 0.D0 , DY    )
                   CALL LCPRMV ( HOOK    , DEPS , DSIG  )
                   CALL LCEQVN ( NDT     , DSIG , DY(1) )
C
C - SOLUTION INITIALE = EXPLICITE
C
                ELSEIF ( TYPESS .EQ. 2 ) THEN
                   CALL LMACVX ( IMAT, NMAT, MATERF , TEMPD, SIG,
     1                           YD(NDT+1), SEUIL )
                   DT = TIMEF - TIMED
                   YV = LMACIN ( IMAT , NMAT , MATERF , V )
                   CALL LMAFS ( IMAT , NMAT , MATERF , SIG , X , DFDS )
C
C - DV
                   IF ( SEUIL .LT. 0.D0 ) SEUIL = 0.D0
                   DV = DE0 * DT * ( SINH(SEUIL/K) )**N
C
C - DSIG
                   CALL LCPRSV ( - DV , DFDS   , VTMP )
                   CALL LCSOVE ( DEPS , VTMP   , VTMP )
                   CALL LCPRMV ( HOOK , VTMP   , DSIG )
C
C - DX
                   CALL LCPRMV ( MATERF(52,2) , X    , VTMP  )
                   CALL LCPRSC ( VTMP         , X    , NORMX )
                   NORMX = SQRT(1.5D0*NORMX)
C                   ZZ = -DT * (RM*(NORMX/A0)**M + RM1*(NORMX/A0)**M1)
                   ZZ = -DT * RM * SINH((NORMX/A0)**M )
                   IF ( NORMX .NE. 0.D0 ) THEN
                       CALL LCPRSV ( ZZ/NORMX    , X     , VTMP )
                       CALL LCPRMV ( MATERF(52,2), VTMP  ,VTMP2  )
                       CALL LCPRMV ( MATERF(52,1), VTMP2  ,DX    )
                   ELSE
                       CALL LCINVE ( 0.D0        , DX           )
                   ENDIF
                   CALL LCPRSV ( 2.D0/3.D0*YV*DV, DFDS  , VTMP )
                   CALL LCPRMV ( MATERF(52,1), VTMP  , VTMP1)
                   CALL LCPRSV ( P           , VTMP1 , VTMP )
                   CALL LCSOVE ( DX          , VTMP  , DX   )
                   CALL LCDIVE ( X           , X1    , VTMP )
                   CALL LCPRSV ( P*DV        , VTMP  , VTMP )
                   CALL LCPRMV ( MATERF(16,2), VTMP  , VTMP2)
                   CALL LCDIVE ( DX          , VTMP2 , DX   )
C
C - DX1
                   CALL LCPRSV ( P1          , VTMP1 , DX1  )
                   CALL LCDIVE ( X1          , X2    , VTMP )
                   CALL LCPRSV ( DV*P1       , VTMP  , VTMP )
                   CALL LCPRMV ( MATERF(16,2), VTMP  , VTMP2)
                   CALL LCDIVE ( DX1         , VTMP2 , DX1  )
C
C - DX2
                   CALL LCPRSV ( P2          , VTMP1 , DX2  )
                   CALL LCPRSV ( DV*P2       , X2    , VTMP )
                   CALL LCPRMV ( MATERF(16,2), VTMP  , VTMP2)
                   CALL LCDIVE ( DX2         , VTMP2 , DX2  )
C
C - DY
                   CALL LCEQVN ( NDT         , DSIG  , DY(1)      )
                   CALL LCEQVN ( NDT         , DX    , DY(NDT+1)  )
                   CALL LCEQVN ( NDT         , DX1   , DY(2*NDT+1))
                   CALL LCEQVN ( NDT         , DX2   , DY(3*NDT+1))
                   DY(4*NDT+1) = DV
C
C
C - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
C
                ELSEIF ( TYPESS .EQ. 3 ) THEN
                   CALL LCINVN ( 4*NDT+1     , ESSAI , DY   )
                ENDIF
C
C
        END
