      SUBROUTINE INSINI ( TYPESS, ESSAI, MOD, IMAT, NMAT,
     &                      MATERF, TEMPD, YD,  DEPS, DY  )
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
C   NADAI_BETON : CALCUL SOLUTION ESSAI DY = ( DSIG  DP (DEPS3))
C                               AVEC     Y  = ( SIG  P  (EPS3))
C       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
C           MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TEMPD  :  TEMPERATURE A T
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
        INTEGER         IMAT, NDT , NDI , TYPESS , NMAT
        REAL*8          UN  , ZERO
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
C
        REAL*8          YD(*)     , DY(*),  ESSAI
        REAL*8          HOOK(6,6) , DFDS(6)
        REAL*8          DEPS(6)   , DEPSM(6)
        REAL*8          SIG(6)    , DSIG(6)
        REAL*8          VTMP(6)   , VTMP1(6), VTMP2(6)
        REAL*8          XX,  YY, DFDK , NUN
        REAL*8          KAPA, TAU, HP, R0, DP,  DEV(6)
        REAL*8          LCS  , KPIC , KRUP
        REAL*8          SEUIL, I1, J2, J3, RCOS3T
C
        REAL*8          MATERF(NMAT,2) , TEMPD
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
C
        IF ( TYPESS .EQ. -1 ) TYPESS = 2
        CALL LCEQVN ( NDT , YD(1)       , SIG )
        KAPA = YD(NDT+1)
C
        LCS    = MATERF(1,2)
        R0     = MATERF(3,2)
        KPIC   = MATERF(4,2)
        KRUP   = MATERF(5,2)
        NUN = MATERF(2,1) / (UN-MATERF(2,1))
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
C
C - SOLUTION INITIALE = NUL
C
                IF ( TYPESS .EQ. 0 .OR. TYPESS .EQ. 1 ) THEN
                CALL LCTRMA (HOOK , HOOK)
                CALL LCINVN( NDT+2 , ZERO , DY )
                  IF(MOD(1:6).EQ.'C_PLAN')THEN
                  DEPS(3) = ZERO
                  ENDIF
C
C - SOLUTION INITIALE = EXPLICITE
C
                ELSEIF ( TYPESS .EQ. 2 ) THEN
C - S
C
      CALL INSINV ( SIG , I1, J2, J3, RCOS3T)
      CALL INSISO (LCS, KPIC, R0, KRUP, KAPA, HP, TAU)
      CALL INSNAD ( I1 , J2 , TAU , SEUIL )
C
C - DP
        IF ( KAPA .EQ. ZERO) THEN
                CALL LCPRMV ( HOOK  , DEPS , SIG )
        ENDIF
C
      CALL INSFS (SIG , DFDS  , DEV )
      DFDK = -HP
      CALL LCPRMV ( HOOK  , DEPS , VTMP1)
      CALL LCPRSC ( DFDS  , VTMP1, YY    )
      CALL LCPRMV ( HOOK  , DFDS , VTMP2 )
      CALL LCPRSC ( DFDS  , VTMP2, XX    )
      XX = XX - DFDK
      DP = YY / XX
      IF( DP .LT. 0.D0) THEN
        DP = 0.D0
      ENDIF
C
C - (DEPS(3))
                IF(MOD(1:6).EQ.'C_PLAN')THEN
                DEPS(3) = NUN * (DP*(DFDS(1)+DFDS(2))-DEPS(1)-DEPS(2))
     &                    + DFDS(3)*DP
                ENDIF
C - DSIG
                CALL LCPRSV ( -DP   , DFDS , VTMP )
                CALL LCSOVE ( DEPS  , VTMP , VTMP )
                CALL LCPRMV ( HOOK  , VTMP , DSIG )
C - DY
                CALL LCEQVN ( NDT , DSIG   , DY(1) )
                DY(NDT+1) = DP
                        IF(MOD(1:6).EQ.'C_PLAN')THEN
                        DY(NDT+2)   = DEPS(3)
                        DY(3)       = ZERO
                        ENDIF
C
C - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
C
                ELSEIF ( TYPESS .EQ. 3 ) THEN
                CALL LCINVN ( NDT+2  , ESSAI , DY )
                        IF ( MOD(1:6).EQ.'C_PLAN' )THEN
                        DEPS(3) = ESSAI
                        DY(3)   = ZERO
                        ENDIF
                ENDIF
C
        END
