       SUBROUTINE INSJAC ( MOD, NMAT, MATERF,
     1                      YF,   DY,   NMOD,  DRDY)
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
C       NADAI_BETON   : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE DRDY
C                    DY    = ( DSIG  DP     (DEPS3) )
C                    Y     = ( SIG   KAPA   (EPS3)  )
C                    DRDY  = ( DGDS  DGDP   (DGDE3) )
C                            ( DFDS  DFDP   (DFDE3) )
C                            ((DQDS)(DQDP)  (DQDE3) )
C
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YF     :  VARIABLES A T + DT =  ( SIG  KAPA  (EPS3F) )
C           DY     :  SOLUTION           =  ( DSIG DP    (DEPS3) )
C           NMOD   :  DIMENSION DECLAREE DRDY
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT , NMOD
        REAL*8          MUN , UN  , ZERO
        PARAMETER       ( MUN  = -1.D0  )
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
C
        REAL*8          HOOK(6,6) , I4(6,6) , DDFDDS(6,6)
        REAL*8          SIG(6)    , DEV(6)
        REAL*8          KAPA      , DEDE3(6)
        REAL*8          YF(*)     , DY(*) , DRDY(NMOD,NMOD)
C
        REAL*8          DGDS(6,6), DGDP(6),  DGDE3(6)
        REAL*8          DFDS(6)  , DFDP   ,  DFDE3
        REAL*8          DQDS(6)  , DQDP   ,  DQDE3
C
        REAL*8          MATERF(NMAT,2)
        REAL*8          DP , LCS, KPIC, HP, TAU , R0 , KRUP
        REAL*8          H1 , H2 , H3 , H4
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
        DATA DEDE3      / ZERO  , ZERO  , MUN   , ZERO , ZERO , ZERO/
        DATA  I4        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
C       ----------------------------------------------------------------
C
        CALL LCEQVN ( NDT , YF(1)       , SIG )
        DP = DY(NDT+1)
        KAPA = YF(NDT+1)
        LCS     = MATERF(1,2)
        R0      = MATERF(3,2)
        KPIC    = MATERF(4,2)
        KRUP    = MATERF(5,2)
C
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
        CALL INSISO (LCS, KPIC, R0, KRUP, KAPA, HP, TAU)
        CALL LCINVN ( 6 , 0.D0 , DFDS )
        CALL INSFS  ( SIG , DFDS  , DEV )
        CALL INSFSS ( SIG , DDFDDS  )
C
C - DGDS(T+DT)
        CALL LCPRMM ( HOOK    , DDFDDS , DGDS )
        CALL LCPRSM ( DP      , DGDS   , DGDS )
        CALL LCSOMA ( I4      , DGDS   , DGDS )
C
C - DGDP(T+DT)
        CALL LCINVN ( 6 , 0.D0 , DGDP )
        CALL LCPRMV ( HOOK    , DFDS   , DGDP  )
C
C - DFDS(T+DT) > DEJA FAIT
C
C
C - DFDP(T+DT)
        DFDP = -HP
C
C - CONTRAINTES PLANES -----------------------------------------------
C
        IF( MOD(1:6) .EQ. 'C_PLAN' ) THEN
          H1 = HOOK(3,1)
          H2 = HOOK(3,2)
          H3 = HOOK(3,3)
          H4 = HOOK(3,4)
C
C - DGDE3(T+DT)
          CALL LCINVN ( 6    , 0.D0 , DGDE3 )
          CALL LCPRMV ( HOOK , DEDE3 , DGDE3 )
C
C - DFDE3(T+DT)
          DFDE3   = 0.D0
C
C - DQDE3(T+DT)
          DQDE3   = H3
C
C - DQDS (T+DT)
          DQDS(1) = - DP* ( H3*DDFDDS(3,1) + H1*DDFDDS(1,1) +
     1                      H2*DDFDDS(2,1) + H4*DDFDDS(4,1)    )
          DQDS(2) = - DP* ( H3*DDFDDS(3,2) + H1*DDFDDS(1,2) +
     1                      H2*DDFDDS(2,2) + H4*DDFDDS(4,2)    )
          DQDS(3) = - DP* ( H3*DDFDDS(3,3) + H1*DDFDDS(1,3) +
     1                      H2*DDFDDS(2,3) + H4*DDFDDS(4,3)    )
          DQDS(4) = - DP* ( H3*DDFDDS(3,4) + H1*DDFDDS(1,4) +
     1                      H2*DDFDDS(2,4) + H4*DDFDDS(4,4)    )

C
C - DQDP (T+DT)
          DQDP    = - H1*DFDS(1) - H2*DFDS(2)
     &              - H3*DFDS(3) - H4*DFDS(4)
C
         ENDIF
C
C - ASSEMBLAGE ----------------------------------------------------
C
C - DRDY(T+DT)  =  DGDS   DGDP     (DGDE3)
C                  DFDS   DFDP     (DFDE3)
C                 (DQDS) (DQDP)    (DQDE3)
C
        CALL LCICMA (DGDS, 6,6,NDT,NDT,1,1, DRDY,NMOD,NMOD,1,1)
        CALL LCICMA (DGDP, 6,1,NDT,1,  1,1, DRDY,NMOD,NMOD,1,NDT+1)
C
        CALL LCICMA (DFDS, 1,6,1,  NDT,1,1, DRDY,NMOD,NMOD,NDT+1,1)
        DRDY(NDT+1,NDT+1) = DFDP
C
        IF( MOD(1:6).EQ.'C_PLAN' ) THEN
        CALL LCICMA (DGDE3, 6,1,NDT,1,  1,1, DRDY,NMOD,NMOD,1,NDT+2)
        CALL LCICMA (DQDS,  1,6,1,  NDT,1,1, DRDY,NMOD,NMOD,NDT+2,1)
        DRDY(NDT+1,NDT+2) = DFDE3
        DRDY(NDT+2,NDT+2) = DQDE3
        DRDY(NDT+2,NDT+1) = DQDP
        ENDIF
       END
