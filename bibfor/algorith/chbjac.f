        SUBROUTINE CHBJAC ( MOD, IMAT, NMAT, MATERF, TEMPF,
     1                      YF,   DY,   NMOD,  DRDY)
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
C       CHABOCHE   : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C                    DY    = ( DSIG  DX1   DX2   DP   (DEPS3) )
C                    Y     = ( SIG   X1    X2    P    (EPS3)  )
C                    DRDY  = ( DGDS  DGDX1  DGDX2  DGDP (DGDE3) )
C                            ( DLDS  DLDX1  DLDX2  DLDP (DLDE3) )
C                            ( DJDS  DJDX1  DJDX2  DJDP (DJDE3) )
C                            ( DFDS  DFDX1  DFDX2  DFDP (DFDE3) )
C                            ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDE3) )
C       IN  MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TEMPF  :  TEMPERATURE A T+DT
C           YF     :  VARIABLES A T + DT =  ( SIGF X1F X2F PF (EPS3F) )
C           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
C           NMOD   :  DIMENSION DECLAREE DRDY
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       ----------------------------------------------------------------
        INTEGER         IMAT, NDT , NDI , NMAT , NMOD
        REAL*8          MUN , UN  , ZERO , D23 , D13
        PARAMETER       ( MUN  = -1.D0  )
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
        PARAMETER       ( D23  =  .66666666666666D0 )
        PARAMETER       ( D13  = -.33333333333333D0 )
C
        REAL*8          HOOK(6,6) , DDFDDS(6,6) , DDFDSX(6,6) , I4(6,6)
        REAL*8          ID(6,6)
        REAL*8          SIG(6)    , X1(6) ,       X2(6)
        REAL*8          P         , DP
        REAL*8          YF(*)     , DY(*) , DRDY(NMOD,NMOD)
C
        REAL*8          DGDS(6,6),DGDX1(6,6),DGDX2(6,6),DGDP(6),DGDE3(6)
        REAL*8          DLDS(6,6),DLDX1(6,6),DLDX2(6,6),DLDP(6),DLDE3(6)
        REAL*8          DJDS(6,6),DJDX1(6,6),DJDX2(6,6),DJDP(6),DJDE3(6)
        REAL*8          DFDS(6),  DFDX1(6),  DFDX2(6),  DFDP,   DFDE3
        REAL*8          DQDS(6),  DQDX1(6),  DQDX2(6),  DQDP,   DQDE3
C
        REAL*8          VTMP1(6), VTMP2(6) , MTMP(6,6),  DEDE3(6)
        REAL*8          MATERF(NMAT,2),       TEMPF
        REAL*8          RI,  RO,  B,  K, W, A1, A2, C1, C2, NU
        REAL*8          H1,  H2,  H3, H4
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        REAL*8          CHBCIN
C       ----------------------------------------------------------------
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
        DP = DY(3*NDT+1)
C
        RI = MATERF(1,2)
        RO = MATERF(2,2)
        B  = MATERF(3,2)
        K  = MATERF(4,2)
        W  = MATERF(5,2)
        A1 = MATERF(6,2)
        A2 = MATERF(7,2)
        C1 = MATERF(8,2)
        C2 = MATERF(9,2)
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
        CALL CHBFS   ( SIG, X1,   X2, DFDS )
        CALL CHBFSS  ( SIG, X1,   X2, ID, DDFDDS )
        CALL CHBFSX  ( SIG, X1,   X2, I4, DDFDSX )
        H1 = -C1 * A1 * CHBCIN(K,W,P) * D23
        H2 = -C2 * A2 * CHBCIN(K,W,P) * D23
        H3 =  C1 * A1 * W * ( K - UN ) * EXP (-W*P) * D23
        H4 =  C2 * A2 * W * ( K - UN ) * EXP (-W*P) * D23
C
C - DGDS(T+DT)
        CALL LCPRMM ( HOOK    , DDFDDS , DGDS )
        CALL LCPRSM ( DP      , DGDS   , DGDS )
        CALL LCSOMA ( I4      , DGDS   , DGDS )
C
C - DGDX1(T+DT)
        CALL LCPRMM ( HOOK    , DDFDSX , DGDX1 )
        CALL LCPRSM ( DP      , DGDX1  , DGDX1 )
C
C - DGDX2(T+DT)
        CALL LCEQMA( DGDX1    , DGDX2 )
C
C - DGDP(T+DT)
        CALL LCPRMV ( HOOK    , DFDS   , DGDP  )
C
C - DLDS(T+DT)
        CALL LCPRSM ( H1*DP   , DDFDDS , DLDS  )
C
C - DLDX1(T+DT)
        CALL LCPRSM ( H1*DP   , DDFDSX , DLDX1 )
        CALL LCPRSM ( C1*DP+UN, I4     , MTMP  )
        CALL LCSOMA ( DLDX1   , MTMP   , DLDX1 )
C
C - DLDX2(T+DT)
        CALL LCPRSM ( H1*DP   , DDFDSX , DLDX2 )
C
C - DLDP(T+DT)
        CALL LCPRSV ( H1      , DFDS   , DLDP  )
        CALL LCPRSV ( C1      , X1     , VTMP1 )
        CALL LCSOVE ( DLDP    , VTMP1  , DLDP  )
        CALL LCPRSV ( H3*DP   , DFDS   , VTMP1 )
        CALL LCSOVE ( DLDP    , VTMP1  , DLDP  )
C
C - DJDS(T+DT)
        CALL LCPRSM ( H2*DP   , DDFDDS , DJDS  )
C
C - DJDX1(T+DT)
        CALL LCPRSM ( H2*DP   , DDFDSX , DJDX1 )
C
C - DJDX2(T+DT)
        CALL LCPRSM ( H2*DP   , DDFDSX , DJDX2 )
        CALL LCPRSM ( C2*DP+UN, I4     , MTMP  )
        CALL LCSOMA ( DJDX2   , MTMP   , DJDX2 )
C
C - DJDP(T+DT)
        CALL LCPRSV ( H2      , DFDS   , DJDP  )
        CALL LCPRSV ( C2      , X2     , VTMP1 )
        CALL LCSOVE ( DJDP    , VTMP1  , DJDP  )
        CALL LCPRSV ( H4*DP   , DFDS   , VTMP1 )
        CALL LCSOVE ( DJDP    , VTMP1  , DJDP  )
C
C - DFDS(T+DT) > DEJA FAIT
C
C - DFDX1(T+DT)
        CALL LCPRSV ( -UN     , DFDS   , DFDX1 )
C
C - DFDX2(T+DT)
        CALL LCEQVE ( DFDX1   , DFDX2 )
C
C - DFDP(T+DT)
        DFDP = - B * ( RI - RO ) * EXP (-B*P)
C
C - CONTRAINTES PLANES -------------------------------------------------
C
          IF( MOD(1:6).EQ.'C_PLAN' ) THEN
C
C - DGDE3(T+DT)
          CALL LCPRMV ( HOOK , DEDE3 , DGDE3 )
C
C - DLDE3(T+DT)
          CALL LCINVE ( ZERO , DLDE3 )
C
C - DJDE3(T+DT)
          CALL LCINVE ( ZERO , DJDE3 )
C
C - DFDE3(T+DT)
          DFDE3   = ZERO
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
          CALL LCEQVE ( DQDX1 , DQDX2 )
C
C - DQDP (T+DT)
          DQDP    = - HOOK(3,1)*DFDS(1) - HOOK(3,2)*DFDS(2)
     &              - HOOK(3,3)*DFDS(3) - HOOK(3,4)*DFDS(4)
C
         ENDIF
C
C - ASSEMBLAGE ---------------------------------------------------------
C
C - DRDY(T+DT)  =  DGDS  DGDX1  DGDX2  DGDP (DGDE3)
C                  DLDS  DLDX1  DLDX2  DLDP (DLDE3)
C                  DJDS  DJDX1  DJDX2  DJDP (DJDE3)
C                  DFDS  DFDX1  DFDX2  DFDP (DFDE3)
C                 (DQDS)(DQDX1)(DQDX2)(DQDP)(DQDE3)
C
        CALL LCICMA (DGDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
        CALL LCICMA (DGDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,NDT+1)
        CALL LCICMA (DGDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,2*NDT+1)
        CALL LCICMA (DGDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,1,3*NDT+1)
C
        CALL LCICMA (DLDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
        CALL LCICMA (DLDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,NDT+1)
        CALL LCICMA (DLDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,2*NDT+1)
        CALL LCICMA (DLDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,NDT+1,3*NDT+1)
C
        CALL LCICMA (DJDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,1)
        CALL LCICMA (DJDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,NDT+1)
       CALL LCICMA(DJDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,2*NDT+1)
       CALL LCICMA(DJDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,2*NDT+1,3*NDT+1)
C
        CALL LCICMA (DFDS, 1,6,1,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,1)
        CALL LCICMA (DFDX1,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,NDT+1)
        CALL LCICMA (DFDX2,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,2*NDT+1)
        DRDY(3*NDT+1,3*NDT+1) = DFDP
C
         IF( MOD(1:6).EQ.'C_PLAN' ) THEN
         CALL LCICMA (DGDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,3*NDT+2)
         CALL LCICMA (DLDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,NDT+1,3*NDT+2)
         CALL LCICMA(DJDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,2*NDT+1,3*NDT+2)
         DRDY(3*NDT+1,3*NDT+2) = DFDE3
         CALL LCICMA (DQDS, 1,6,1,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+2,1)
         CALL LCICMA (DQDX1,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+2,NDT+1)
         CALL LCICMA(DQDX2,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+2,2*NDT+1)
         DRDY(3*NDT+2,3*NDT+1) = DQDP
         DRDY(3*NDT+2,3*NDT+2) = DQDE3
         ENDIF
        END
