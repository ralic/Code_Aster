        SUBROUTINE ONOJAC ( MOD, NMAT, MATERF,
     1                      YF,   DY,   NMOD,  DRDY)
C-------------------------------------------------------------------
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
C   OHNO :  CALCUL DU JACOBIEN
C-------------------------------------------------------------------
        IMPLICIT REAL*8 (A-H,O-Z)
        INTEGER         NDT , NDI , NMAT , NMOD
        REAL*8          MUN , UN  , ZERO , D23 , D13
        PARAMETER       ( MUN  = -1.D0  )
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
        PARAMETER       ( D23  =  .66666666666666D0 )
        PARAMETER       ( D13  = -.33333333333333D0 )
C
        REAL*8      HOOK(6,6) , DDFDDS(6,6) , DDFDSX(6,6) , I4(6,6)
        REAL*8      ID(6,6)
        REAL*8      SIG(6)    , X1(6) ,       X2(6) , X3(6)
        REAL*8      X4(6),  X5(6), P         , DP
        REAL*8      YF(*)     , DY(*) , DRDY(NMOD,NMOD)
C
        REAL*8      DGDS(6,6),DGDX1(6,6), DGDX2(6,6), DGDX3(6,6)
        REAL*8      DGDX4(6,6), DGDX5(6,6), DGDP(6),DGDE3(6)
        REAL*8      DLDS(6,6),DLDX1(6,6), DLDX2(6,6), DLDX3(6,6)
        REAL*8      DLDX4(6,6), DLDX5(6,6),DLDP(6),DLDE3(6)
        REAL*8      DJDS(6,6),DJDX1(6,6), DJDX2(6,6), DJDX3(6,6)
        REAL*8      DJDX4(6,6), DJDX5(6,6), DJDP(6), DJDE3(6)
        REAL*8      DFDS(6),  DFDX1(6),  DFDX2(6),  DFDX3(6)
        REAL*8      DFDX4(6),  DFDX5(6),DFDP,   DFDE3
        REAL*8      DQDS(6),  DQDX1(6),  DQDX2(6), DQDX3(6)
        REAL*8      DQDX4(6), DQDX5(6),  DQDP,   DQDE3
        REAL*8      DIDS(6,6),DIDX1(6,6),DIDX2(6,6), DIDX3(6,6)
        REAL*8      DIDX4(6,6),DIDX5(6,6), DIDP(6),DIDE3(6)
        REAL*8      DMDS(6,6), DMDX1(6,6), DMDX2(6,6), DMDX3(6,6)
        REAL*8      DMDX4(6,6), DMDX5(6,6),DMDP(6),DMDE3(6)
        REAL*8      DNDS(6,6), DNDX1(6,6), DNDX2(6,6), DNDX3(6,6)
        REAL*8      DNDX4(6,6), DNDX5(6,6),DNDP(6),DNDE3(6)
        REAL*8      VTMP1(6), VTMP2(6) , MTMP(6,6),  DEDE3(6)
        REAL*8      MATERF(NMAT,2),  VTMP(6)
        REAL*8      RI,  R0,  B,  PHI, A1, A2, C1, C2, NU
        REAL*8      H1,  H2,  H3, H4, GAMMA1, GAMMA2, M1, M2
        REAL*8      XI1, XI2, H5,H6,H7,H8,H9, NORMX1, NORMX2
        REAL*8      SCR1, SCR2, VT1(6), VT2(6), G1, G2, G3
        REAL*8      SCR3(6), SCR4(6), K1(6), K2(6)
        REAL*8      A3, C3, GAMMA3, M3, XI3, NORMX3,SCR5, SCR6(6)
        REAL*8      G4 ,K3(6), J1, J2, J3, J4 , T1
        REAL*8      A4, C4, GAMMA4, M4, XI4, NORMX4,SCR7, SCR8(6)
        REAL*8      G5 ,K4(6), J5, J6, J7, J8 , T2
        REAL*8      A5, C5, GAMMA5, M5, XI5, NORMX5,SCR9, SCR10(6)
        REAL*8      G6 ,K5(6), J9, J10, J11, J12 , T3
        CHARACTER*8     MOD
        REAL*8          ONOCIN
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
        CALL LCEQVN ( NDT , YF(3*NDT+1) , X3  )
        CALL LCEQVN ( NDT , YF(4*NDT+1) , X4  )
        CALL LCEQVN ( NDT , YF(5*NDT+1) , X5  )
        P  = YF(6*NDT+1)
        DP = DY(6*NDT+1)
C
        RI = MATERF(1,2)
        R0 = MATERF(2,2)
        B  = MATERF(3,2)
        PHI  = MATERF(4,2)
        A1 = MATERF(5,2)
        A2 = MATERF(6,2)
        A3 = MATERF(7,2)
        A4 = MATERF(8,2)
        A5 = MATERF(9,2)
        GAMMA1 = MATERF(10,2)
        GAMMA2 = MATERF(11, 2)
        GAMMA3 = MATERF(12, 2)
        GAMMA4 = MATERF(13, 2)
        GAMMA5 = MATERF(14, 2)
        M1 = MATERF(15,2)
        M2 = MATERF (16,2)
        M3 = MATERF (17,2)
        M4 = MATERF (18,2)
        M5 = MATERF (19,2)
C
        XI1 = A1/(GAMMA1*ONOCIN(PHI, B, P))
        XI2 = A2/(GAMMA2*ONOCIN(PHI,B,P))
        XI3 = A3/(GAMMA3*ONOCIN(PHI,B,P))
        XI4 = A4/(GAMMA4*ONOCIN(PHI,B,P))
        XI5 = A5/(GAMMA5*ONOCIN(PHI,B,P))
C
        CALL ONOFS(SIG,X1, X2, X3, X4, X5, DFDS)
        CALL ONOFSS(SIG,X1, X2, X3, X4, X5, ID, DDFDDS)
C
        CALL LCPRSC ( X1, X1, NORMX1 )
        NORMX1 = SQRT (1.5D0 * NORMX1)
        CALL LCPRSN (NDT, DFDS, X1, SCR1)
        CALL LCPRMV (DDFDDS, X1, SCR3)
C
        CALL LCPRSC ( X2, X2, NORMX2 )
        NORMX2 = SQRT (1.5D0 * NORMX2)
        CALL LCPRSN (NDT, DFDS, X2, SCR2)
        CALL LCPRMV (DDFDDS, X2, SCR4)
C
        CALL LCPRSC ( X3, X3, NORMX3 )
        NORMX3 = SQRT (1.5D0 * NORMX3)
        CALL LCPRSN (NDT, DFDS, X3, SCR5)
        CALL LCPRMV (DDFDDS, X3, SCR6)
C
        CALL LCPRSC ( X4, X4, NORMX4 )
        NORMX4 = SQRT (1.5D0 * NORMX4)
        CALL LCPRSN (NDT, DFDS, X4, SCR7)
        CALL LCPRMV (DDFDDS, X4, SCR8)
C
        CALL LCPRSC ( X5, X5, NORMX5 )
        NORMX5 = SQRT (1.5D0 * NORMX5)
        CALL LCPRSN (NDT, DFDS, X5, SCR9)
        CALL LCPRMV (DDFDDS, X5, SCR10)
C
        IF (NORMX1.NE.0.D0) THEN
        C1 =GAMMA1*ONOCIN(PHI,B,P)*(1.D0/XI1)**M1*(NORMX1**(M1-UN))
        C1 = C1*SCR1
        G2 = GAMMA1*ONOCIN(PHI,B,P)*(1.D0/XI1)**M1*(NORMX1**(M1-UN))
        ELSE
        C1 = 0.D0
        G2 = 0.D0
        ENDIF
        CALL LCPRSV(G2*DP, SCR3, K1)
C
        IF (NORMX2.NE.0.D0) THEN
        C2 =GAMMA2*ONOCIN(PHI,B,P)*(1.D0/XI2)**M2*(NORMX2**(M2-UN))
        C2 = C2*SCR2
        G3 = GAMMA2*ONOCIN(PHI,B,P)*(1.D0/XI2)**M2*(NORMX2**(M2-UN))
        ELSE
        C2 = 0.D0
        G3 = 0.D00
        ENDIF
        CALL LCPRSV(G3*DP, SCR4, K2)
C
        IF (NORMX3.NE.0.D0) THEN
        C3 =GAMMA3*ONOCIN(PHI,B,P)*(1.D0/XI3)**M3*(NORMX3**(M3-UN))
        C3 = C3*SCR5
        G4 = GAMMA3*ONOCIN(PHI,B,P)*(1.D0/XI3)**M3*(NORMX3**(M3-UN))
        ELSE
        C3 = 0.D0
        G4 = 0.D00
        ENDIF
        CALL LCPRSV(G4*DP, SCR6, K3)
C
        IF (NORMX4.NE.0.D0) THEN
        C4 =GAMMA4*ONOCIN(PHI,B,P)*(1.D0/XI4)**M4*(NORMX4**(M4-UN))
        C4 = C4*SCR7
        G5 = GAMMA4*ONOCIN(PHI,B,P)*(1.D0/XI4)**M4*(NORMX4**(M4-UN))
        ELSE
        C4 = 0.D0
        G5 = 0.D00
        ENDIF
        CALL LCPRSV(G5*DP, SCR8, K4)
C
        IF (NORMX5.NE.0.D0) THEN
        C5 =GAMMA5*ONOCIN(PHI,B,P)*(1.D0/XI5)**M5*(NORMX5**(M5-UN))
        C5 = C5*SCR9
        G6 = GAMMA5*ONOCIN(PHI,B,P)*(1.D0/XI5)**M5*(NORMX5**(M5-UN))
        ELSE
        C5 = 0.D0
        G6 = 0.D00
        ENDIF
        CALL LCPRSV(G6*DP, SCR10, K5)
C
        CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
        CALL ONOFS   ( SIG, X1,   X2, X3, X4, X5, DFDS )
        CALL ONOFSS  ( SIG, X1,   X2, X3, X4, X5, ID, DDFDDS )
        CALL ONOFSX  ( SIG, X1,   X2, X3, X4, X5, I4, DDFDSX )
C
        H1 = - A1 * D23
        H2 = - A2 * D23
        H3 = GAMMA1*((1.D0/XI1)**M1) * B*(UN -PHI )*EXP (-B*P)
        H4 = GAMMA2 *((1.D0/XI2)**M2)* B*( UN -PHI )*EXP (-B*P)
        H5 = GAMMA1*ONOCIN(PHI,B,P)*M1*(1.D0/XI1)**M1-1
        H6= -H5*(GAMMA1/A1)*B*(UN-PHI)*EXP(-B*P)
        H7 = GAMMA2*ONOCIN(PHI,B,P)*M2*(1.D0/XI2)**M2-1
        H8= -H7*(GAMMA2/A2)*B*(UN-PHI)*EXP(-B*P)
C
        J1 = -A3 * D23
        J2 = GAMMA3*((1.D0/XI3)**M3) * B*(UN -PHI )*EXP (-B*P)
        J3 = GAMMA3*ONOCIN(PHI,B,P)*M3*(1.D0/XI3)**M3-1
        J4 = -J3*(GAMMA3/A3)*B*(UN-PHI)*EXP(-B*P)
        J5 = -A4 * D23
        J6 = GAMMA4*((1.D0/XI4)**M4) * B*(UN -PHI )*EXP (-B*P)
        J7 = GAMMA4*ONOCIN(PHI,B,P)*M4*(1.D0/XI4)**M4-1
        J8= -J7*(GAMMA4/A4)*B*(UN-PHI)*EXP(-B*P)
        J9 = -A5 * D23
        J10 = GAMMA5*((1.D0/XI5)**M5) * B*(UN -PHI )*EXP (-B*P)
        J11 = GAMMA5*ONOCIN(PHI,B,P)*M5*(1.D0/XI5)**M5-1
        J12= -J11*(GAMMA5/A5)*B*(UN-PHI)*EXP(-B*P)
C
C - DGDS(T+DT)
        CALL LCPRMM ( HOOK    , DDFDDS , DGDS )
        CALL LCPRSM ( DP      , DGDS   , DGDS )
        CALL LCSOMA ( I4      , DGDS   , DGDS )
C - DGDX1(T+DT)
        CALL LCPRMM ( HOOK    , DDFDSX , DGDX1 )
        CALL LCPRSM ( DP      , DGDX1  , DGDX1 )
C - DGDX2(T+DT)
        CALL LCEQMA ( DGDX1    , DGDX2 )
C - DGDX3(T+DT)
        CALL LCEQMA ( DGDX1    , DGDX3 )
C - DGDX4(T+DT)
        CALL LCEQMA ( DGDX1    , DGDX4 )
C - DGDX5(T+DT)
        CALL LCEQMA ( DGDX1    , DGDX5 )
C - DGDP(T+DT)
        CALL LCPRMV ( HOOK    , DFDS   , DGDP  )
C - DLDS(T+DT)
        CALL LCPRSM ( H1*DP   , DDFDDS , DLDS  )
        CALL LCPRTE (K1, X1, MTMP)
        CALL LCSOMA (DLDS, MTMP, DLDS)
C - DLDX1(T+DT)
        CALL LCPRSM ( H1*DP   , DDFDSX , DLDX1 )
        CALL LCPRSM ( C1*DP+UN, I4     , MTMP  )
        CALL LCSOMA ( DLDX1   , MTMP   , DLDX1 )
        IF (NORMX1.EQ.0.D0) THEN
           H9 = 0.D0
        ELSE
           H9 =1.5D0* (M1- UN)/(NORMX1**2)
        END IF
        CALL LCPRSV (H9*DP,  X1,  VTMP1)
        CALL LCPRSV (C1, VTMP1, VTMP1 )
        CALL LCPRTE (VTMP1 , X1, MTMP)
        CALL LCSOMA ( DLDX1, MTMP, DLDX1)
        CALL LCPRMV (I4,  DFDS, VTMP)
        CALL LCPRMV ( DDFDSX, X1, VT1)
        CALL LCSOVE (VTMP, VT1, VT2)
        CALL LCPRSV (G2*DP, X1, VT1)
        CALL LCPRTE(VT2, VT1, MTMP)
        CALL LCSOMA (MTMP, DLDX1, DLDX1)
C - DLDX2(T+DT)
        CALL LCPRSM ( H1*DP   , DDFDSX , DLDX2 )
        CALL LCPRMV (DDFDSX, X1, VT1)
        CALL LCPRSV (G2*DP, X1, VTMP)
        CALL LCPRTE ( VTMP, VT1, MTMP)
        CALL LCSOMA ( DLDX2, MTMP, DLDX2)
C - DLDX3(T+DT)
        CALL LCEQMA ( DLDX2    , DLDX3 )
C - DLDX4(T+DT)
        CALL LCEQMA ( DLDX2    , DLDX4 )
C - DLDX5(T+DT)
        CALL LCEQMA ( DLDX2    , DLDX5 )
C - DLDP(T+DT)
        CALL LCPRSV ( H1      , DFDS   , DLDP  )
        CALL LCPRSV ( C1      , X1     , VTMP1 )
        CALL LCSOVE ( DLDP    , VTMP1  , DLDP  )
        CALL LCPRSV ( H3*DP   , X1   , VTMP1 )
        CALL LCPRSV (H6*DP    ,X1, VTMP2)
        CALL LCSOVE (VTMP1, VTMP2, VTMP1)
        CALL LCSOVE ( DLDP    , VTMP1  , DLDP  )
C - DJDS(T+DT)
        CALL LCPRSM ( H2*DP   , DDFDDS , DJDS  )
        CALL LCPRTE (K2,  X2,  MTMP)
        CALL LCSOMA ( DLDS, MTMP, DLDS)
C - DJDX1(T+DT)
        CALL LCPRSM ( H2*DP   , DDFDSX , DJDX1 )
        CALL LCPRMV (DDFDSX, X2, VT1)
        CALL LCPRSV (G2*DP, X2, VTMP)
        CALL LCPRTE ( VTMP, VT1, MTMP)
        CALL LCSOMA ( DJDX1, MTMP, DJDX1)
C - DJDX2(T+DT)
        CALL LCPRSM ( H2*DP   , DDFDSX , DJDX2 )
        CALL LCPRSM ( C2*DP+UN, I4     , MTMP  )
        CALL LCSOMA ( DJDX2   , MTMP   , DJDX2 )
        IF (NORMX2.EQ.0.D0) THEN
           G1 = 0.D0
        ELSE
           G1 =1.5D0* (M2- UN)/(NORMX2**2)
        END IF
        CALL LCPRSV (G1*DP,  X2,  VTMP1)
        CALL LCPRSV (C2, VTMP1, VTMP1 )
        CALL LCPRTE (VTMP1 , X2, MTMP)
        CALL LCSOMA ( DJDX2, MTMP, DJDX2)
        CALL LCPRMV (I4,  DFDS, VTMP)
        CALL LCPRMV ( DDFDSX, X2, VT1)
        CALL LCSOVE (VTMP, VT1, VT2)
        CALL LCPRSV (G3*DP, X2, VT1)
        CALL LCPRTE(VT2, VT1, MTMP)
        CALL LCSOMA (MTMP, DJDX2, DJDX2)
C - DJDX3(T+DT)
        CALL LCEQMA ( DJDX1    , DJDX3 )
C - DJDX4(T+DT)
        CALL LCEQMA ( DJDX1    , DJDX4 )
C - DJDX5(T+DT)
        CALL LCEQMA ( DJDX1    , DJDX5 )
C - DJDP(T+DT)
        CALL LCPRSV ( H2      , DFDS   , DJDP  )
        CALL LCPRSV ( C2      , X2     , VTMP1 )
        CALL LCSOVE ( DJDP    , VTMP1  , DJDP  )
        CALL LCPRSV ( H4*DP   , X2   , VTMP1 )
        CALL LCPRSV (H8*DP , X2,  VTMP2)
        CALL LCSOVE (VTMP1, VTMP2, VTMP1)
        CALL LCSOVE ( DJDP    , VTMP1  , DJDP  )
C - DIDS(T+DT)
        CALL LCPRSM ( J1*DP   , DDFDDS , DIDS  )
        CALL LCPRTE (K3,  X3,  MTMP)
        CALL LCSOMA ( DIDS, MTMP, DIDS)
C - DIDX1(T+DT)
        CALL LCPRSM ( J1*DP   , DDFDSX , DIDX1 )
        CALL LCPRMV (DDFDSX, X3, VT1)
        CALL LCPRSV (G4*DP, X3, VTMP)
        CALL LCPRTE ( VTMP, VT1, MTMP)
        CALL LCSOMA ( DIDX1, MTMP, DIDX1)
C - DIDX2(T+DT)
        CALL LCEQMA ( DIDX1    , DIDX2 )
C - DIDX3(T+DT)
        CALL LCPRSM ( J1*DP   , DDFDSX , DIDX3 )
        CALL LCPRSM ( C3*DP+UN, I4     , MTMP  )
        CALL LCSOMA ( DIDX3   , MTMP   , DIDX3 )
        IF (NORMX3.EQ.0.D0) THEN
           T1 = 0.D0
        ELSE
           T1 =1.5D0* (M3- UN)/(NORMX3**2)
        END IF
        CALL LCPRSV (T1*DP,  X3,  VTMP1)
        CALL LCPRSV (C3, VTMP1, VTMP1 )
        CALL LCPRTE (VTMP1 , X3, MTMP)
        CALL LCSOMA ( DIDX3, MTMP, DIDX3)
        CALL LCPRMV (I4,  DFDS, VTMP)
        CALL LCPRMV ( DDFDSX, X3, VT1)
        CALL LCSOVE (VTMP, VT1, VT2)
        CALL LCPRSV (G4*DP, X3, VT1)
        CALL LCPRTE(VT2, VT1, MTMP)
       CALL LCSOMA (MTMP, DIDX3, DIDX3)
C - DIDX4(T+DT)
        CALL LCEQMA ( DIDX1    , DIDX4 )
C - DIDX5(T+DT)
        CALL LCEQMA ( DIDX1    , DIDX5 )
C - DIDP(T+DT)
        CALL LCPRSV ( J1      , DFDS   , DIDP  )
        CALL LCPRSV ( C3      , X3     , VTMP1 )
        CALL LCSOVE ( DIDP    , VTMP1  , DIDP  )
        CALL LCPRSV ( J2*DP   , X3   , VTMP1 )
        CALL LCPRSV (J4*DP , X3,  VTMP2)
        CALL LCSOVE (VTMP1, VTMP2, VTMP1)
        CALL LCSOVE ( DIDP    , VTMP1  , DIDP  )
C - DMDS(T+DT)
        CALL LCPRSM ( J5*DP   , DDFDDS , DMDS  )
        CALL LCPRTE (K4,  X4,  MTMP)
        CALL LCSOMA ( DMDS, MTMP, DMDS)
C - DMDX1(T+DT)
        CALL LCPRSM ( J5*DP   , DDFDSX , DMDX1 )
        CALL LCPRMV (DDFDSX, X4, VT1)
        CALL LCPRSV (G5*DP, X4, VTMP)
        CALL LCPRTE ( VTMP, VT1, MTMP)
        CALL LCSOMA ( DMDX1, MTMP, DMDX1)
C - DMDX2(T+DT)
        CALL LCEQMA ( DMDX1    , DMDX2 )
C - DMDX3(T+DT)
        CALL LCEQMA ( DMDX1    , DMDX3 )
C - DMDX4(T+DT)
        CALL LCPRSM ( J5*DP   , DDFDSX , DMDX4 )
        CALL LCPRSM ( C4*DP+UN, I4     , MTMP  )
        CALL LCSOMA ( DMDX4   , MTMP   , DMDX4 )
        IF (NORMX4.EQ.0.D0) THEN
           T2 = 0.D0
        ELSE
           T2 =1.5D0* (M4- UN)/(NORMX4**2)
        END IF
        CALL LCPRSV (T2*DP,  X4,  VTMP1)
        CALL LCPRSV (C4, VTMP1, VTMP1 )
        CALL LCPRTE (VTMP1 , X4, MTMP)
        CALL LCSOMA ( DMDX4, MTMP, DMDX4)
        CALL LCPRMV (I4,  DFDS, VTMP)
        CALL LCPRMV ( DDFDSX, X4, VT1)
        CALL LCSOVE (VTMP, VT1, VT2)
        CALL LCPRSV (G5*DP, X4, VT1)
        CALL LCPRTE(VT2, VT1, MTMP)
       CALL LCSOMA (MTMP, DMDX4, DMDX4)
C - DMDX5(T+DT)
        CALL LCEQMA ( DMDX1    , DMDX5 )
C - DMDP(T+DT)
        CALL LCPRSV ( J5      , DFDS   , DMDP  )
        CALL LCPRSV ( C4      , X4     , VTMP1 )
        CALL LCSOVE ( DMDP    , VTMP1  , DMDP  )
        CALL LCPRSV ( J6*DP   , X4   , VTMP1 )
        CALL LCPRSV (J8*DP , X4,  VTMP2)
        CALL LCSOVE (VTMP1, VTMP2, VTMP1)
        CALL LCSOVE ( DMDP    , VTMP1  , DMDP  )
C - DNDS(T+DT)
        CALL LCPRSM ( J9*DP   , DDFDDS , DNDS  )
      CALL LCPRTE (K5, X5, MTMP)
      CALL LCSOMA (DNDS, MTMP, DNDS)
C - DNDX1(T+DT)
        CALL LCPRSM ( J9*DP   , DDFDSX , DNDX1 )
        CALL LCPRMV (DDFDSX, X5, VT1)
        CALL LCPRSV (G6*DP, X5, VTMP)
        CALL LCPRTE ( VTMP, VT1, MTMP)
        CALL LCSOMA ( DNDX1, MTMP, DNDX1)
C - DNDX2(T+DT)
        CALL LCEQMA ( DNDX1    , DNDX2 )
C - DNDX3(T+DT)
        CALL LCEQMA ( DNDX1    , DNDX3 )
C - DNDX4(T+DT)
        CALL LCEQMA ( DNDX1    , DNDX4 )
C - DNDX5(T+DT)
        CALL LCPRSM ( J9*DP   , DDFDSX , DNDX5 )
        CALL LCPRSM ( C5*DP+UN, I4     , MTMP  )
        CALL LCSOMA ( DNDX5   , MTMP   , DNDX5 )
        IF (NORMX5.EQ.0.D0) THEN
           T3 = 0.D0
        ELSE
           T3 =1.5D0*(M5- UN)/(NORMX5**2)
        END IF
        CALL LCPRSV (T3*DP,  X5,  VTMP1)
        CALL LCPRSV (C5, VTMP1, VTMP1 )
        CALL LCPRTE (VTMP1 , X5, MTMP)
        CALL LCSOMA ( DNDX5, MTMP, DNDX5)
        CALL LCPRMV (I4,  DFDS, VTMP)
        CALL LCPRMV ( DDFDSX, X5, VT1)
        CALL LCSOVE (VTMP, VT1, VT2)
        CALL LCPRSV (G6*DP, X5, VT1)
        CALL LCPRTE(VT2, VT1, MTMP)
        CALL LCSOMA (MTMP, DLDX1, DLDX1)
C - DNDP(T+DT)
        CALL LCPRSV ( J9      , DFDS   , DNDP  )
        CALL LCPRSV ( C5      , X5     , VTMP1 )
        CALL LCSOVE ( DNDP    , VTMP1  , DNDP  )
        CALL LCPRSV ( J10*DP   , X5   , VTMP1 )
        CALL LCPRSV (J12*DP    ,X5, VTMP2)
        CALL LCSOVE (VTMP1, VTMP2, VTMP1)
        CALL LCSOVE ( DNDP    , VTMP1  , DNDP  )
C - DFDS(T+DT) > DEJA FAIT
C - DFDX1(T+DT)
        CALL LCPRSV ( -UN     , DFDS   , DFDX1 )
C - DFDX2(T+DT)
        CALL LCEQVE ( DFDX1   , DFDX2 )
C - DFDX3(T+DT)
        CALL LCEQVE ( DFDX1   , DFDX3 )
C - DFDX4(T+DT)
        CALL LCEQVE ( DFDX1   , DFDX4 )
C - DFDX5(T+DT)
        CALL LCEQVE ( DFDX1   , DFDX5 )
C - DFDP(T+DT)
        DFDP = - B * ( RI - R0 ) * EXP (-B*P)
C - CONTRAINTES PLANES -------------------------------------------------
          IF( MOD(1:6).EQ.'C_PLAN' ) THEN
C - DGDE3(T+DT)
          CALL LCPRMV ( HOOK , DEDE3 , DGDE3 )
C - DLDE3(T+DT)
          CALL LCINVE ( ZERO , DLDE3 )
C - DJDE3(T+DT)
          CALL LCINVE ( ZERO , DJDE3 )
C - DIDE3(T+DT)
          CALL LCINVE ( ZERO , DIDE3 )
C - DMDE3(T+DT)
          CALL LCINVE ( ZERO , DMDE3 )
C - DNDE3(T+DT)
          CALL LCINVE ( ZERO , DNDE3 )
C - DFDE3(T+DT)
          DFDE3   = ZERO
C - DQDE3(T+DT)
          DQDE3   = HOOK(3,3)
C - DQDS (T+DT)
          DQDS(1) = - DP*(HOOK(3,3)*DDFDDS(3,1) + HOOK(3,1)*DDFDDS(1,1)+
     &                    HOOK(3,2)*DDFDDS(2,1) + HOOK(3,4)*DDFDDS(4,1))
          DQDS(2) = - DP*(HOOK(3,3)*DDFDDS(3,2) + HOOK(3,1)*DDFDDS(1,2)+
     &                    HOOK(3,2)*DDFDDS(2,2) + HOOK(3,4)*DDFDDS(4,2))
          DQDS(3) = - DP*(HOOK(3,3)*DDFDDS(3,3) + HOOK(3,1)*DDFDDS(1,3)+
     &                    HOOK(3,2)*DDFDDS(2,3) + HOOK(3,4)*DDFDDS(4,3))
          DQDS(4) = - DP*(HOOK(3,3)*DDFDDS(3,4) + HOOK(3,1)*DDFDDS(1,4)+
     &                    HOOK(3,2)*DDFDDS(2,4) + HOOK(3,4)*DDFDDS(4,4))
C - DQDX1 (T+DT)
          DQDX1(1)= - DP*(HOOK(3,3)*DDFDSX(3,1) + HOOK(3,1)*DDFDSX(1,1)+
     &                    HOOK(3,2)*DDFDSX(2,1) + HOOK(3,4)*DDFDSX(4,1))
          DQDX1(2)= - DP*(HOOK(3,3)*DDFDSX(3,2) + HOOK(3,1)*DDFDSX(1,2)+
     &                    HOOK(3,2)*DDFDSX(2,2) + HOOK(3,4)*DDFDSX(4,2))
          DQDX1(3)= - DP*(HOOK(3,3)*DDFDSX(3,3) + HOOK(3,1)*DDFDSX(1,3)+
     &                    HOOK(3,2)*DDFDSX(2,3) + HOOK(3,4)*DDFDSX(4,3))
          DQDX1(4)= - DP*(HOOK(3,3)*DDFDSX(3,4) + HOOK(3,1)*DDFDSX(1,4)+
     &                    HOOK(3,2)*DDFDSX(2,4) + HOOK(3,4)*DDFDSX(4,4))
C - DQDX2 (T+DT)
          CALL LCEQVE ( DQDX1 , DQDX2 )
C - DQDX3 (T+DT)
          CALL LCEQVE ( DQDX1 , DQDX3 )
C - DQDX4 (T+DT)
          CALL LCEQVE ( DQDX1 , DQDX4 )
C - DQDX5 (T+DT)
          CALL LCEQVE ( DQDX1 , DQDX5 )
C - DQDP (T+DT)
          DQDP    = - HOOK(3,1)*DFDS(1) - HOOK(3,2)*DFDS(2)
     &              - HOOK(3,3)*DFDS(3) - HOOK(3,4)*DFDS(4)
         ENDIF
C - ASSEMBLAGE ---------------------------------------------------------
C
        CALL LCICMA (DGDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
        CALL LCICMA (DGDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,NDT+1)
        CALL LCICMA (DGDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,2*NDT+1)
        CALL LCICMA (DGDX3,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,3*NDT+1)
        CALL LCICMA (DGDX4,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,4*NDT+1)
        CALL LCICMA (DGDX5,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,5*NDT+1)
        CALL LCICMA (DGDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,1,6*NDT+1)
        CALL LCICMA (DLDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
        CALL LCICMA (DLDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,NDT+1)
        CALL LCICMA (DLDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,2*NDT+1)
        CALL LCICMA (DLDX3,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,3*NDT+1)
        CALL LCICMA (DLDX4,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,4*NDT+1)
        CALL LCICMA (DLDX5,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,5*NDT+1)
        CALL LCICMA (DLDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,NDT+1,6*NDT+1)

        CALL LCICMA (DJDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,1)
        CALL LCICMA (DJDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,NDT+1)
       CALL LCICMA(DJDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,2*NDT+1)
       CALL LCICMA(DJDX3,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,3*NDT+1)
       CALL LCICMA(DJDX4,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,4*NDT+1)
       CALL LCICMA(DJDX5,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+1,5*NDT+1)
       CALL LCICMA(DJDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,2*NDT+1,6*NDT+1)
C
        CALL LCICMA (DIDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,1)
        CALL LCICMA (DIDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,NDT+1)
       CALL LCICMA(DIDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,2*NDT+1)
       CALL LCICMA(DIDX3,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,3*NDT+1)
       CALL LCICMA(DIDX4,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,4*NDT+1)
       CALL LCICMA(DIDX5,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,3*NDT+1,5*NDT+1)
       CALL LCICMA(DIDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,3*NDT+1,6*NDT+1)

        CALL LCICMA(DMDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,1)
        CALL LCICMA(DMDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,NDT+1)
       CALL LCICMA(DMDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,2*NDT+1)
       CALL LCICMA(DMDX3,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,3*NDT+1)
       CALL LCICMA(DMDX4,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,4*NDT+1)
       CALL LCICMA(DMDX5,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,4*NDT+1,5*NDT+1)
       CALL LCICMA(DMDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,4*NDT+1,6*NDT+1)

        CALL LCICMA (DNDS, 6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,5*NDT+1,1)
        CALL LCICMA (DNDX1,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,5*NDT+1,NDT+1)
       CALL LCICMA(DNDX2,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,5*NDT+1,2*NDT+1)
       CALL LCICMA(DNDX3,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,5*NDT+1,3*NDT+1)
       CALL LCICMA(DNDX4,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,5*NDT+1,4*NDT+1)
       CALL LCICMA(DNDX5,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,5*NDT+1,5*NDT+1)
       CALL LCICMA(DNDP, 6,1,NDT,1,  1,1,DRDY,NMOD,NMOD,5*NDT+1,6*NDT+1)

        CALL LCICMA (DFDS, 1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+1,1)
        CALL LCICMA (DFDX1,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+1,NDT+1)
        CALL LCICMA (DFDX2,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+1,2*NDT+1)
        CALL LCICMA (DFDX3,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+1,3*NDT+1)
        CALL LCICMA (DFDX4,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+1,4*NDT+1)
        CALL LCICMA (DFDX5,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+1,5*NDT+1)
        DRDY(6*NDT+1,6*NDT+1) = DFDP
C
         IF( MOD(1:6).EQ.'C_PLAN' ) THEN
         CALL LCICMA (DGDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,6*NDT+2)
         CALL LCICMA (DLDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,NDT+1,6*NDT+2)
         CALL LCICMA(DJDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,2*NDT+1,6*NDT+2)
         CALL LCICMA(DIDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,3*NDT+1,6*NDT+2)
         CALL LCICMA(DMDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,4*NDT+1,6*NDT+2)
         CALL LCICMA(DNDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,5*NDT+1,6*NDT+2)
         DRDY(6*NDT+1,6*NDT+2) = DFDE3
         CALL LCICMA (DQDS, 1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+2,1)
         CALL LCICMA (DQDX1,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+2,NDT+1)
         CALL LCICMA(DQDX2,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+2,2*NDT+1)
         CALL LCICMA(DQDX3,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+2,3*NDT+1)
         CALL LCICMA(DQDX4,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+2,4*NDT+1)
         CALL LCICMA(DQDX5,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,6*NDT+2,5*NDT+1)
         DRDY(6*NDT+2,6*NDT+1) = DQDP
         DRDY(6*NDT+2,6*NDT+2) = DQDE3
         ENDIF
        END
