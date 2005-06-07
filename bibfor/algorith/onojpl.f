        SUBROUTINE ONOJPL ( MOD,NMAT,MATER,SIG,VIN,DSDE )
        IMPLICIT REAL*8 (A-H,O-Z)
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
C       ----------------------------------------------------------------
C       CHABOCHE   :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C                     ELASTO_PLASTIQUE EN VITESSE A T OU T+DT
C       ----------------------------------------------------------------
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           SIG    :  CONTRAINTES
C           VIN    :  VARIABLES INTERNES
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT
        REAL*8          UN , D23
        PARAMETER       ( D23  =  .66666666666666D0 )
        PARAMETER       ( UN   =  1.D0   )
C
        REAL*8       VIN(*)
        REAL*8       SIG(6) ,     X1(6) ,    X2(6)   , P ,  DFDP
        REAL*8       HOOK(6,6),   DSDE(6,6), DFDS(6)
        REAL*8       VTMP(6),     VTMP1(6),  VTMP2(6)
C
        REAL*8       RI,  R0, B, PHI,  A1, A2, C1, C2, GAMMA1, GAMMA2
        REAL*8       MATER(NMAT,2)
        REAL*8       H1 , H2 , H3 , H4 , H5 , D, XI1, XI2, M1, M2
        REAL*8       NORMX1, NORMX2, SCR1, SCR2
        REAL*8       X3(6), X4(6), X5(6)
        REAL*8       A3, C3, GAMMA3, XI3, M3, NORMX3, SCR3, J1, VTMP3(6)
        REAL*8       A4, C4, GAMMA4, XI4, M4, NORMX4, SCR4, J2, VTMP4(6)
        REAL*8       A5, C5, GAMMA5, XI5, M5, NORMX5, SCR5, J3, VTMP5(6)
        CHARACTER*8     MOD
        REAL*8          ONOCIN
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        CALL LCEQVN ( NDT , VIN(1)     ,  X1  )
        CALL LCEQVN ( NDT , VIN(NDT+1) ,  X2  )
        CALL LCEQVN ( NDT , VIN(2*NDT+1) , X3  )
        CALL LCEQVN ( NDT , VIN(3*NDT+1) , X4  )
        CALL LCEQVN ( NDT , VIN(4*NDT+1) , X5  )
        P = VIN(5*NDT+1)
C
        RI = MATER(1,2)
        R0 = MATER(2,2)
        B  = MATER(3,2)
        PHI  = MATER(4,2)
        A1 = MATER(5,2)
        A2 = MATER(6,2)
        A3 = MATER(7,2)
        A4 = MATER(8,2)
        A5 = MATER(9,2)
        GAMMA1 = MATER(10,2)
        GAMMA2= MATER(11,2)
        GAMMA3= MATER(12,2)
        GAMMA4= MATER(13,2)
        GAMMA5= MATER(14,2)
        M1 = MATER (15,2)
        M2 = MATER (16,2)
        M3 = MATER (17,2)
        M4 = MATER (18,2)
        M5 = MATER (19,2)
C
        XI1 = A1/(GAMMA1*ONOCIN(PHI, B, P))
        XI2 = A2/(GAMMA2*ONOCIN(PHI,B,P))
        XI3 = A3/(GAMMA3*ONOCIN(PHI,B,P))
        XI4 = A4/(GAMMA4*ONOCIN(PHI,B,P))
        XI5 = A5/(GAMMA5*ONOCIN(PHI,B,P))
C
        CALL ONOFS(SIG,X1,X2, X3, X4, X5, DFDS)
C
        CALL LCPRSC ( X1, X1, NORMX1 )
        NORMX1 = SQRT (1.5D0 * NORMX1)
        CALL LCPRSN (NDT, DFDS, X1, SCR1)
C
        CALL LCPRSC ( X2, X2, NORMX2 )
        NORMX2 = SQRT (1.5D0 * NORMX2)
        CALL LCPRSN (NDT, DFDS, X2, SCR2)
C
        CALL LCPRSC ( X3, X3, NORMX3 )
        NORMX3 = SQRT (1.5D0 * NORMX3)
        CALL LCPRSN (NDT, DFDS, X3, SCR3)
C
        CALL LCPRSC ( X4, X4, NORMX4 )
        NORMX4 = SQRT (1.5D0 * NORMX4)
        CALL LCPRSN (NDT, DFDS, X4, SCR4)
C
        CALL LCPRSC ( X5, X5, NORMX5 )
        NORMX5 = SQRT (1.5D0 * NORMX5)
        CALL LCPRSN (NDT, DFDS, X5, SCR5)
C
        IF (NORMX1.NE.0.D0) THEN
        C1 = GAMMA1*ONOCIN(PHI,B,P)*(1.D0/XI1)**M1*(NORMX1**(M1-UN))
        C1 = C1*SCR1
        ELSE
        C1 = 0.D0
        ENDIF
C
        IF (NORMX2.NE.0.D0) THEN
        C2 =GAMMA2*ONOCIN(PHI,B,P)*(1.D0/XI2)**M2*(NORMX2**(M2-UN))
        C2 = C2*SCR2
        ELSE
         C2 = 0.D0
        ENDIF
C
        IF (NORMX3.NE.0.D0) THEN
        C3 =GAMMA3*ONOCIN(PHI,B,P)*(1.D0/XI3)**M3*(NORMX3**(M3-UN))
        C3 = C3*SCR3
        ELSE
         C3 = 0.D0
        ENDIF
C
        IF (NORMX4.NE.0.D0) THEN
        C4 =GAMMA4*ONOCIN(PHI,B,P)*(1.D0/XI4)**M4*(NORMX4**(M4-UN))
        C4 = C4*SCR4
        ELSE
         C4 = 0.D0
        ENDIF
C
        IF (NORMX5.NE.0.D0) THEN
        C5 =GAMMA5*ONOCIN(PHI,B,P)*(1.D0/XI5)**M5*(NORMX5**(M5-UN))
        C5 = C5*SCR5
        ELSE
         C5 = 0.D0
        ENDIF
C
       H1  =  A1 *D23
       H2  =  A2 *D23
       J1  =  A3 *D23
       J2  =  A4 *D23
       J3  =  A5 *D23
C
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATER(1,1) , HOOK )
        CALL ONOFS  ( SIG    , X1 , X2, X3, X4, X5,  DFDS )
        CALL LCPRMV ( HOOK   , DFDS  , VTMP1 )
        CALL LCPRSC ( DFDS   , VTMP1 , H3    )
        DFDP =  B * ( R0 - RI ) * EXP(-B*P)
        CALL LCPRSC ( DFDS   , DFDS  , H4    )
        H4 = H4  *  ( H1+ H2 + J1 + J2 + J3 )
C
        CALL LCPRSV ( C1     , X1    , VTMP1 )
        CALL LCPRSV ( C2     , X2    , VTMP2 )
        CALL LCPRSV ( C3     , X3    , VTMP3 )
        CALL LCPRSV ( C4     , X4    , VTMP4 )
        CALL LCPRSV ( C5     , X5    , VTMP5 )
C
        CALL LCSOVE ( VTMP1  , VTMP2 , VTMP  )
        CALL LCSOVE ( VTMP,   VTMP3 , VTMP  )
        CALL LCSOVE ( VTMP,   VTMP4 , VTMP  )
        CALL LCSOVE ( VTMP,   VTMP5 , VTMP  )
C
        CALL LCPRSC ( DFDS   , VTMP  , H5    )
        D = H3 - DFDP + H4 - H5
C
        CALL LCPRMV ( HOOK   , DFDS  , VTMP  )
        CALL LCPRTE ( VTMP   , VTMP  , DSDE  )
        CALL LCPRSM ( -UN/D  , DSDE  , DSDE  )
        CALL LCSOMA ( HOOK   , DSDE  , DSDE  )
C
        END
