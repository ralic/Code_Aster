        SUBROUTINE CHBJPL ( MOD,NMAT,MATER,SIG,VIN,DSDE )
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
        REAL*8          VIN(*)
        REAL*8          SIG(6) ,     X1(6) ,    X2(6)   , P ,  DFDP
        REAL*8          HOOK(6,6),   DSDE(6,6), DFDS(6)
        REAL*8          VTMP(6),     VTMP1(6),  VTMP2(6)
C
        REAL*8          RI,  RO, B, K, W, A1, A2, C1, C2
        REAL*8          MATER(NMAT,2)
        REAL*8          H1 , H2 , H3 , H4 , H5 , D
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        REAL*8          CHBCIN
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        CALL LCEQVN ( NDT , VIN(1)     , X1  )
        CALL LCEQVN ( NDT , VIN(NDT+1) , X2  )
        P = VIN(2*NDT+1)
C
        RI = MATER(1,2)
        RO = MATER(2,2)
        B  = MATER(3,2)
        K  = MATER(4,2)
        W  = MATER(5,2)
        A1 = MATER(6,2)
        A2 = MATER(7,2)
        C1 = MATER(8,2)
        C2 = MATER(9,2)
        H1  = C1 * A1 * D23
        H2  = C2 * A2 * D23
C
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATER(1,1) , HOOK )
        CALL CHBFS  ( SIG    , X1    , X2,   DFDS )
        CALL LCPRMV ( HOOK   , DFDS  , VTMP1 )
        CALL LCPRSC ( DFDS   , VTMP1 , H3    )
        DFDP =  B * ( RO - RI ) * EXP(-B*P)
        CALL LCPRSC ( DFDS   , DFDS  , H4    )
        H4 = H4  *  ( H1+H2 ) * CHBCIN(K,W,P)
        CALL LCPRSV ( C1     , X1    , VTMP1 )
        CALL LCPRSV ( C2     , X2    , VTMP2 )
        CALL LCSOVE ( VTMP1  , VTMP2 , VTMP  )
        CALL LCPRSC ( DFDS   , VTMP  , H5    )
        D = H3 - DFDP + H4 - H5
        CALL LCPRMV ( HOOK   , DFDS  , VTMP  )
        CALL LCPRTE ( VTMP   , VTMP  , DSDE  )
        CALL LCPRSM ( -UN/D  , DSDE  , DSDE  )
        CALL LCSOMA ( HOOK   , DSDE  , DSDE  )
C
        END
