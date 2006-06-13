      SUBROUTINE INSFS (SIG , DFDS  , DEV )
C
      IMPLICIT REAL*8 (A-H,O-Z)
C       ============================================================
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
C       ------------------------------------------------------------
C       NADAI_BETON  :
C
C       3D / 1D / DP / CP
C
C          IN   SIG :  TENSEUR CONTRAINTE
C
C          OUT  DFDS : GRADIENT = DERIVEE FONCTION SEUIL /  SIGMA
C               DFDS = ( A*C1 + DEV/TOCT ) / (3*B)
C                      ( SI TOCT=0 , DFDS=0 )
C               DEV     : DEVIATEUR DE CONTRAINTE (SIG - HYD)
C                         HYD  = 1/3 TR(SIG) I
C------------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C
       INTEGER  NDT , NDI
       REAL*8   ZERO , UN , DEUX , TROIS
       PARAMETER       ( ZERO   = 0.D0   )
       PARAMETER       ( UN     = 1.D0   )
       PARAMETER       ( DEUX   = 2.D0   )
       PARAMETER       ( TROIS  = 3.D0   )
       REAL*8  SIG(6) , DFDS(6) , DEV(6) , C1(6) , VTMP(6)
       REAL*8  A , B , S , TOCT , BETA , LCNRTS
       DATA  C1    / UN , UN , UN , ZERO , ZERO , ZERO/
C------------------------------------------------------------------
       CALL LCINVE ( ZERO , DEV )
       BETA   = 1.16D0
       CALL LCDEVI (SIG, DEV)
       S =  LCNRTS ( DEV )
       TOCT = SQRT( DEUX / 9.D0 ) * S
       A = SQRT( DEUX ) * ( BETA - UN ) / ( DEUX * BETA - UN )
       B = SQRT( DEUX ) / TROIS * BETA  / ( DEUX * BETA - UN )
C
       CALL LCINVE ( ZERO , DFDS )
       IF ( TOCT .NE. ZERO ) THEN
        CALL LCPRSV ( UN / TOCT       , DEV   , VTMP )
        CALL LCPRSV ( A               , C1    , DFDS )
        CALL LCSOVE ( VTMP            , DFDS  , DFDS )
        A = UN / TROIS / B
        CALL LCPRSV ( A  , DFDS  , DFDS )
       ENDIF
C
      END
