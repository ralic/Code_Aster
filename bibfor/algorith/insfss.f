      SUBROUTINE INSFSS (SIG , DDFDDS)
C
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
C       NADAI_BETON  :
C
C       3D / 1D / DP / CP
C
C          IN   SIG :  TENSEUR CONTRAINTE
C
C          OUT  DDFDDS :  DERIVEE SECONDE FONCTION SEUIL / SIGM / SIGM
C                                   T         3
C               DDFDDS = ( - DEV DEV /( 9 TOCT ) + ID / (3 TOCT) ) / B
C       ----------------------------------------------------------------
C
        COMMON /TDIM/   NDT , NDI
C
        INTEGER  NDT , NDI
        REAL*8     ZERO , UN , DEUX, TROIS, D23 , D13
        PARAMETER       ( ZERO  = 0.D0   )
        PARAMETER       ( UN    = 1.D0   )
        PARAMETER       ( DEUX  = 2.D0   )
        PARAMETER       ( TROIS = 3.D0   )
        PARAMETER       ( D23   =  .66666666666666D0 )
        PARAMETER       ( D13   = -.33333333333333D0 )
        REAL*8  SIG(6) , DDFDDS(6,6) , ID(6,6) , W(6,6) , DEV(6)
        REAL*8  TOCT, A1, A2 , A3 , S , B , BETA , LCNRTS
C       ----------------------------------------------------------------
        DATA ID         / D23   , D13   , D13   , ZERO , ZERO , ZERO ,
     &                    D13   , D23   , D13   , ZERO , ZERO , ZERO ,
     &                    D13   , D13   , D23   , ZERO , ZERO , ZERO ,
     &                    ZERO  , ZERO  , ZERO  , UN   , ZERO , ZERO ,
     &                    ZERO  , ZERO  , ZERO  , ZERO , UN   , ZERO ,
     &                    ZERO  , ZERO  , ZERO  , ZERO , ZERO , UN   /
C       ----------------------------------------------------------------
        CALL LCINVE ( ZERO , DEV )
        BETA  = 1.16D0
        CALL LCDEVI ( SIG , DEV )
        S =  LCNRTS ( DEV )
        TOCT = SQRT( DEUX / 9.D0 ) * S
C
        IF( TOCT .EQ. ZERO ) THEN
        CALL LCINMA ( ZERO , DDFDDS )
        ELSE
        B = SQRT( DEUX ) / TROIS * BETA  / ( DEUX * BETA - UN )
        A1 = UN / ( TROIS * TOCT )
        A2 = UN / B
        A3 = - TROIS * (A1**3)
        CALL LCEQMA ( ID     , W  )
        CALL LCPRSM ( A1     , W      , W )
        CALL LCPRTE ( DEV    , DEV    , DDFDDS )
        CALL LCPRSM ( A3     , DDFDDS , DDFDDS )
        CALL LCSOMA ( DDFDDS , W      , DDFDDS )
        CALL LCPRSM ( A2     , DDFDDS , DDFDDS )
        ENDIF
C
        END
