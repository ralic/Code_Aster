        SUBROUTINE LCVVSS ( SIG , DDVDDS )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C       3D / 1D / DP / CP
C       DERIVEE SECONDE DE LA ( NORME DE VON MISES DE SIG ) / SIG / SIG
C
C       IN  SIG    :  TENSEUR CONTRAINTE
C                                                      T
C       OUT DDVDDS :  DDVDDS = 1/S ( 3/2 ID - DVDS DVDS )
C                     DVDS   = 3/2  DEV/S
C                                            T
C                     ID     = I4 - 1/3 I2 I2
C                                      T     1/2
C                     S      = (3/2 DEV  DEV )
C                     DEV    = SIG - 1/3 TR(SIG) I
C       ID     =  TENSEUR DE PASSAGE CONTRAINTE > DEVIATEUR
C       I2     =  TENSEUR UNITE ORDRE 2
C       I4     =  TENSEUR UNITE ORDRE 4
C       ----------------------------------------------------------------
        REAL*8          D23   , D32  , D13  , UN , ZERO
C
        PARAMETER       ( D32  = 1.5D0  )
        PARAMETER       ( D23  =  .66666666666666D0 )
        PARAMETER       ( D13  = -.33333333333333D0 )
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
C
        REAL*8          DVDS(6) ,     SIG(6) ,     DEV(6) , S
        REAL*8          DDVDDS(6,6) , DVDS2(6,6) , ID(6,6)
        REAL*8          LCNRTS
C
        DATA ID         / D23   , D13   , D13   , ZERO , ZERO , ZERO ,
     1                    D13   , D23   , D13   , ZERO , ZERO , ZERO ,
     2                    D13   , D13   , D23   , ZERO , ZERO , ZERO ,
     3                    ZERO  , ZERO  , ZERO  , UN   , ZERO , ZERO ,
     4                    ZERO  , ZERO  , ZERO  , ZERO , UN   , ZERO ,
     5                    ZERO  , ZERO  , ZERO  , ZERO , ZERO , UN /
C       ----------------------------------------------------------------
        CALL LCDEVI ( SIG , DEV )
        S =  LCNRTS ( DEV )
                IF ( S .EQ. ZERO ) THEN
                CALL LCINMA ( ZERO , DDVDDS )
                ELSE
                CALL LCVS   ( SIG   , DVDS )
                CALL LCPRTE ( DVDS  , DVDS  , DVDS2  )
                CALL LCPRSM ( D32   , ID    , DDVDDS )
                CALL LCDIMA ( DDVDDS, DVDS2 , DDVDDS )
                CALL LCPRSM ( UN/S  , DDVDDS, DDVDDS )
                ENDIF
        END
