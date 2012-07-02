        SUBROUTINE CHBFSX ( SIG , X1 , X2 , I4, DDFDSX )
      IMPLICIT NONE
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       DERIVEE / S / X DE LA NORMALE A LA FONCTION SEUIL A (SIG,X1,X2)
C       IN  SIG    :  TENSEUR CONTRAINTE
C       IN  X1     :  TENSEUR CINEMATIQUE 1
C       IN  X2     :  TENSEUR CINEMATIQUE 2
C                                                       T
C       OUT  DDFDSX:  DDFDSX = -1/S ( 3/2 I4 - DFDS DFDS )
C                     DFDS   = 3 (D-X1-X2) / 2 S
C                                           T           1/2
C                     S      = (3/2(D-X1-X2) (D-X1-X2))
C                     D      = SIG - 1/3 TR(SIG) I
C       ----------------------------------------------------------------
        INTEGER         N , ND
        REAL*8          DFDS(6) ,  SIG(6) , X1(6) , X2(6) , DEV(6) , S
        REAL*8          DDFDSX(6,6) , DFDS2(6,6)
        REAL*8          I4(6,6)
        REAL*8          LCNRTS
        COMMON /TDIM/   N , ND
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
        CALL LCDEVI ( SIG , DEV )
        CALL LCDIVE ( DEV , X1 , DEV )
        CALL LCDIVE ( DEV , X2 , DEV )
        S =  LCNRTS ( DEV )
        CALL CHBFS   ( SIG     , X1  ,   X2 , DFDS )
        CALL LCPRTE ( DFDS    , DFDS  , DFDS2 )
        CALL LCPRSM ( 1.5D0     , I4    , DDFDSX )
        CALL LCDIMA ( DDFDSX  , DFDS2 , DDFDSX )
        CALL LCPRSM ( -1.D0 / S , DDFDSX, DDFDSX )
        END
