        SUBROUTINE ONOFS        ( SIG, X1, X2, X3,X4, X5, DFDS )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
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
C       3D / 1D / DP / CP
C       DERIVEE /SIG DE LA FONCTION SEUIL A (SIG,X1,X2,X3,X4,X5 ) DONNES
C
C       IN  SIG    :  TENSEUR CONTRAINTE
C       IN  X1     :  TENSEUR CINEMATIQUE 1
C       IN  X2     :  TENSEUR CINEMATIQUE 2
C       IN  X3     :  TENSEUR CINEMATIQUE 3
C       IN  X4     :  TENSEUR CINEMATIQUE 4
C       IN  X5     :  TENSEUR CINEMATIQUE 5
C       OUT DFDS   :  NORMALE DFDS = 3 (D-X1-X2-X3-X4-X5) / 2 S
C                                               T                 1/2
C                       S = (3/2(D-X1-X2-X3-X4-X5)(D-X1-X2-X3-X4-X5))
C                    ET D = SIG - 1/3 TR(SIG) I
C       ----------------------------------------------------------------
        INTEGER         N , ND
        REAL*8          DFDS(6) , SIG(6) , X1(6) , X2(6) , DEV(6) , S
        REAL*8          LCNRTS  , X3(6), X4(6), X5(6)
C       ----------------------------------------------------------------
        COMMON /TDIM/   N , ND
C       ----------------------------------------------------------------
C
        CALL LCDEVI ( SIG , DEV )
        CALL LCDIVE ( DEV , X1 , DEV )
        CALL LCDIVE ( DEV , X2 , DEV )
        CALL LCDIVE ( DEV , X3 , DEV )
        CALL LCDIVE ( DEV , X4 , DEV )
        CALL LCDIVE ( DEV , X5 , DEV )
C
        S =  LCNRTS ( DEV )
        CALL LCPRSV ( 1.5D0 / S , DEV , DFDS )
        END
