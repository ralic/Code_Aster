        SUBROUTINE LCVS ( SIG , DVDS )
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
C       DERIVEE DE LA CONTRAINTE DE VM / SIG = NORMALE SEUIL DE VM
C
C       IN  SIG    :  TENSEUR CONTRAINTE
C       OUT DVDS   :  NORMALE DVDS = 3/2 DEV/S
C                                            T     1/2
C                             S    = (3/2 DEV  DEV )
C                       ET    DEV  = SIG - 1/3 TR(SIG) I
C       ----------------------------------------------------------------
        REAL*8          D32  , ZERO
        PARAMETER       ( D32  = 1.5D0  )
        PARAMETER       ( ZERO = 0.D0   )
        REAL*8          DVDS(6) , SIG(6) ,  DEV(6) , S
        REAL*8          LCNRTS
C       ----------------------------------------------------------------
        CALL LCDEVI ( SIG , DEV )
        S =  LCNRTS ( DEV )
                IF ( S .EQ. ZERO ) THEN
                CALL LCINVE ( ZERO , DVDS )
                ELSE
                CALL LCPRSV ( D32 / S , DEV , DVDS )
                ENDIF
        END
