      SUBROUTINE INSPRI ( S , T )
      IMPLICIT REAL*8 (A-H,O-Z) 
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C       -----------------------------------------------------------
C       NADAI_B :  CALCUL DES CONTRAINTES ET DIRECTIONS PRINCIPALES
C                  L'ANGLE PHI=T(4) : EST EN DEGRE
C
C       IN
C           S     :  CONTRAINTE CORRIGEE A T
C       OUT
C           T     :  CONTRAINTES ET DIRECTIONS PRINCIPALES
C                    T(1) = CONTRAINTE MAJEURE
C                    T(2) = CONTRAINTE MINEURE
C                    T(3) = S(3) CONTRAINTE DE CISAILLEMENT
C                    T(1) = DIRECTION PRINCIPALE MAJEURE (EN DEGRE)
C       -----------------------------------------------------------
        REAL*8    S(4) , T(4) , PI , A1 , A2 , A3 , A4 , A5
C       ------------------------------------------------------------
      PI = 4.D0 * ATAN(1.D0)
      A1 = S(1) + S(2)
      A2 = S(1) - S(2)
      A4 = S(3)
      IF ( ABS(A4) .GE. 1.D-8 ) GOTO 11
      IF ( A2 .LT. 1.D-8 ) GOTO 121
      T(1) = S(1)
      T(2) = S(2)
      T(3) = S(3)
      T(4) = 0.D0
      GOTO 10
  121 CONTINUE
      T(1) = S(2)
      T(2) = S(1)
      T(3) = S(3)
      T(4) = 90.D0
      GOTO 10
   11 CONTINUE
      IF ( ABS(A2) .GE. 1.D-8 ) GOTO 111
      T(1) = S(1) + ABS(A4)
      T(2) = S(2) - ABS(A4)
      T(3) = S(3)
      T(4) = 45.D0 * ABS(A4) / A4
      GOTO 10
  111 CONTINUE
      A5 = SQRT( A2 * A2 + 4.D0 * A4 * A4 )
      T(1) = ( A1 + A5 ) / 2.D0
      T(2) = ( A1 - A5 ) / 2.D0
      T(3) = S(3)
      T(4) = ATAN( 2.D0 * A4 / A2 ) * 90.D0 / PI
      IF ( A2 .GE. 1.D-8 ) GOTO 10
      T(4) = T(4) + 90.D0 * ABS(A4) / A4
  10  CONTINUE
      END
