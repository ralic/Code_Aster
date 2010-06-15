      SUBROUTINE I3RPQP(PTO,E1,E2,E3,PT,NBPT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER NBPT
      REAL*8  PTO(*),E1(*),E2(*),E3(*),PT(3,*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     COORDONEES DES POINTS DE PT DANS LE REPERE (PTO,E1,E2,E3)
C     ------------------------------------------------------------------
C IN  PTO  : R : TABLE(1..3) : ORIGINE
C IN  NBPT : I : NOMBRE DE POINTS
C IN  E1   : R : TABLE(1..3) : VECTEUR 1
C IN  E2   : R : TABLE(1..3) : VECTEUR 2
C IN  E3   : R : TABLE(1..3) : VECTEUR 3
C VAR PT X : R : TABLE(1..3,1..NBPT) : POINT A TRAITER
C     ------------------------------------------------------------------
C
      INTEGER I,J
      REAL*8  X(3),C,ZERO
C
C======================================================================
      ZERO = 0.0D0
      DO 20, J = 1, NBPT, 1
         DO 21, I = 1, 3, 1
            X(I) = PT(I,J) - PTO(I)
21       CONTINUE
         C  = ZERO
         DO 22, I = 1, 3, 1
            C = C + E1(I)*X(I)
22       CONTINUE
         PT(1,J) = C
         C  = ZERO
         DO 23, I = 1, 3, 1
            C = C + E2(I)*X(I)
23       CONTINUE
         PT(2,J) = C
         C  = ZERO
         DO 24, I = 1, 3, 1
            C = C + E3(I)*X(I)
24       CONTINUE
         PT(3,J) = C
20    CONTINUE
      END
