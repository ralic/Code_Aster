      SUBROUTINE I3AFK2(EPSI,S,FK,IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER IRET
      REAL*8  EPSI,S(3,*),FK(4,*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C     ASSEMBLAGE DE LA TRANSFO GEOM POUR UN QUADRANGLE
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  S      : R : TABLE(1..3,1..4) DES COORDONEES DES SOMMETS
C OUT FK     : R : TABLE(1..4,1..3) DES COEF DES FK
C OUT IRET   : I : CODE RETOUR : -1 --> DEGENERESCENCE
C            :   :                0 --> RAS
C     ------------------------------------------------------------------
C
      REAL*8  A,B,C,D,ZERO,UN
      INTEGER I
C
C======================================================================
C
      IRET = 0
      DO 10, I = 1, 3, 1
         A       = S(I,1)
         B       = S(I,2)
         C       = S(I,3)
         D       = S(I,4)
         FK(1,I) = ( A + B + C + D)*0.25D0
         FK(2,I) = (-A + B + C - D)*0.25D0
         FK(3,I) = (-A - B + C + D)*0.25D0
         FK(4,I) = ( A - B + C - D)*0.25D0
10    CONTINUE
      END
