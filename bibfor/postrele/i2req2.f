      SUBROUTINE I2REQ2 (EPSI,A,B,C,NR,R1,R2,MULT1,MULT2)
      IMPLICIT REAL*8 (A-H,O-Z)
C
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
C
      INTEGER NR, MULT1, MULT2
      REAL*8 A, B, C, R1, R2, EPSI
C
      REAL*8 DELTA, AUX1, AUX2,COND
C
      NR     = 0
      MULT1  = 0
      MULT2  = 0
      R1     = 0.0D0
      R2     = 0.0D0
C
      COND   = ABS(A) + ABS(B) + ABS(C)
      A      = A/COND
      B      = B/COND
      C      = C/COND
      DELTA  = B*B - 4*A*C
      AUX2   = 2*A
      AUX1   = 0.0D0
C
      IF ( ABS(A) .LE. EPSI ) THEN
C
         IF ( ABS(B) .GT. EPSI ) THEN
C
            NR     = 1
            R1     = -C/B
            MULT1  = 1
C
         ENDIF
C
      ELSE
C
         IF ( ABS(DELTA) .LT. EPSI )   THEN
C
            NR     = 1
            R1     = -B/AUX2
            MULT1  = 2
C
         ELSE IF ( DELTA .GT. 0.0D0 ) THEN
C
            NR     = 2
            AUX1   = SQRT(DELTA)
            R1     = (-B - AUX1)/AUX2
            R2     = (-B + AUX1)/AUX2
            MULT1  = 1
            MULT2  = 1
C
            IF ( R1 .GT. R2) THEN
C
               AUX1 = R1
               R1   = R2
               R2   = AUX1
C
            ENDIF
C
         ELSE
C
         ENDIF
C
      ENDIF
C
      END
