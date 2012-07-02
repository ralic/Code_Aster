      SUBROUTINE I2REQ4 (EPSI,A,B,C,D,E,NBR,R1,R2,R3,R4,
     +                   MULT1,MULT2,MULT3,MULT4)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
      INTEGER NBR,MULT1,MULT2,MULT3,MULT4
      REAL*8 A,B,C,D,E,EPSI,R1,R2,R3,R4
C
      INTEGER NBR3,NBR21,NBR22
      REAL*8  Y,Y1,Y2,Y3,X11,X12,X21,X22
      REAL*8  INVA,B3,B2,B1,B0,A2
      REAL*8  AUX,AUX1,AUX2,P1,P2,Q1,Q2,COND
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      NBR = 0
      R1  = 0.0D0
      R2  = 0.0D0
      R3  = 0.0D0
      R4  = 0.0D0
C
      MULT1 = 0
      MULT2 = 0
      MULT3 = 0
      MULT4 = 0
C
      COND = ABS(A) + ABS(B) + ABS(C) + ABS(D) + ABS(E)
      A    = A/COND
      B    = B/COND
      C    = C/COND
      D    = D/COND
      E    = E/COND
C
      AUX1 = 0.0D0
      AUX2 = 0.0D0
      AUX  = 0.0D0
      P1   = 0.0D0
      P2   = 0.0D0
      Q1   = 0.0D0
      Q2   = 0.0D0
C
      NBR21 = 0
      NBR22 = 0
      NBR3  = 0
C
      INVA = 0.0D0
C
      IF ( ABS(A) .LT. EPSI ) THEN
C
C-----------L' EQUATION DEGENERE EN UNE EQUATION DE DEGRE 3-------
C
         CALL I2REQ3 (EPSI,B,C,D,E,NBR,R1,R2,R3,MULT1,MULT2,MULT3)
C
      ELSE
C
C-----------L 'EQUATION EST BIEN DE DEGRE 4----------------------
C
C-----------FACTORISATION DE L' EQUATION DEDEGRE 4 EN PRODUIT---
C-----------DE DEUX EQUATIONS DE DEGRE 2                     ---
C
         INVA = 1.0D0/A
         B    = INVA*B
         C    = INVA*C
         D    = INVA*D
         E    = INVA*E
C
         A2 = 1.0D0
C
         B3 = 1.0D0
         B2 = -C
         B1 = B*D - 4.0D0*E
         B0 = E*(4.0D0*C - B*B) - D*D
C
         Y1 = 0.0D0
         Y2 = 0.0D0
         Y3 = 0.0D0
         Y  = 0.0D0
C
         CALL I2REQ3(EPSI,B3,B2,B1,B0,NBR3,Y1,Y2,Y3,MULT1,MULT2,MULT3)
C
         IF (NBR3 .EQ. 1) THEN
C
            Y = Y1
C
         ELSE IF (NBR3 .EQ. 2) THEN
C
            Y = Y2
C
         ELSE
C
            Y = Y3
C
         ENDIF
C
         AUX1 =  (B*B + 4.0D0*(Y -C))
         AUX2 =  (Y*Y - 4.0D0*E)
C
         IF ( ABS(AUX1) .LT. EPSI ) THEN
C
             AUX1 = 0.0D0
C
         ELSE
C
             AUX1 = SQRT (AUX1)
C
         ENDIF
C
         IF (ABS(AUX2) .LT. EPSI) THEN
C
             AUX2 = 0.0D0
C
         ELSE
C
             AUX2 = SQRT (AUX2)
C
         ENDIF
C
         P1 = 0.5D0 * (B + AUX1)
         P2 = 0.5D0 * (B - AUX1)
         Q1 = 0.5D0 * (Y + AUX2)
         Q2 = 0.5D0 * (Y - AUX2)
C
         IF ( B*Y .LT. 2.0D0*D) THEN
C
            AUX = Q1
            Q1  = Q2
            Q2  = AUX
C
         ENDIF
C
C-------------RESOULTION DES EQUATIONS DE DEGRES 2-----------------
C
         CALL I2REQ2 (EPSI,A2,P1,Q1,NBR21,X11,X12,MULT1,MULT2)
         CALL I2REQ2 (EPSI,A2,P2,Q2,NBR22,X21,X22,MULT3,MULT4)
C
         NBR = NBR21 + NBR22
C
C-------------TRI DES RACINES-------------------------------------
C
         IF ( NBR .EQ. 1 ) THEN
C
            IF ( NBR21 .EQ. 1 ) THEN
C
               R1 = X11
C
            ELSE
C
               R1    = X21
               MULT1 = MULT3
C
            ENDIF
C
         ELSE IF ( NBR .EQ. 2 ) THEN
C
            IF ( NBR21 .EQ. 2 ) THEN
C
               R1 = X11
               R2 = X12
C
            ELSE IF( NBR21 .EQ. 1 ) THEN
C
               IF ( ABS(X11-X21) .LT. EPSI ) THEN
C
                  NBR   = 1
                  MULT1 = MULT1 + MULT3
                  R1    = X11
C
               ELSE IF ( X11 .GT. X21 ) THEN
C
                  MULT2 = MULT1
                  MULT1 = MULT3
                  NBR   = 2
                  R1    = X21
                  R2    = X11
C
               ELSE
C
                  MULT2 = MULT3
                  NBR   = 2
                  R1    = X11
                  R2    = X21
C
               ENDIF
C
            ELSE
C
               R1    = X21
               R2    = X22
               MULT1 = MULT3
               MULT2 = MULT4
C
            ENDIF
C
C
         ELSE IF ( NBR .EQ. 3 ) THEN
C
            IF ( NBR21 .EQ. 2 ) THEN
C
               IF ( ABS(X11-X21) .LT. EPSI ) THEN
C
                  MULT1 = MULT1 + MULT3
                  NBR   = 2
                  R1    = X11
                  R2    = X12
C
               ELSE IF ( ABS(X12-X21) .LT. EPSI ) THEN
C
                  MULT2 = MULT2 + MULT3
                  NBR   = 2
                  R1    = X11
                  R2    = X12
C
               ELSE IF ( X21 .LT. X11 ) THEN
C
                  MULT4 = MULT3
                  MULT3 = MULT2
                  MULT2 = MULT1
                  MULT1 = MULT4
                  NBR   = 3
                  R1    = X21
                  R2    = X11
                  R3    = X12
C
               ELSE IF ( X12 .LT. X21 ) THEN
C
                  MULT4 = MULT3
                  MULT3 = MULT2
                  MULT2 = MULT4
                  NBR   = 3
                  R1    = X11
                  R2    = X21
                  R3    = X12
C
               ELSE
C
                  NBR   = 3
                  R1    = X11
                  R2    = X12
                  R3    = X22
C
               ENDIF
C
            ELSE
C
               IF ( ABS(X11-X21) .LT. EPSI ) THEN
C
                  MULT1 = MULT1 + MULT3
                  MULT2 = MULT4
                  NBR   = 2
                  R1    = X21
                  R2    = X22
C
               ELSE IF ( ABS(X11-X22) .LT. EPSI ) THEN
C
                  MULT2 = MULT1 + MULT3
                  MULT1 = MULT3
                  NBR   = 2
                  R1    = X21
                  R2    = X22
C
               ELSE IF ( X11 .LT. X21 ) THEN
C
                  MULT2 = MULT3
                  MULT3 = MULT4
                  NBR   = 3
                  R1    = X11
                  R2    = X21
                  R3    = X22
C
               ELSE IF ( X11 .LT. X22 ) THEN
C
                  MULT2 = MULT1
                  MULT1 = MULT3
                  MULT3 = MULT4
                  NBR   = 3
                  R1    = X21
                  R2    = X11
                  R3    = X22
C
               ELSE
C
                  MULT2 = MULT4
                  MULT4 = MULT1
                  MULT1 = MULT3
                  MULT3 = MULT4
                  NBR   = 3
                  R1    = X21
                  R2    = X22
                  R3    = X11
C
               ENDIF
C
            ENDIF
C
         ELSE IF ( NBR .EQ. 4 )THEN
C
           IF ( ABS(X22-X12) .LT. EPSI ) THEN
C
              IF ( ABS(X11-X21) .LT. EPSI ) THEN
C
                 MULT1 = 2
                 MULT2 = 2
                 NBR   = 2
                 R1    = X11
                 R2    = X12
C
              ELSE IF ( X21 .GT. X11 ) THEN
C
                 MULT2 = 1
                 NBR   = 3
                 R1    = X11
                 R2    = X21
                 R3    = X12
C
              ELSE
C
                 MULT3 = 2
                 NBR   = 3
                 R1    = X21
                 R2    = X11
                 R3    = X12
C
              ENDIF
C
           ELSE IF ( ABS(X22-X11) .LT. EPSI ) THEN
C
                 MULT2 = 2
                 NBR   = 3
                 R1    = X21
                 R2    = X11
                 R3    = X12
C
           ELSE IF ( X22 .GT. X12 ) THEN
C
              IF ( ABS(X12-X21) .LT. EPSI ) THEN
C
                 MULT2 = 2
                 NBR   = 3
                 R1    = X11
                 R2    = X12
                 R3    = X22
C
              ELSE IF ( ABS(X11-X21) .LT. EPSI ) THEN
C
                 MULT1 = 2
                 NBR   = 3
                 R1    = X11
                 R2    = X12
                 R3    = X22
C
              ELSE IF ( X21 .GT. X12 ) THEN
C
                 R1    = X11
                 R2    = X12
                 R3    = X21
                 R4    = X22
C
              ELSE IF ( X21 .GT. X11 ) THEN
C
                 R1    = X11
                 R2    = X21
                 R3    = X12
                 R4    = X22
C
              ELSE
C
                 R1    = X21
                 R2    = X11
                 R3    = X12
                 R4    = X22
C
              ENDIF
C
           ELSE IF (X22 .GT. X11 ) THEN
C
              IF ( ABS(X11-X21) .LT. EPSI ) THEN
C
                 MULT1 = 2
                 NBR   = 3
                 R1    = X11
                 R2    = X22
                 R3    = X12
C
              ELSE IF ( X21 .GT. X11 ) THEN
C
                 R1    = X11
                 R2    = X21
                 R3    = X22
                 R4    = X12
C
              ELSE
C
                 R1    = X21
                 R2    = X11
                 R3    = X22
                 R4    = X12
C
              ENDIF
C
           ELSE
C
                 R1    = X21
                 R2    = X22
                 R3    = X11
                 R4    = X12
C
           ENDIF
C
         ENDIF
C
      ENDIF
C
      END
