      SUBROUTINE I2REQ3 (EPSI,A,B,C,D,NBR,R1,R2,R3,MULT1,MULT2,MULT3)
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
      INTEGER NBR,MULT1,MULT2,MULT3
      REAL*8 A,B,C,D,R1,R2,R3,EPSI
C
      REAL*8     PI
C
      REAL*8  AUX,B2,U,P,Q,R,DEUXR,RPUIS3
      REAL*8  INVA,INVA2,INV3,DISC,X,Y,PHI,COND
      REAL*8 TRIGOM
C
C-----------------------------------------------------------------------
      REAL*8 R8PI 
C-----------------------------------------------------------------------
      PI=R8PI()
      R1  = 0.0D0
      R2  = 0.0D0
      R3  = 0.0D0
      NBR = 0
C
      MULT1 = 0
      MULT2 = 0
      MULT3 = 0
C
      INV3  = 1.0D0/3.0D0
      INVA  = 0.0D0
      INVA2 = 0.0D0
C
      COND = ABS(A) + ABS(B) + ABS(C) + ABS(D)
      A    = A/COND
      B    = B/COND
      C    = C/COND
      D    = D/COND
C
      B2 = 0.0D0
      P  = 0.0D0
      Q  = 0.0D0
      Q  = 0.0D0
      Q  = 0.0D0
      R  = 0.0D0
      U  = 0.0D0
C
      DISC = 0.0D0
      AUX  = 0.0D0
C
      X = 0.0D0
      Y = 0.0D0
C
      IF ( ABS(A) .LT. EPSI) THEN
C
C--------L' EQUATION DEGENERE EN UNE EQUATION DE DEGRE 2----------------
C
         CALL I2REQ2 (EPSI,B,C,D,NBR,R1,R2,MULT1,MULT2)
C
      ELSE
C
C-------L 'EQUATION EST BIEN DE DEGRE 3--------------------------------
C
         INVA  = 1.0D0/A
         INVA2 = INVA*INVA
C
         B2 = B*B
         P  = (3*A*C - B2)*INVA2/9.0D0
         Q  = B*B2*INVA2*INVA/27.0D0
         Q  = Q - B*C*INVA2*INV3*0.5D0
         Q  = Q + D*INVA*0.5D0
         R  = 0.0D0
         U  = 0.0D0
C
         DISC = P*P*P + Q*Q
C
         X = -B*INVA*INV3
         Y = 0.0D0
C
         IF ( ABS(P) .LT. EPSI ) THEN
C
C----------------1 RACINE (TRIPLE OU SIMLE)-----------------------
C
            NBR = 1
C
            IF ( ABS(Q) .LT. EPSI ) THEN
C
               MULT1 = 3
               R1    = X
C
            ELSE
C
               MULT1 = 1
               Y     = ABS(2*Q)**INV3
C
               IF ( Q .LT. 0.0D0 ) THEN
C
                  R1 = X + Y
C
               ELSE
C
                  R1 = X - Y
C
               ENDIF
C
            ENDIF
C
         ELSE
C
C-------------CALCUL DES GRANDEURS AUXILIAIRES------------------
C
              R = SQRT(ABS(P))
C
              IF ( Q .LT. 0.0D0 ) THEN
C
                 R = -R
C
              ENDIF
C
              DEUXR  = 2*R
              RPUIS3 = R*R*R
              U      = Q/RPUIS3
C
              IF ( P .GE. 0.0D0 ) THEN
C
C-----------------UNE RACINE SIMPLE--------------------------------
C
                 PHI = LOG(U + SQRT(U*U+1))
C
                 MULT1 = 1
                 NBR  = 1
                 Y    = -DEUXR*SINH(PHI*INV3)
                 R1   = X + Y
C
              ELSE
C
                 IF ( ABS(DISC) .LT. EPSI ) THEN
C
C-----------------UNE DOUBLE ET UNE SIMPLE-----------------------
C
C
                    NBR =  2
                    R1  = -DEUXR + X
                    R2  =  R     + X
C
                    IF ( R1 .GT. R2 ) THEN
C
                       AUX   = R1
                       R1    = R2
                       R2    = AUX
                       MULT1 = 2
                       MULT2 = 1
C
                    ELSE
C
                       MULT1 = 1
                       MULT2 = 2
C
                    ENDIF
C
                 ELSE IF ( DISC .GE. 0.0D0 ) THEN
C
C-----------------UNE RACINE SIMPLE------------------------------
C
                    PHI = LOG(U + SQRT(U*U-1))
C
                    NBR   = 1
                    MULT1 = 1
                    Y     = -DEUXR*COSH(PHI*INV3)
                    R1    = X + Y
C
                 ELSE
C
C-----------------TROIS RACINES SIMPLES--------------------------
C
C
                    MULT1 = 1
                    MULT2 = 1
                    MULT3 = 1
                    NBR   = 3
C
                    PHI = TRIGOM('ACOS', U)
C
                    Y  = -DEUXR*COS(PHI*INV3)
                    R1 = X + Y
                    Y  = DEUXR*COS((PI-PHI)*INV3)
                    R2 = X + Y
                    Y  = DEUXR*COS((PI+PHI)*INV3)
                    R3 = X + Y
C
C----------------------TRI DES RACINES---------------------------
C
                    IF ( R1 .GT. R2 ) THEN
C
                       AUX = R1
                       R1  = R2
                       R2  = AUX
C
                    ENDIF
C
                    IF ( R1 .GT. R3) THEN
C
                       AUX = R1
                       R1  = R3
                       R3  = R2
                       R2  = AUX
C
                    ELSE IF ( R2 .GT. R3 ) THEN
C
                       AUX = R2
                       R2  = R3
                       R3  = AUX
C
                    ELSE
C
                    ENDIF
C
                 ENDIF
C
              ENDIF
C
C
         ENDIF
C
      ENDIF
C
      END
