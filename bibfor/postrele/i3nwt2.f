      SUBROUTINE I3NWT2(EPSI,SEUIL,MAXITR,FK,X,IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER MAXITR,IRET
      REAL*8  EPSI,SEUIL,FK(4,*),X(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C     RESOLUTION DE FK(X) = 0 PAR NEWTON-RAPHSON
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  SEUIL  : R : CRITERE D' ARRET
C IN  MAXITR : I : NOMBRE MAX D' ITERATION AUTORISE
C IN  FK     : R : TABLE(1..4,1..2) DES COEF DE LA TRANSFORMATION
C VAR X      : R : TABLE(1..2)      DES COORDONNEES DU POINT
C OUT IRET   : I : CODE RETOUR :  0 --> CONVERGENCE A SEUIL
C                                 1 --> MAX_ITER UTILISE
C                                -1 --> UN JACOBIEN NUL  RENCONTRE
C     ------------------------------------------------------------------
C
      INTEGER I
      REAL*8  VALFK(3,1),D,D1,D2,J11,J12,J21,J22
      LOGICAL FINI
C
C======================================================================
C
      FINI = .FALSE.
      I    =  1
      IRET =  0
10    CONTINUE
      IF ( .NOT. FINI ) THEN
         CALL I3EFK2(FK,1,X(1),X(2),VALFK)
         J11 = FK(2,1) + FK(4,1)*X(2)
         J12 = FK(3,1) + FK(4,1)*X(1)
         J21 = FK(2,2) + FK(4,2)*X(2)
         J22 = FK(3,2) + FK(4,2)*X(1)
         D   = J11*J22 - J12*J21
         IF ( ABS(D) .LT. EPSI ) THEN
            FINI = .TRUE.
            IRET = -1
         ELSE
            D = 1/D
            D1   = (VALFK(1,1)*J22 - VALFK(2,1)*J12)*D
            D2   = (VALFK(2,1)*J11 - VALFK(1,1)*J21)*D
            D    = X(1)*X(1) + X(2)*X(2)
            X(1) = X(1) - D1
            X(2) = X(2) - D2
            D1   = D1*D1 + D2*D2
            I    = I + 1
            IF ( D1 .LE. D*SEUIL*SEUIL ) THEN
               FINI = .TRUE.
               IRET = 0
            ELSE
               IF ( I .GT. MAXITR ) THEN
                  FINI = .TRUE.
                  IRET = 1
               ENDIF
            ENDIF
         ENDIF
         GOTO 10
      ENDIF
      CALL I3EFK2(FK,1,X(1),X(2),VALFK)
      D = VALFK(1,1)*VALFK(1,1) + VALFK(2,1)*VALFK(2,1)
      END
