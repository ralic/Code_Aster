      SUBROUTINE I3CRTP(EPSI,SEUIL,S,P,CR,IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER IRET
      REAL*8  EPSI,SEUIL,S(3,*),P(*),CR(*)
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
C     CALCUL DES COORDONNEES DE REF DU POINT DE CORDO (X,Y) DANS LE
C     TRIANGLE DE SOMMETS S
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  SEUIL  : R : SEUIL
C IN  P      : R : COORDONNEES DU POINT
C IN  S      : R : TABLE(1..3,1..3) DES COORDONNEES DES SOMMETS
C OUT CR     : R : TABLE(1..2)      DES COORDONNEES DE REF DE P
C OUT IRET   : I : CODE RETOUR : 0 --> PAS DE PB
C                                1 --> DEGENERESCENCE
C     ------------------------------------------------------------------
C
      REAL*8  A(3,2),B(3),C,D,ZERO,UN
      INTEGER I,J
C
C======================================================================
C
      ZERO = 0.0D0
      UN   = 1.0D0
      IRET = 0
      DO 10, I = 1, 3, 1
         D = S(I,1)
         DO 11, J = 1, 2, 1
            A(I,J) = S(I,J+1) - D
11       CONTINUE
         B(I) = P(I) - D
         C = MAX(ABS(A(I,1)),ABS(A(I,2)))
         IF ( C .GT. EPSI*D ) THEN
            C      = UN/C
            A(I,1) = A(I,1)*C
            A(I,2) = A(I,2)*C
            B(I)   = B(I)  *C
         ENDIF
10    CONTINUE
      D = A(1,1)*A(2,2) - A(1,2)*A(2,1)
      IF ( ABS(D) .GT. EPSI ) THEN
         D     =  UN/D
         CR(1) = (B(1)*A(2,2) - B(2)*A(1,2))*D
         CR(2) = (B(2)*A(1,1) - B(1)*A(2,1))*D
      ELSE
         D = A(1,1)*A(3,2) - A(1,2)*A(3,1)
         IF ( ABS(D) .GT. EPSI ) THEN
            D     =  UN/D
            CR(1) = (B(1)*A(3,2) - B(3)*A(1,2))*D
            CR(2) = (B(3)*A(1,1) - B(1)*A(3,1))*D
         ELSE
            D = A(2,1)*A(3,2) - A(2,2)*A(3,1)
            IF ( ABS(D) .GT. EPSI ) THEN
               D     =  UN/D
               CR(1) = (B(2)*A(3,2) - B(3)*A(2,2))*D
               CR(2) = (B(3)*A(2,1) - B(2)*A(3,1))*D
            ELSE
               IRET = 1
            ENDIF
         ENDIF
      ENDIF
      IF ( IRET .NE. 1 ) THEN
         DO 20, I = 1, 2, 1
            D = CR(I)
            IF ( ABS(D) .LT. EPSI ) THEN
               CR(I) = ZERO
            ELSE IF ( ABS(D-UN) .LT. EPSI ) THEN
               CR(I) = UN
            ELSE
            ENDIF
20       CONTINUE
      ENDIF
      END
