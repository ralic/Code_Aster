      SUBROUTINE I3CRQP(EPSI,SEUIL,S,X,Y,CR,IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER IRET
      REAL*8  EPSI,SEUIL,S(3,*),X,Y,CR(*)
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
C     QUADRANGLE PLAN DE SOMMETS S
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  SEUIL  : R : SEUIL
C IN  X,Y    : R : COORDONNEES DU POINT
C IN  S      : R : TABLE(1..3,1..4) DES COORDONNEES DES SOMMETS
C OUT CR     : R : TABLE(1..2)      DES COORDONNEES DE REF DE (X,Y)
C OUT IRET   : I : CODE RETOUR : 0 --> CONVERGENCE
C                                1 --> ARRET A NB_MAX ITERATIONS
C                               -1 --> FACE DEGENEREE
C     ------------------------------------------------------------------
C
      REAL*8  FK(4,3),M(2),R1(2),R2(2),A,B,C,D,ZERO,UN
      INTEGER MAXITR
C
C======================================================================
C
      IRET   = 0
      MAXITR = 10
      ZERO   = 0.0D0
      UN     = 1.0D0
      CALL I3AFK2(EPSI,S,FK,IRET)
      A = FK(1,1)
      B = FK(2,1)
      C = FK(3,1)
      D = FK(4,1)
      A = MAX(ABS(A),ABS(B),ABS(C),ABS(D))
      IF ( A .EQ. ZERO ) THEN
         IRET = -1
      ELSE
         A       = UN/A
         FK(1,1) = FK(1,1)*A
         FK(2,1) = FK(2,1)*A
         FK(3,1) = FK(3,1)*A
         FK(4,1) = FK(4,1)*A
         M (1)   = X*A
      ENDIF
      A = FK(1,2)
      B = FK(2,2)
      C = FK(3,2)
      D = FK(4,2)
      A = MAX(ABS(A),ABS(B),ABS(C),ABS(D))
      IF ( A .EQ. ZERO ) THEN
         IRET = -1
      ELSE
         A       = UN/A
         FK(1,2) = FK(1,2)*A
         FK(2,2) = FK(2,2)*A
         FK(3,2) = FK(3,2)*A
         FK(4,2) = FK(4,2)*A
         M (2)   = Y*A
      ENDIF
      FK(1,3) = ZERO
      FK(2,3) = ZERO
      FK(3,3) = ZERO
      FK(4,3) = ZERO
      IF ( IRET .NE. -1 ) THEN
         CALL I3DCH2(EPSI,SEUIL,MAXITR,FK,M,R1,R2,IRET)
         CR(1) = 0.5D0*(R1(1) + R1(2))
         CR(2) = 0.5D0*(R2(1) + R2(2))
         IF ( IRET .GT. 0 ) THEN
            FK(1,1) = FK(1,1) - M(1)
            FK(1,2) = FK(1,2) - M(2)
            CALL I3NWT2(EPSI,EPSI,MAXITR,FK,CR,IRET)
         ENDIF
      ENDIF
      END
