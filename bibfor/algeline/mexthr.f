      SUBROUTINE MEXTHR (N, A, LDA)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, LDA
      COMPLEX    *16 A(LDA,*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C-----------------------------------------------------------------------
C IN  : N    : DIMENSION DE LA MATRICE.
C     : LDA  : DIMENSION DE A.
C I/O : A    : MATRICE HERMITIENNE COMPLEXE D'ORDRE N.
C         IN : PARTIE TRIANGULAIRE SUPERIEURE.
C        OUT   PARTIE TRIANGULAIRE INFERIEURE DE LA MATRICE A DEFINIE
C              COMME UNE MATRICE COMPLEXE HERMITIENNE.
C-----------------------------------------------------------------------
      INTEGER    I, J
      REAL *8 EPS
      COMPLEX    *16 CONJG
      REAL *8 R8PREM
C
      IF (N .LT. 1) THEN
         WRITE(6,*)  'THE ARGUMENT N = %(I1).  IT MUST BE AT '//
     &               'LEAST 1.'
         GO TO 9000
      END IF
      IF (LDA .LT. N) THEN
         WRITE(6,*)  'THE ARGUMENT LDA = %(I1).  IT MUST BE AT '//
     &               'LEAST AS LARGE AS N = %(I2).'
         GO TO 9000
      END IF
      EPS = 10.0D0*R8PREM()
      DO 10  I=1, N
         IF (ABS(DIMAG(A(I,I))) .NE. 0.0D0) THEN
            IF (ABS(DIMAG(A(I,I))) .GT. EPS*ABS(DBLE(A(I,I)))) THEN
               WRITE(6,*)   A(I,I)
               WRITE(6,*)  'THE MATRIX ELEMENT A(%(I1),%(I1)) '//
     &                     '= %(Z1).  THE DIAGONAL OF A HERMITIAN '//
     &                     'MATRIX MUST BE REAL.'
               GO TO 9000
            ELSE
               WRITE(6,*)   A(I,I)
               WRITE(6,*)  'THE MATRIX ELEMENT A(%(I1),%(I1)) '//
     &                     '= %(Z1).  THE DIAGONAL OF A HERMITIAN '//
     &                     'MATRIX MUST BE REAL: ITS IMAGINARY PART '//
     &                     'IS SET TO ZERO.'
               A(I,I) = DBLE(A(I,I))
            END IF
         END IF
   10 CONTINUE
C
      DO 30  J=1, N - 1
         DO 20  I=J + 1, N
            A(I,J) = DCONJG(A(J,I))
   20    CONTINUE
   30 CONTINUE
C
 9000 CONTINUE
 9999 CONTINUE
      END
