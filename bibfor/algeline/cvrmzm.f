      SUBROUTINE CVRMZM (N, A, LDA, B, LDB)
      IMPLICIT NONE
      INTEGER    N, LDA, LDB
      REAL *8 A(LDA,*)
      COMPLEX    *16 B(LDB,*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C   COPIE UNE MATRICE REELLE DANS UNE MATRICE COMPLEXE.
C-----------------------------------------------------------------------
C IN  : N    : DIMENSION DES MATRICES A ET B.
C     : A    : MATRICE REELLE DE DIMENSION N.
C     : LDA  : DIMENSION DE A.
C     : LDB  : DIMENSION DE B.
C OUT : B    : MATRICE COMPLEXE D'ORDRE N CONTENANT UNE COPIE DE A.
C-----------------------------------------------------------------------
      INTEGER    I, J
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IF (N .LT. 1) THEN
         WRITE(6,*)  'THE ORDER OF THE MATRIX MUST BE AT '//
     &               'LEAST 1 WHILE N = %(I1) IS GIVEN.'
         GO TO 9000
      END IF
C
      IF (LDA .LT. N) THEN
         WRITE(6,*)  'THE LEADING DIMENSION OF A MUST BE AT '//
     &               'LEAST AS LARGE AS N WHILE LDA = %(I1) AND N '//
     &               '= %(I2) ARE GIVEN.'
         GO TO 9000
      END IF
C
      IF (LDB .LT. N) THEN
         WRITE(6,*)  'THE LEADING DIMENSION OF B MUST BE AT '//
     &               'LEAST AS LARGE AS N WHILE LDB = %(I1) AND N '//
     &               '= %(I2) ARE GIVEN.'
         GO TO 9000
      END IF
C       --- A EST COPIEE DANS B
      DO 10  J=N, 1, -1
         DO 10  I=N, 1, -1
            B(I,J) = DCMPLX(A(I,J),0.0D0)
   10 CONTINUE
C
 9000 CONTINUE
      END
