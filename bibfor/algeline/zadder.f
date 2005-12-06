      SUBROUTINE ZADDER (UPLO, N, ALPHA, X, INCX, A, LDA)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, INCX, LDA
      REAL *8 ALPHA
      COMPLEX    *16 X(*), A(*)
      CHARACTER*(*)  UPLO
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C    CALCUL DE ALPHA*X*CONJG(X)'  =>    A MATRICE HERMITIENNE.
C-----------------------------------------------------------------------
C IN  : UPLO : INDIQUE LE MODE DE STOCKAGE DE LA MATRICE.
C              SI UPLO EST 'U' ALORS SEULEMENT LA PARTIE SUPERIEURE DE A
C              EST UTILISEE. SI UPLO EST 'L', ALORS LA PARTIE INFERIEURE
C              EST UTILISEE.
C     : N    : DIMENSION DE LA MATRICE A.
C     : ALPHA: SCALAIRE.
C     : X    : DVECTEURE COMPLEXE DE LONGUEUR (N-1)*IABS(INCX)+1.
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
C I/O : A    : MATRICE COMPLEXE DE DIMENSION N.
C IN  : LDA  : DIMENSION DE A.
C-----------------------------------------------------------------------
      INTEGER    IX, J
      COMPLEX    *16 TEMP, TEMP1, TEMP2, TEMP3, TEMP4
      LOGICAL    UPPER
      REAL *8 DBLE
      COMPLEX    *16 CONJG
C
      IF (N.EQ.0 .OR. ALPHA.EQ.0.0D0) GO TO 9000
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
C
      UPPER = (UPLO(1:1).EQ.'U') .OR. (UPLO(1:1).EQ.'u')
C
      DO 10  J=1, N
         TEMP = ALPHA*DCONJG(X(IX))
         IF (UPPER) THEN
            IF (INCX .GE. 0) THEN
               CALL ZAXPY (J-1, TEMP, X, INCX, A(LDA*(J-1)+1), 1)
            ELSE
               CALL ZAXPY (J-1, TEMP, X(IX-INCX), INCX,
     &                     A(LDA*(J-1)+1), 1)
            END IF
         ELSE
            IF (INCX .GE. 0) THEN
               CALL ZAXPY (N-J, TEMP, X(IX+INCX), INCX,
     &                     A(LDA*(J-1)+J+1), 1)
            ELSE
               CALL ZAXPY (N-J, TEMP, X, INCX, A(LDA*(J-1)+J+1), 1)
            END IF
         END IF
         TEMP1 = A(LDA*(J-1)+J)
         TEMP2 = X(IX)*TEMP
         TEMP3 = DBLE(TEMP1)
         TEMP4 = DBLE(TEMP2)
         A(LDA*(J-1)+J) = TEMP3 + TEMP4
         IX = IX + INCX
   10 CONTINUE
C
 9000 CONTINUE
      GOTO 9999
 9999 CONTINUE
      END
