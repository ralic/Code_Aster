      SUBROUTINE ZMVPY (UPLO, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, LDA, INCX, INCY
      COMPLEX    *16 ALPHA, BETA, A(LDA,*), X(*), Y(*)
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
C  PRODUIT D'UNE MATRICE HERMITIENNE PAR UN VECTEUR SUIVANT OPTION
C                         'U' OU 'L'.
C-----------------------------------------------------------------------
C IN  : UPLO : CARACTERE SPECIFIANT LE MODE DE STOCKAGE.
C                 UPLO              STRUCTURE
C              'U'             MATRICE TRIANGULAIRE SUPERIEURE.
C              'L'             MATRICE TRIANGULAIRE INFERIEURE
C     : N    : DIMENSION DE LA MATRICE.
C     : ALPHA: COMPLEXE.
C     : A    : MATRICE COMPLXE DE DIMENSION M*N.
C     : LDA  : DIMENSION DE A.
C     : X    : DVECTEUR COMPLXE DE LONGUEUR (N-1)*IABS(INCX)+1.
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
C     : BETA : COMPLEXE.LORSQUE BETA EGAL ZERO, Y N'EST PAS CALCULE.
C     : Y    : VECTEUR COMPLEXE DE LONGUEUR (N-1)*IABS(INCY)+1.
C     : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE Y.
C-----------------------------------------------------------------------
      INTEGER    I, IX, IY, J, KY
      COMPLEX    *16 TEMP
      INTEGER    IABS
      REAL *8 DBLE
      COMPLEX    *16 CONJG
C
      IF (N.EQ.0 .OR.
     &    (ALPHA.EQ.(0.0D0,0.0D0).AND.BETA.EQ.(1.0D0,0.0D0)))
     &    GO TO 9000
C
      IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
C
      IF (BETA .EQ. (1.0D0,0.0D0)) THEN
      ELSE IF (INCY .EQ. 0) THEN
         IF (BETA .EQ. (0.0D0,0.0D0)) THEN
            Y(1) = (0.0D0,0.0D0)
         ELSE
            Y(1) = BETA**N*Y(1)
         END IF
      ELSE IF (BETA .EQ. (0.0D0,0.0D0)) THEN
         CALL ZINIT (N, (0.0D0,0.0D0), Y, ABS(INCY))
      ELSE
         CALL ZMULT (N, BETA, Y, ABS(INCY))
      END IF
C
      IF (ALPHA .EQ. (0.0D0,0.0D0)) GO TO 9000
C
      IF (UPLO(1:1).EQ.'U' .OR. UPLO(1:1).EQ.'u') THEN
         DO 20  J=1, N
            TEMP = ALPHA*X(IX)
            KY = IY + (J-2)*MIN(INCY,0)
            CALL ZAXPY (J-1, TEMP, A(1,J), 1, Y(KY), INCY)
            KY = IY + (J-1)*INCY
            Y(KY) = Y(KY) + TEMP*DBLE(A(J,J))
            DO 10  I=J + 1, N
               KY = KY + INCY
               Y(KY) = Y(KY) + TEMP*DCONJG(A(J,I))
   10       CONTINUE
            IX = IX + INCX
   20    CONTINUE
      ELSE
         DO 40  J=1, N
            TEMP = ALPHA*X(IX)
            KY = IY
            DO 30  I=1, J - 1
               Y(KY) = Y(KY) + TEMP*DCONJG(A(J,I))
               KY = KY + INCY
   30       CONTINUE
            Y(KY) = Y(KY) + TEMP*DBLE(A(J,J))
            KY = KY + INCY + (N-J-1)*MIN(INCY,0)
            CALL ZAXPY (N-J, TEMP, A(J+1,J), 1, Y(KY), INCY)
            IX = IX + INCX
   40    CONTINUE
      END IF
C
 9000 CONTINUE
      END
