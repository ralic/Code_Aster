      SUBROUTINE DLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/02/2000   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C 
C     SUBROUTINE LAPACK INITIALISANT UNE MATRICE TRIDIAGONALE 
C     SYMETRIQUE.
C-----------------------------------------------------------------------
C
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C  PURPOSE
C  =======
C
C  DLASET INITIALIZES AN M-BY-N MATRIX A TO BETA ON THE DIAGONAL AND
C  ALPHA ON THE OFFDIAGONALS.
C
C  ARGUMENTS
C  =========
C
C  UPLO    (INPUT) CHARACTER*1
C          SPECIFIES THE PART OF THE MATRIX A TO BE SET.
C          = 'U':      UPPER TRIANGULAR PART IS SET, THE STRICTLY LOWER
C                      TRIANGULAR PART OF A IS NOT CHANGED.
C          = 'L':      LOWER TRIANGULAR PART IS SET, THE STRICTLY UPPER
C                      TRIANGULAR PART OF A IS NOT CHANGED.
C          OTHERWISE:  ALL OF THE MATRIX A IS SET.
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
C
C  ALPHA   (INPUT) REAL*8
C          THE CONSTANT TO WHICH THE OFFDIAGONAL ELEMENTS ARE TO BE SET.
C
C  BETA    (INPUT) REAL*8
C          THE CONSTANT TO WHICH THE DIAGONAL ELEMENTS ARE TO BE SET.
C
C  A       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDA,N)
C          ON EXIT, THE LEADING M-BY-N SUBMATRIX OF A IS SET AS FOLLOWS:
C
C          IF UPLO = 'U', A(I,J) = ALPHA, 1<=I<=J-1, 1<=J<=N,
C          IF UPLO = 'L', A(I,J) = ALPHA, J+1<=I<=M, 1<=J<=N,
C          OTHERWISE,     A(I,J) = ALPHA, 1<=I<=M, 1<=J<=N, I.NE.J,
C
C          AND, FOR ALL UPLO, A(I,I) = BETA, 1<=I<=MIN(M,N).
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
C
C-----------------------------------------------------------------------
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      CHARACTER*1   UPLO
      INTEGER       LDA, M, N
      REAL*8   ALPHA, BETA
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   A( LDA, * )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, J
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( LSAME( UPLO, 'U' ) ) THEN
C
C        SET THE STRICTLY UPPER TRIANGULAR OR TRAPEZOIDAL PART OF THE
C        ARRAY TO ALPHA.
C
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
C
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
C
C        SET THE STRICTLY LOWER TRIANGULAR OR TRAPEZOIDAL PART OF THE
C        ARRAY TO ALPHA.
C
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
C
      ELSE
C
C        SET THE LEADING M-BY-N SUBMATRIX TO ALPHA.
C
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      END IF
C
C     SET THE FIRST MIN(M,N) DIAGONAL ELEMENTS TO BETA.
C
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
C
C     END OF DLASET
C
      END
