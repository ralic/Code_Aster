      SUBROUTINE GLACPY( UPLO, M, N, A, LDA, B, LDB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C  PURPOSE
C  =======
C
C  GLACPY COPIES ALL OR PART OF A TWO-DIMENSIONAL MATRIX A TO ANOTHER
C  MATRIX B.
C
C  ARGUMENTS
C  =========
C
C  UPLO    (INPUT) CHARACTER*1
C          SPECIFIES THE PART OF THE MATRIX A TO BE COPIED TO B.
C          = 'U':      UPPER TRIANGULAR PART
C          = 'L':      LOWER TRIANGULAR PART
C          OTHERWISE:  ALL OF THE MATRIX A
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT) COMPLEX*16 ARRAY, DIMENSION (LDA,N)
C          THE M BY N MATRIX A.  IF UPLO = 'U', ONLY THE UPPER TRAPEZIUM
C          IS ACCESSED; IF UPLO = 'L', ONLY THE LOWER TRAPEZIUM IS
C          ACCESSED.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
C
C  B       (OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDB,N)
C          ON EXIT, B = A IN THE LOCATIONS SPECIFIED BY UPLO.
C
C  LDB     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY B.  LDB >= MAX(1,M).
C
C  =====================================================================
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        UPLO
      INTEGER            LDA, LDB, M, N
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         A( LDA, * ), B( LDB, * )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, J
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     ..
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( LLSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 1, N
            DO 10 I = 1, MIN( J, M )
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
C
      ELSE IF( LLSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, N
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
C
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
C
 1000 CONTINUE
C
C     END OF GLACPY
C
      END
