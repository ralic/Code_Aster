      SUBROUTINE GLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C
C  PURPOSE
C  =======
C
C  GLASET INITIALIZES A 2-D ARRAY A TO BETA ON THE DIAGONAL AND
C  ALPHA ON THE OFFDIAGONALS.
C
C  ARGUMENTS
C  =========
C
C  UPLO    (INPUT) CHARACTER*1
C          SPECIFIES THE PART OF THE MATRIX A TO BE SET.
C          = 'U':      UPPER TRIANGULAR PART IS SET. THE LOWER TRIANGLE
C                      IS UNCHANGED.
C          = 'L':      LOWER TRIANGULAR PART IS SET. THE UPPER TRIANGLE
C                      IS UNCHANGED.
C          OTHERWISE:  ALL OF THE MATRIX A IS SET.
C
C  M       (INPUT) INTEGER
C          ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF A.
C
C  N       (INPUT) INTEGER
C          ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF A.
C
C  ALPHA   (INPUT) COMPLEX*16
C          ALL THE OFFDIAGONAL ARRAY ELEMENTS ARE SET TO ALPHA.
C
C  BETA    (INPUT) COMPLEX*16
C          ALL THE DIAGONAL ARRAY ELEMENTS ARE SET TO BETA.
C
C  A       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDA,N)
C          ON ENTRY, THE M BY N MATRIX A.
C          ON EXIT, A(I,J) = ALPHA, 1 <= I <= M, 1 <= J <= N, I.NE.J;
C                   A(I,I) = BETA , 1 <= I <= MIN(M,N)
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
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
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        UPLO
      INTEGER            LDA, M, N
      COMPLEX*16         ALPHA, BETA
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         A( LDA, * )
C     ..
C
C     .. LOCAL SCALARS ..
      INTEGER            I, J
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( LLSAME( UPLO, 'U' ) ) THEN
C
C        SET THE DIAGONAL TO BETA AND THE STRICTLY UPPER TRIANGULAR
C        PART OF THE ARRAY TO ALPHA.
C
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
         DO 30 I = 1, MIN( N, M )
            A( I, I ) = BETA
   30    CONTINUE
C
      ELSE IF( LLSAME( UPLO, 'L' ) ) THEN
C
C        SET THE DIAGONAL TO BETA AND THE STRICTLY LOWER TRIANGULAR
C        PART OF THE ARRAY TO ALPHA.
C
         DO 50 J = 1, MIN( M, N )
            DO 40 I = J + 1, M
               A( I, J ) = ALPHA
   40       CONTINUE
   50    CONTINUE
         DO 60 I = 1, MIN( N, M )
            A( I, I ) = BETA
   60    CONTINUE
C
      ELSE
C
C        SET THE ARRAY TO BETA ON THE DIAGONAL AND ALPHA ON THE
C        OFFDIAGONAL.
C
         DO 80 J = 1, N
            DO 70 I = 1, M
               A( I, J ) = ALPHA
   70       CONTINUE
   80    CONTINUE
         DO 90 I = 1, MIN( M, N )
            A( I, I ) = BETA
   90    CONTINUE
      END IF
C
 1000 CONTINUE
C
C     END OF GLASET
C
      END
