      REAL*8 FUNCTION GLANHS( NORM, N, A, LDA, WORK )
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
C  GLANHS  RETURNS THE VALUE OF THE ONE NORM,  OR THE FROBENIUS NORM, OR
C  THE  INFINITY NORM,  OR THE  ELEMENT OF  LARGEST ABSOLUTE VALUE  OF A
C  HESSENBERG MATRIX A.
C
C  DESCRIPTION
C  ===========
C
C  GLANHS RETURNS THE VALUE
C
C     GLANHS = ( MAX(ABS(A(I,J))), NORM = 'M' OR 'M'
C              (
C              ( NORM1(A),         NORM = '1', 'O' OR 'O'
C              (
C              ( NORMI(A),         NORM = 'I' OR 'I'
C              (
C              ( NORMF(A),         NORM = 'F', 'F', 'E' OR 'E'
C
C  WHERE  NORM1  DENOTES THE  ONE NORM OF A MATRIX (MAXIMUM COLUMN SUM),
C  NORMI  DENOTES THE  INFINITY NORM  OF A MATRIX  (MAXIMUM ROW SUM) AND
C  NORMF  DENOTES THE  FROBENIUS NORM OF A MATRIX (SQUARE ROOT OF SUM OF
C  SQUARES).  NOTE THAT  MAX(ABS(A(I,J)))  IS NOT A  MATRIX NORM.
C
C  ARGUMENTS
C  =========
C
C  NORM    (INPUT) CHARACTER*1
C          SPECIFIES THE VALUE TO BE RETURNED IN GLANHS AS DESCRIBED
C          ABOVE.
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX A.  N >= 0.  WHEN N = 0, GLANHS IS
C          SET TO ZERO.
C
C  A       (INPUT) COMPLEX*16 ARRAY, DIMENSION (LDA,N)
C          THE N BY N UPPER HESSENBERG MATRIX A; THE PART OF A BELOW THE
C          FIRST SUB-DIAGONAL IS NOT REFERENCED.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(N,1).
C
C  WORK    (WORKSPACE) DOUBLE PRECISION ARRAY, DIMENSION (LWORK),
C          WHERE LWORK >= N WHEN NORM = 'I'; OTHERWISE, WORK IS NOT
C          REFERENCED.
C
C =====================================================================
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
      CHARACTER*1        NORM
      INTEGER            LDA, N
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8             WORK( * )
      COMPLEX*16         A( LDA, * )
C     ..
C     .. PARAMETERS ..
      REAL*8             ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, J
      REAL*8             SCALE, SUM, VALUE
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LLSAME( NORM, 'M' ) ) THEN
C
C        FIND MAX(ABS(A(I,J))).
C
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, MIN( N, J+1 )
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LLSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
C
C        FIND NORM1(A).
C
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, MIN( N, J+1 )
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LLSAME( NORM, 'I' ) ) THEN
C
C        FIND NORMI(A).
C
         DO 50 I = 1, N
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, MIN( N, J+1 )
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, N
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF(( LLSAME( NORM, 'F' )) .OR. ( LLSAME( NORM, 'E' ))) THEN
C
C        FIND NORMF(A).
C
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL GLASSQ( MIN( N, J+1 ), A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
C
      GLANHS = VALUE
 1000 CONTINUE
C
C     END OF GLANHS
C
      END
