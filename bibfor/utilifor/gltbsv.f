      SUBROUTINE GLTBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C  Purpose
C  =======
C
C  GLTBSV  solves one of the systems of equations
C
C     A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b,
C
C  where b and x are n element vectors and A is an n by n unit, or
C  non-unit, upper or lower triangular band matrix, with ( k + 1 )
C  diagonals.
C
C  No test for singularity or near-singularity is included in this
C  routine. Such tests must be performed before calling this routine.
C
C  Parameters
C  ==========
C
C  UPLO   - CHARACTER*1.
C           On entry, UPLO specifies whether the matrix is an upper or
C           lower triangular matrix as follows:
C
C              UPLO = 'U' or 'u'   A is an upper triangular matrix.
C
C              UPLO = 'L' or 'l'   A is a lower triangular matrix.
C
C           Unchanged on exit.
C
C  TRANS  - CHARACTER*1.
C           On entry, TRANS specifies the equations to be solved as
C           follows:
C
C              TRANS = 'N' or 'n'   A*x = b.
C
C              TRANS = 'T' or 't'   A'*x = b.
C
C             TRANS = 'C' or 'c'   conjg( A' )*x = b.
C
C           Unchanged on exit.
C
C  DIAG   - CHARACTER*1.
C           On entry, DIAG specifies whether or not A is unit
C           triangular as follows:
C
C              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
C
C              DIAG = 'N' or 'n'   A is not assumed to be unit
C                                  triangular.
C
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the order of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  K      - INTEGER.
C           On entry with UPLO = 'U' or 'u', K specifies the number of
C           super-diagonals of the matrix A.
C           On entry with UPLO = 'L' or 'l', K specifies the number of
C           sub-diagonals of the matrix A.
C           K must satisfy  0 .le. K.
C           Unchanged on exit.
C
C  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
C           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
C           by n part of the array A must contain the upper triangular
C           band part of the matrix of coefficients, supplied column by
C           column, with the leading diagonal of the matrix in row
C           ( k + 1 ) of the array, the first super-diagonal starting at
C           position 2 in row k, and so on. The top left k by k triangle
C           of the array A is not referenced.
C           The following program segment will transfer an upper
C           triangular band matrix from conventional full matrix storage
C           to band storage:
C
C                 DO 20, J = 1, N
C                    M = K + 1 - J
C                    DO 10, I = MAX( 1, J - K ), J
C                       A( M + I, J ) = matrix( I, J )
C              10    CONTINUE
C              20 CONTINUE
C
C           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
C           by n part of the array A must contain the lower triangular
C           band part of the matrix of coefficients, supplied column by
C           column, with the leading diagonal of the matrix in row 1 of
C           the array, the first sub-diagonal starting at position 1 in
C           row 2, and so on. The bottom right k by k triangle of the
C           array A is not referenced.
C           The following program segment will transfer a lower
C           triangular band matrix from conventional full matrix storage
C           to band storage:
C
C                 DO 20, J = 1, N
C                    M = 1 - J
C                    DO 10, I = J, MIN( N, J + K )
C                       A( M + I, J ) = matrix( I, J )
C              10    CONTINUE
C              20 CONTINUE
C
C           Note that when DIAG = 'U' or 'u' the elements of the array A
C           corresponding to the diagonal elements of the matrix are not
C          referenced, but are assumed to be unity.
C           Unchanged on exit.
C
C   LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. LDA must be at least
C           ( k + 1 ).
C           Unchanged on exit.
C
C  X      - COMPLEX*16       array of dimension at least
C           ( 1 + ( n - 1 )*abs( INCX ) ).
C           Before entry, the incremented array X must contain the n
C           element right-hand side vector b. On exit, X is overwritten
C           with the solution vector x.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C
C  Level 2 Blas routine.
C
C  -- Written on 22-October-1986.
C     Jack Dongarra, Argonne National Lab.
C     Jeremy Du Croz, Nag Central Office.
C     Sven Hammarling, Nag Central Office.
C     Richard Hanson, Sandia National Labs.
C
C======================================================================
C REMPLACE LA BLAS ZTBSV SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. Scalar Arguments ..
      INTEGER            INCX, K, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
C     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), X( * )
C     ..
C     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. Local Scalars ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JX, KPLUS1, KX, L
      LOGICAL            NOCONJ, NOUNIT
C     .. External Functions ..
      LOGICAL            LLSAME
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF     ( .NOT.LLSAME( UPLO , 'U' ).AND.
     $         .NOT.LLSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LLSAME( TRANS, 'N' ).AND.
     $         .NOT.LLSAME( TRANS, 'T' ).AND.
     $         .NOT.LLSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LLSAME( DIAG , 'U' ).AND.
     $         .NOT.LLSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( K.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( K + 1 ) )THEN
         INFO = 7
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'GLTBSV', INFO )
         GOTO 9999
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     $   GOTO 9999
C
      NOCONJ = LLSAME( TRANS, 'T' )
      NOUNIT = LLSAME( DIAG , 'N' )
C
C     Set up the start point in X if the increment is not unity. This
C     will be  ( N - 1 )*INCX  too small for descending loops.
C
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
C
C     Start the operations. In this version the elements of A are
C     accessed by sequentially with one pass through A.
C
      IF( LLSAME( TRANS, 'N' ) )THEN
C
C        Form  x := inv( A )*x.
C
         IF( LLSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 20, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     L = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( KPLUS1, J )
                     TEMP = X( J )
                     DO 10, I = J - 1, MAX( 1, J - K ), -1
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 40, J = N, 1, -1
                  KX = KX - INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = KPLUS1 - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( KPLUS1, J )
                     TEMP = X( JX )
                     DO 30, I = J - 1, MAX( 1, J - K ), -1
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      - INCX
   30                CONTINUE
                  END IF
                  JX = JX - INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     L = 1 - J
                     IF( NOUNIT )
     $                  X( J ) = X( J )/A( 1, J )
                     TEMP = X( J )
                     DO 50, I = J + 1, MIN( N, J + K )
                        X( I ) = X( I ) - TEMP*A( L + I, J )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE
               JX = KX
               DO 80, J = 1, N
                  KX = KX + INCX
                  IF( X( JX ).NE.ZERO )THEN
                     IX = KX
                     L  = 1  - J
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )/A( 1, J )
                     TEMP = X( JX )
                     DO 70, I = J + 1, MIN( N, J + K )
                        X( IX ) = X( IX ) - TEMP*A( L + I, J )
                        IX      = IX      + INCX
   70                CONTINUE
                  END IF
                  JX = JX + INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
C
C        Form  x := inv( A' )*x  or  x := inv( conjg( A') )*x.
C
         IF( LLSAME( UPLO, 'U' ) )THEN
            KPLUS1 = K + 1
            IF( INCX.EQ.1 )THEN
               DO 110, J = 1, N
                  TEMP = X( J )
                  L    = KPLUS1 - J
                  IF( NOCONJ )THEN
                     DO 90, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - A( L + I, J )*X( I )
   90                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( KPLUS1, J )
                  ELSE
                     DO 100, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( I )
  100                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( KPLUS1, J ) )
                  END IF
                  X( J ) = TEMP
  110          CONTINUE
            ELSE
               JX = KX
               DO 140, J = 1, N
                  TEMP = X( JX )
                  IX   = KX
                  L    = KPLUS1  - J
                  IF( NOCONJ )THEN
                     DO 120, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - A( L + I, J )*X( IX )
                        IX   = IX   + INCX
  120                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( KPLUS1, J )
                  ELSE
                     DO 130, I = MAX( 1, J - K ), J - 1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( IX )
                        IX   = IX   + INCX
  130                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( KPLUS1, J ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   + INCX
                  IF( J.GT.K )
     $               KX = KX + INCX
  140          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 170, J = N, 1, -1
                  TEMP = X( J )
                  L    = 1      - J
                  IF( NOCONJ )THEN
                     DO 150, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - A( L + I, J )*X( I )
  150                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( 1, J )
                  ELSE
                     DO 160, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( I )
  160                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( 1, J ) )
                  END IF
                  X( J ) = TEMP
  170          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 200, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = KX
                  L    = 1       - J
                  IF( NOCONJ )THEN
                     DO 180, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - A( L + I, J )*X( IX )
                        IX   = IX   - INCX
  180                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( 1, J )
                  ELSE
                     DO 190, I = MIN( N, J + K ), J + 1, -1
                        TEMP = TEMP - DCONJG( A( L + I, J ) )*X( IX )
                        IX   = IX   - INCX
  190                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/DCONJG( A( 1, J ) )
                  END IF
                  X( JX ) = TEMP
                  JX      = JX   - INCX
                  IF( ( N - J ).GE.K )
     $               KX = KX - INCX
  200          CONTINUE
            END IF
         END IF
      END IF
C
9999  CONTINUE
C
C     End of GLTBSV .
C
      END
