      SUBROUTINE GLATRS( UPLO, TRANS, DIAG, NORMIN, N, A, LDA, X, SCALE,
     $                   CNORM, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     JUNE 30, 1992
C
C
C  PURPOSE
C  =======
C
C  GLATRS SOLVES ONE OF THE TRIANGULAR SYSTEMS
C
C     A * X = S*B,  A**T * X = S*B,  OR  A**H * X = S*B,
C
C  WITH SCALING TO PREVENT OVERFLOW.  HERE A IS AN UPPER OR LOWER
C  TRIANGULAR MATRIX, A**T DENOTES THE TRANSPOSE OF A, A**H DENOTES THE
C  CONJUGATE TRANSPOSE OF A, X AND B ARE N-ELEMENT VECTORS, AND S IS A
C  SCALING FACTOR, USUALLY LESS THAN OR EQUAL TO 1, CHOSEN SO THAT THE
C  COMPONENTS OF X WILL BE LESS THAN THE OVERFLOW THRESHOLD.  IF THE
C  UNSCALED PROBLEM WILL NOT CAUSE OVERFLOW, THE LEVEL 2 BLAS ROUTINE
C  GLTRSV IS CALLED. IF THE MATRIX A IS SINGULAR (A(J,J) = 0 FOR SOME J)
C  THEN S IS SET TO 0 AND A NON-TRIVIAL SOLUTION TO A*X = 0 IS RETURNED.
C
C  ARGUMENTS
C  =========
C
C  UPLO    (INPUT) CHARACTER*1
C          SPECIFIES WHETHER THE MATRIX A IS UPPER OR LOWER TRIANGULAR.
C          = 'U':  UPPER TRIANGULAR
C          = 'L':  LOWER TRIANGULAR
C
C  TRANS   (INPUT) CHARACTER*1
C          SPECIFIES THE OPERATION APPLIED TO A.
C          = 'N':  SOLVE A * X = S*B     (NO TRANSPOSE)
C          = 'T':  SOLVE A**T * X = S*B  (TRANSPOSE)
C          = 'C':  SOLVE A**H * X = S*B  (CONJUGATE TRANSPOSE)
C
C  DIAG    (INPUT) CHARACTER*1
C          SPECIFIES WHETHER OR NOT THE MATRIX A IS UNIT TRIANGULAR.
C          = 'N':  NON-UNIT TRIANGULAR
C          = 'U':  UNIT TRIANGULAR
C
C  NORMIN  (INPUT) CHARACTER*1
C          SPECIFIES WHETHER CNORM HAS BEEN SET OR NOT.
C          = 'Y':  CNORM CONTAINS THE COLUMN NORMS ON ENTRY
C          = 'N':  CNORM IS NOT SET ON ENTRY.  ON EXIT, THE NORMS WILL
C                  BE COMPUTED AND STORED IN CNORM.
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT) COMPLEX*16 ARRAY, DIMENSION (LDA,N)
C          THE TRIANGULAR MATRIX A.  IF UPLO = 'U', THE LEADING N BY N
C          UPPER TRIANGULAR PART OF THE ARRAY A CONTAINS THE UPPER
C          TRIANGULAR MATRIX, AND THE STRICTLY LOWER TRIANGULAR PART OF
C          A IS NOT REFERENCED.  IF UPLO = 'L', THE LEADING N BY N LOWER
C          TRIANGULAR PART OF THE ARRAY A CONTAINS THE LOWER TRIANGULAR
C          MATRIX, AND THE STRICTLY UPPER TRIANGULAR PART OF A IS NOT
C          REFERENCED.  IF DIAG = 'U', THE DIAGONAL ELEMENTS OF A ARE
C          ALSO NOT REFERENCED AND ARE ASSUMED TO BE 1.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX (1,N).
C
C  X       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (N)
C          ON ENTRY, THE RIGHT HAND SIDE B OF THE TRIANGULAR SYSTEM.
C          ON EXIT, X IS OVERWRITTEN BY THE SOLUTION VECTOR X.
C
C  SCALE   (OUTPUT) DOUBLE PRECISION
C          THE SCALING FACTOR S FOR THE TRIANGULAR SYSTEM
C             A * X = S*B,  A**T * X = S*B,  OR  A**H * X = S*B.
C          IF SCALE = 0, THE MATRIX A IS SINGULAR OR BADLY SCALED, AND
C          THE VECTOR X IS AN EXACT OR APPROXIMATE SOLUTION TO A*X = 0.
C
C  CNORM   (INPUT OR OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (N)
C
C          IF NORMIN = 'Y', CNORM IS AN INPUT ARGUMENT AND CNORM(J)
C          CONTAINS THE NORM OF THE OFF-DIAGONAL PART OF THE J-TH COLUMN
C          OF A.  IF TRANS = 'N', CNORM(J) MUST BE GREATER THAN OR EQUAL
C          TO THE INFINITY-NORM, AND IF TRANS = 'T' OR 'C', CNORM(J)
C          MUST BE GREATER THAN OR EQUAL TO THE 1-NORM.
C
C          IF NORMIN = 'N', CNORM IS AN OUTPUT ARGUMENT AND CNORM(J)
C          RETURNS THE 1-NORM OF THE OFFDIAGONAL PART OF THE J-TH COLUMN
C          OF A.
C
C  INFO    (OUTPUT) INTEGER
C          = 0:  SUCCESSFUL EXIT
C          < 0:  IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
C
C  FURTHER DETAILS
C  ======= =======
C
C  A ROUGH BOUND ON X IS COMPUTED; IF THAT IS LESS THAN OVERFLOW, GLTRSV
C  IS CALLED, OTHERWISE, SPECIFIC CODE IS USED WHICH CHECKS FOR POSSIBLE
C  OVERFLOW OR DIVIDE-BY-ZERO AT EVERY OPERATION.
C
C  A COLUMNWISE SCHEME IS USED FOR SOLVING A*X = B.  THE BASIC ALGORITHM
C  IF A IS LOWER TRIANGULAR IS
C
C       X[1:N] := B[1:N]
C       FOR J = 1, ..., N
C            X(J) := X(J) / A(J,J)
C            X[J+1:N] := X[J+1:N] - X(J) * A[J+1:N,J]
C       END
C
C  DEFINE BOUNDS ON THE COMPONENTS OF X AFTER J ITERATIONS OF THE LOOP:
C     M(J) = BOUND ON X[1:J]
C     G(J) = BOUND ON X[J+1:N]
C  INITIALLY, LET M(0) = 0 AND G(0) = MAX{X(I), I=1,...,N}.
C
C  THEN FOR ITERATION J+1 WE HAVE
C     M(J+1) <= G(J) / | A(J+1,J+1) |
C     G(J+1) <= G(J) + M(J+1) * | A[J+2:N,J+1] |
C            <= G(J) ( 1 + CNORM(J+1) / | A(J+1,J+1) | )
C
C  WHERE CNORM(J+1) IS GREATER THAN OR EQUAL TO THE INFINITY-NORM OF
C  COLUMN J+1 OF A, NOT COUNTING THE DIAGONAL.  HENCE
C
C     G(J) <= G(0) PRODUCT ( 1 + CNORM(I) / | A(I,I) | )
C                  1<=I<=J
C  AND
C
C     |X(J)| <= ( G(0) / |A(J,J)| ) PRODUCT ( 1 + CNORM(I) / |A(I,I)| )
C                                   1<=I< J
C
C  SINCE |X(J)| <= M(J), WE USE THE LEVEL 2 BLAS ROUTINE GLTRSV IF THE
C  RECIPROCAL OF THE LARGEST M(J), J=1,..,N, IS LARGER THAN
C  MAX(UNDERFLOW, 1/OVERFLOW).
C
C  THE BOUND ON X(J) IS ALSO USED TO DETERMINE WHEN A STEP IN THE
C  COLUMNWISE METHOD CAN BE PERFORMED WITHOUT FEAR OF OVERFLOW.  IF
C  THE COMPUTED BOUND IS GREATER THAN A LARGE CONSTANT, X IS SCALED TO
C  PREVENT OVERFLOW, BUT IF THE BOUND OVERFLOWS, X IS SET TO 0, X(J) TO
C  1, AND SCALE TO 0, AND A NON-TRIVIAL SOLUTION TO A*X = 0 IS FOUND.
C
C  SIMILARLY, A ROW-WISE SCHEME IS USED TO SOLVE A**T *X = B  OR
C  A**H *X = B.  THE BASIC ALGORITHM FOR A UPPER TRIANGULAR IS
C
C       FOR J = 1, ..., N
C            X(J) := ( B(J) - A[1:J-1,J]' * X[1:J-1] ) / A(J,J)
C       END
C
C  WE SIMULTANEOUSLY COMPUTE TWO BOUNDS
C       G(J) = BOUND ON ( B(I) - A[1:I-1,I]' * X[1:I-1] ), 1<=I<=J
C       M(J) = BOUND ON X(I), 1<=I<=J
C
C  THE INITIAL VALUES ARE G(0) = 0, M(0) = MAX{B(I), I=1,..,N}, AND WE
C  ADD THE CONSTRAINT G(J) >= G(J-1) AND M(J) >= M(J-1) FOR J >= 1.
C  THEN THE BOUND ON X(J) IS
C
C       M(J) <= M(J-1) * ( 1 + CNORM(J) ) / | A(J,J) |
C
C            <= M(0) * PRODUCT ( ( 1 + CNORM(I) ) / |A(I,I)| )
C                      1<=I<=J
C
C  AND WE CAN SAFELY CALL GLTRSV IF 1/M(N) AND 1/G(N) ARE BOTH GREATER
C  THAN MAX(UNDERFLOW, 1/OVERFLOW).
C
C  =====================================================================
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C TOLE CRP_20
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        DIAG, NORMIN, TRANS, UPLO
      INTEGER            INFO, LDA, N
      REAL*8             SCALE
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8             CNORM( * )
      COMPLEX*16         A( LDA, * ), X( * )
C     ..
C     .. PARAMETERS ..
      REAL*8             ZERO, HALF, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0,
     $                   TWO = 2.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            NOTRAN, NOUNIT, UPPER
      INTEGER            I, IMAX, J, JFIRST, JINC, JLAST
      REAL*8             BIGNUM, GROW, REC, SMLNUM, TJJ, TMAX, TSCAL,
     $                   XBND, XJ, XMAX
      COMPLEX*16         CSUMJ, TJJS, USCAL, ZDUM
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
      INTEGER            IDAMAX, GLAMAX
      REAL*8             GLASUM, R8MIEM
      COMPLEX*16         GLDOTC, GLDOTU, GLADIV
C     ..
C     .. STATEMENT FUNCTIONS ..
      REAL*8             CABS1, CABS2
C     ..
C     .. STATEMENT FUNCTION DEFINITIONS ..
      CABS1( ZDUM ) = ABS( DBLE( ZDUM ) ) + ABS( DIMAG( ZDUM ) )
      CABS2( ZDUM ) = ABS( DBLE( ZDUM ) / 2.D0 ) +
     $                ABS( DIMAG( ZDUM ) / 2.D0 )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      INFO = 0
      UPPER = LLSAME( UPLO, 'U' )
      NOTRAN = LLSAME( TRANS, 'N' )
      NOUNIT = LLSAME( DIAG, 'N' )
C
C     TEST THE INPUT PARAMETERS.
C
      IF( .NOT.UPPER .AND. .NOT.LLSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LLSAME( TRANS, 'T' ) .AND. .NOT.
     $         LLSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LLSAME( DIAG, 'U' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LLSAME( NORMIN, 'Y' ) .AND. .NOT.
     $         LLSAME( NORMIN, 'N' ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'GLATRS', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.EQ.0 )
     $   GOTO 1000
C
C     DETERMINE MACHINE DEPENDENT PARAMETERS TO CONTROL OVERFLOW.
C
      SMLNUM = R8MIEM()
      BIGNUM = ONE / SMLNUM
C    ALARME OLIVIER R8MIEM() REMPLACE DLAMCH( 'PRECISION' )
      SMLNUM = SMLNUM / R8MIEM()
      BIGNUM = ONE / SMLNUM
      SCALE = ONE
C
      IF( LLSAME( NORMIN, 'N' ) ) THEN
C
C        COMPUTE THE 1-NORM OF EACH COLUMN, NOT INCLUDING THE DIAGONAL.
C
         IF( UPPER ) THEN
C
C           A IS UPPER TRIANGULAR.
C
            DO 10 J = 1, N
               CNORM( J ) = GLASUM( J-1, A( 1, J ), 1 )
   10       CONTINUE
         ELSE
C
C           A IS LOWER TRIANGULAR.
C
            DO 20 J = 1, N - 1
               CNORM( J ) = GLASUM( N-J, A( J+1, J ), 1 )
   20       CONTINUE
            CNORM( N ) = ZERO
         END IF
      END IF
C
C     SCALE THE COLUMN NORMS BY TSCAL IF THE MAXIMUM ELEMENT IN CNORM IS
C     GREATER THAN BIGNUM/2.
C
      IMAX = IDAMAX( N, CNORM, 1 )
      TMAX = CNORM( IMAX )
      IF( TMAX.LE.BIGNUM*HALF ) THEN
         TSCAL = ONE
      ELSE
         TSCAL = HALF / ( SMLNUM*TMAX )
         CALL BLSCAL( N, TSCAL, CNORM, 1 )
      END IF
C
C     COMPUTE A BOUND ON THE COMPUTED SOLUTION VECTOR TO SEE IF THE
C     LEVEL 2 BLAS ROUTINE GLTRSV CAN BE USED.
C
      XMAX = ZERO
      DO 30 J = 1, N
         XMAX = MAX( XMAX, CABS2( X( J ) ) )
   30 CONTINUE
      XBND = XMAX
C
      IF( NOTRAN ) THEN
C
C        COMPUTE THE GROWTH IN A * X = B.
C
         IF( UPPER ) THEN
            JFIRST = N
            JLAST = 1
            JINC = -1
         ELSE
            JFIRST = 1
            JLAST = N
            JINC = 1
         END IF
C
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 60
         END IF
C
         IF( NOUNIT ) THEN
C
C           A IS NON-UNIT TRIANGULAR.
C
C           COMPUTE GROW = 1/G(J) AND XBND = 1/M(J).
C           INITIALLY, G(0) = MAX{X(I), I=1,...,N}.
C
            GROW = HALF / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 40 J = JFIRST, JLAST, JINC
C
C              EXIT THE LOOP IF THE GROWTH FACTOR IS TOO SMALL.
C
               IF( GROW.LE.SMLNUM )
     $            GO TO 60
C
               TJJS = A( J, J )
               TJJ = CABS1( TJJS )
C
               IF( TJJ.GE.SMLNUM ) THEN
C
C                 M(J) = G(J-1) / ABS(A(J,J))
C
                  XBND = MIN( XBND, MIN( ONE, TJJ )*GROW )
               ELSE
C
C                 M(J) COULD OVERFLOW, SET XBND TO 0.
C
                  XBND = ZERO
               END IF
C
               IF( TJJ+CNORM( J ).GE.SMLNUM ) THEN
C
C                 G(J) = G(J-1)*( 1 + CNORM(J) / ABS(A(J,J)) )
C
                  GROW = GROW*( TJJ / ( TJJ+CNORM( J ) ) )
               ELSE
C
C                 G(J) COULD OVERFLOW, SET GROW TO 0.
C
                  GROW = ZERO
               END IF
   40       CONTINUE
            GROW = XBND
         ELSE
C
C           A IS UNIT TRIANGULAR.
C
C           COMPUTE GROW = 1/G(J), WHERE G(0) = MAX{X(I), I=1,...,N}.
C
            GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) )
            DO 50 J = JFIRST, JLAST, JINC
C
C              EXIT THE LOOP IF THE GROWTH FACTOR IS TOO SMALL.
C
               IF( GROW.LE.SMLNUM )
     $            GO TO 60
C
C              G(J) = G(J-1)*( 1 + CNORM(J) )
C
               GROW = GROW*( ONE / ( ONE+CNORM( J ) ) )
   50       CONTINUE
         END IF
   60    CONTINUE
C
      ELSE
C
C        COMPUTE THE GROWTH IN A**T * X = B  OR  A**H * X = B.
C
         IF( UPPER ) THEN
            JFIRST = 1
            JLAST = N
            JINC = 1
         ELSE
            JFIRST = N
            JLAST = 1
            JINC = -1
         END IF
C
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 90
         END IF
C
         IF( NOUNIT ) THEN
C
C           A IS NON-UNIT TRIANGULAR.
C
C           COMPUTE GROW = 1/G(J) AND XBND = 1/M(J).
C           INITIALLY, M(0) = MAX{X(I), I=1,...,N}.
C
            GROW = HALF / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 70 J = JFIRST, JLAST, JINC
C
C              EXIT THE LOOP IF THE GROWTH FACTOR IS TOO SMALL.
C
               IF( GROW.LE.SMLNUM )
     $            GO TO 90
C
C              G(J) = MAX( G(J-1), M(J-1)*( 1 + CNORM(J) ) )
C
               XJ = ONE + CNORM( J )
               GROW = MIN( GROW, XBND / XJ )
C
               TJJS = A( J, J )
               TJJ = CABS1( TJJS )
C
               IF( TJJ.GE.SMLNUM ) THEN
C
C                 M(J) = M(J-1)*( 1 + CNORM(J) ) / ABS(A(J,J))
C
                  IF( XJ.GT.TJJ )
     $               XBND = XBND*( TJJ / XJ )
               ELSE
C
C                 M(J) COULD OVERFLOW, SET XBND TO 0.
C
                  XBND = ZERO
               END IF
   70       CONTINUE
            GROW = MIN( GROW, XBND )
         ELSE
C
C           A IS UNIT TRIANGULAR.
C
C           COMPUTE GROW = 1/G(J), WHERE G(0) = MAX{X(I), I=1,...,N}.
C
            GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) )
            DO 80 J = JFIRST, JLAST, JINC
C
C              EXIT THE LOOP IF THE GROWTH FACTOR IS TOO SMALL.
C
               IF( GROW.LE.SMLNUM )
     $            GO TO 90
C
C              G(J) = ( 1 + CNORM(J) )*G(J-1)
C
               XJ = ONE + CNORM( J )
               GROW = GROW / XJ
   80       CONTINUE
         END IF
   90    CONTINUE
      END IF
C
      IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN
C
C        USE THE LEVEL 2 BLAS SOLVE IF THE RECIPROCAL OF THE BOUND ON
C        ELEMENTS OF X IS NOT TOO SMALL.
C
         CALL GLTRSV( UPLO, TRANS, DIAG, N, A, LDA, X, 1 )
      ELSE
C
C        USE A LEVEL 1 BLAS SOLVE, SCALING INTERMEDIATE RESULTS.
C
         IF( XMAX.GT.BIGNUM*HALF ) THEN
C
C           SCALE X SO THAT ITS COMPONENTS ARE LESS THAN OR EQUAL TO
C           BIGNUM IN ABSOLUTE VALUE.
C
            SCALE = ( BIGNUM*HALF ) / XMAX
            CALL GLSCAL( N, SCALE, X, 1 )
            XMAX = BIGNUM
         ELSE
            XMAX = XMAX*TWO
         END IF
C
         IF( NOTRAN ) THEN
C
C           SOLVE A * X = B
C
            DO 120 J = JFIRST, JLAST, JINC
C
C              COMPUTE X(J) = B(J) / A(J,J), SCALING X IF NECESSARY.
C
               XJ = CABS1( X( J ) )
               IF( NOUNIT ) THEN
                  TJJS = A( J, J )*TSCAL
               ELSE
                  TJJS = TSCAL
                  IF( TSCAL.EQ.ONE )
     $               GO TO 110
               END IF
               TJJ = CABS1( TJJS )
               IF( TJJ.GT.SMLNUM ) THEN
C
C                    ABS(A(J,J)) > SMLNUM:
C
                  IF( TJJ.LT.ONE ) THEN
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
C
C                          SCALE X BY 1/B(J).
C
                        REC = ONE / XJ
                        CALL GLSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J ) = GLADIV( X( J ), TJJS )
                  XJ = CABS1( X( J ) )
               ELSE IF( TJJ.GT.ZERO ) THEN
C
C                    0 < ABS(A(J,J)) <= SMLNUM:
C
                  IF( XJ.GT.TJJ*BIGNUM ) THEN
C
C                       SCALE X BY (1/ABS(X(J)))*ABS(A(J,J))*BIGNUM
C                       TO AVOID OVERFLOW WHEN DIVIDING BY A(J,J).
C
                     REC = ( TJJ*BIGNUM ) / XJ
                     IF( CNORM( J ).GT.ONE ) THEN
C
C                          SCALE BY 1/CNORM(J) TO AVOID OVERFLOW WHEN
C                          MULTIPLYING X(J) TIMES COLUMN J.
C
                        REC = REC / CNORM( J )
                     END IF
                     CALL GLSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
                  X( J ) = GLADIV( X( J ), TJJS )
                  XJ = CABS1( X( J ) )
               ELSE
C
C                    A(J,J) = 0:  SET X(1:N) = 0, X(J) = 1, AND
C                    SCALE = 0, AND COMPUTE A SOLUTION TO A*X = 0.
C
                  DO 100 I = 1, N
                     X( I ) = ZERO
  100             CONTINUE
                  X( J ) = ONE
                  XJ = ONE
                  SCALE = ZERO
                  XMAX = ZERO
               END IF
  110          CONTINUE
C
C              SCALE X IF NECESSARY TO AVOID OVERFLOW WHEN ADDING A
C              MULTIPLE OF COLUMN J OF A.
C
               IF( XJ.GT.ONE ) THEN
                  REC = ONE / XJ
                  IF( CNORM( J ).GT.( BIGNUM-XMAX )*REC ) THEN
C
C                    SCALE X BY 1/(2*ABS(X(J))).
C
                     REC = REC*HALF
                     CALL GLSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                  END IF
               ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN
C
C                 SCALE X BY 1/2.
C
                  CALL GLSCAL( N, HALF, X, 1 )
                  SCALE = SCALE*HALF
               END IF
C
               IF( UPPER ) THEN
                  IF( J.GT.1 ) THEN
C
C                    COMPUTE THE UPDATE
C                       X(1:J-1) := X(1:J-1) - X(J) * A(1:J-1,J)
C
                     CALL GLAXPY( J-1, -X( J )*TSCAL, A( 1, J ), 1, X,
     $                           1 )
                     I = GLAMAX( J-1, X, 1 )
                     XMAX = CABS1( X( I ) )
                  END IF
               ELSE
                  IF( J.LT.N ) THEN
C
C                    COMPUTE THE UPDATE
C                       X(J+1:N) := X(J+1:N) - X(J) * A(J+1:N,J)
C
                     CALL GLAXPY( N-J, -X( J )*TSCAL, A( J+1, J ), 1,
     $                           X( J+1 ), 1 )
                     I = J + GLAMAX( N-J, X( J+1 ), 1 )
                     XMAX = CABS1( X( I ) )
                  END IF
               END IF
  120       CONTINUE
C
         ELSE IF( LLSAME( TRANS, 'T' ) ) THEN
C
C           SOLVE A**T * X = B
C
            DO 170 J = JFIRST, JLAST, JINC
C
C              COMPUTE X(J) = B(J) - SUM A(K,J)*X(K).
C                                    K<>J
C
               XJ = CABS1( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
C
C                 IF X(J) COULD OVERFLOW, SCALE X BY 1/(2*XMAX).
C
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = A( J, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.ONE ) THEN
C
C                       DIVIDE BY A(J,J) WHEN SCALING X IF A(J,J) > 1.
C
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = GLADIV( USCAL, TJJS )
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL GLSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
C
               CSUMJ = ZERO
               IF( USCAL.EQ.DCMPLX( ONE ) ) THEN
C
C                 IF THE SCALING NEEDED FOR A IN THE DOT PRODUCT IS 1,
C                 CALL GLDOTU TO PERFORM THE DOT PRODUCT.
C
                  IF( UPPER ) THEN
                     CSUMJ = GLDOTU( J-1, A( 1, J ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     CSUMJ = GLDOTU( N-J, A( J+1, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
C
C                 OTHERWISE, USE IN-LINE CODE FOR THE DOT PRODUCT.
C
                  IF( UPPER ) THEN
                     DO 130 I = 1, J - 1
                        CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I )
  130                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 140 I = J + 1, N
                        CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I )
  140                CONTINUE
                  END IF
               END IF
C
               IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN
C
C                 COMPUTE X(J) := ( X(J) - CSUMJ ) / A(J,J) IF 1/A(J,J)
C                 WAS NOT USED TO SCALE THE DOTPRODUCT.
C
                  X( J ) = X( J ) - CSUMJ
                  XJ = CABS1( X( J ) )
                  IF( NOUNIT ) THEN
                     TJJS = A( J, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     $                  GO TO 160
                  END IF
C
C                    COMPUTE X(J) = X(J) / A(J,J), SCALING IF NECESSARY.
C
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.SMLNUM ) THEN
C
C                       ABS(A(J,J)) > SMLNUM:
C
                     IF( TJJ.LT.ONE ) THEN
                        IF( XJ.GT.TJJ*BIGNUM ) THEN
C
C                             SCALE X BY 1/ABS(X(J)).
C
                           REC = ONE / XJ
                           CALL GLSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = GLADIV( X( J ), TJJS )
                  ELSE IF( TJJ.GT.ZERO ) THEN
C
C                       0 < ABS(A(J,J)) <= SMLNUM:
C
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
C
C                          SCALE X BY (1/ABS(X(J)))*ABS(A(J,J))*BIGNUM.
C
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL GLSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = GLADIV( X( J ), TJJS )
                  ELSE
C
C                       A(J,J) = 0:  SET X(1:N) = 0, X(J) = 1, AND
C                       SCALE = 0 AND COMPUTE A SOLUTION TO A**T *X = 0.
C
                     DO 150 I = 1, N
                        X( I ) = ZERO
  150                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  160             CONTINUE
               ELSE
C
C                 COMPUTE X(J) := X(J) / A(J,J) - CSUMJ IF THE DOT
C                 PRODUCT HAS ALREADY BEEN DIVIDED BY 1/A(J,J).
C
                  X( J ) = GLADIV( X( J ), TJJS ) - CSUMJ
               END IF
               XMAX = MAX( XMAX, CABS1( X( J ) ) )
  170       CONTINUE
C
         ELSE
C
C           SOLVE A**H * X = B
C
            DO 220 J = JFIRST, JLAST, JINC
C
C              COMPUTE X(J) = B(J) - SUM A(K,J)*X(K).
C                                    K<>J
C
               XJ = CABS1( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
C
C                 IF X(J) COULD OVERFLOW, SCALE X BY 1/(2*XMAX).
C
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = DCONJG( A( J, J ) )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.ONE ) THEN
C
C                       DIVIDE BY A(J,J) WHEN SCALING X IF A(J,J) > 1.
C
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = GLADIV( USCAL, TJJS )
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL GLSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
C
               CSUMJ = ZERO
               IF( USCAL.EQ.DCMPLX( ONE ) ) THEN
C
C                 IF THE SCALING NEEDED FOR A IN THE DOT PRODUCT IS 1,
C                 CALL GLDOTC TO PERFORM THE DOT PRODUCT.
C
                  IF( UPPER ) THEN
                     CSUMJ = GLDOTC( J-1, A( 1, J ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     CSUMJ = GLDOTC( N-J, A( J+1, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
C
C                 OTHERWISE, USE IN-LINE CODE FOR THE DOT PRODUCT.
C
                  IF( UPPER ) THEN
                     DO 180 I = 1, J - 1
                        CSUMJ = CSUMJ + ( DCONJG( A( I, J ) )*USCAL )*
     $                          X( I )
  180                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 190 I = J + 1, N
                        CSUMJ = CSUMJ + ( DCONJG( A( I, J ) )*USCAL )*
     $                          X( I )
  190                CONTINUE
                  END IF
               END IF
C
               IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN
C
C                 COMPUTE X(J) := ( X(J) - CSUMJ ) / A(J,J) IF 1/A(J,J)
C                 WAS NOT USED TO SCALE THE DOTPRODUCT.
C
                  X( J ) = X( J ) - CSUMJ
                  XJ = CABS1( X( J ) )
                  IF( NOUNIT ) THEN
                     TJJS = DCONJG( A( J, J ) )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     $                  GO TO 210
                  END IF
C
C                    COMPUTE X(J) = X(J) / A(J,J), SCALING IF NECESSARY.
C
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.SMLNUM ) THEN
C
C                       ABS(A(J,J)) > SMLNUM:
C
                     IF( TJJ.LT.ONE ) THEN
                        IF( XJ.GT.TJJ*BIGNUM ) THEN
C
C                             SCALE X BY 1/ABS(X(J)).
C
                           REC = ONE / XJ
                           CALL GLSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = GLADIV( X( J ), TJJS )
                  ELSE IF( TJJ.GT.ZERO ) THEN
C
C                       0 < ABS(A(J,J)) <= SMLNUM:
C
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
C
C                          SCALE X BY (1/ABS(X(J)))*ABS(A(J,J))*BIGNUM.
C
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL GLSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = GLADIV( X( J ), TJJS )
                  ELSE
C
C                       A(J,J) = 0:  SET X(1:N) = 0, X(J) = 1, AND
C                       SCALE = 0 AND COMPUTE A SOLUTION TO A**H *X = 0.
C
                     DO 200 I = 1, N
                        X( I ) = ZERO
  200                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  210             CONTINUE
               ELSE
C
C                 COMPUTE X(J) := X(J) / A(J,J) - CSUMJ IF THE DOT
C                 PRODUCT HAS ALREADY BEEN DIVIDED BY 1/A(J,J).
C
                  X( J ) = GLADIV( X( J ), TJJS ) - CSUMJ
               END IF
               XMAX = MAX( XMAX, CABS1( X( J ) ) )
  220       CONTINUE
         END IF
         SCALE = SCALE / TSCAL
      END IF
C
C     SCALE THE COLUMN NORMS BY 1/TSCAL FOR RETURN.
C
      IF( TSCAL.NE.ONE ) THEN
         CALL BLSCAL( N, ONE / TSCAL, CNORM, 1 )
      END IF
C
 1000 CONTINUE
C
C     END OF GLATRS
C
      END
