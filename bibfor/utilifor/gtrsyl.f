      SUBROUTINE GTRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,
     $                   LDC, SCALE, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     MARCH 31, 1993
C
C
C  PURPOSE
C  =======
C
C  GTRSYL SOLVES THE COMPLEX SYLVESTER MATRIX EQUATION:
C
C     OP(A)*X + X*OP(B) = SCALE*C OR
C     OP(A)*X - X*OP(B) = SCALE*C,
C
C  WHERE OP(A) = A OR A**H, AND A AND B ARE BOTH UPPER TRIANGULAR. A IS
C  M-BY-M AND B IS N-BY-N; THE RIGHT HAND SIDE C AND THE SOLUTION X ARE
C  M-BY-N; AND SCALE IS AN OUTPUT SCALE FACTOR, SET <= 1 TO AVOID
C  OVERFLOW IN X.
C
C  ARGUMENTS
C  =========
C
C  TRANA   (INPUT) CHARACTER*1
C          SPECIFIES THE OPTION OP(A):
C          = 'N': OP(A) = A    (NO TRANSPOSE)
C          = 'C': OP(A) = A**H (CONJUGATE TRANSPOSE)
C
C  TRANB   (INPUT) CHARACTER*1
C          SPECIFIES THE OPTION OP(B):
C          = 'N': OP(B) = B    (NO TRANSPOSE)
C          = 'C': OP(B) = B**H (CONJUGATE TRANSPOSE)
C
C  ISGN    (INPUT) INTEGER
C          SPECIFIES THE SIGN IN THE EQUATION:
C          = +1: SOLVE OP(A)*X + X*OP(B) = SCALE*C
C          = -1: SOLVE OP(A)*X - X*OP(B) = SCALE*C
C
C  M       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX A, AND THE NUMBER OF ROWS IN THE
C          MATRICES X AND C. M >= 0.
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX B, AND THE NUMBER OF COLUMNS IN THE
C          MATRICES X AND C. N >= 0.
C
C  A       (INPUT) COMPLEX*16 ARRAY, DIMENSION (LDA,M)
C          THE UPPER TRIANGULAR MATRIX A.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A. LDA >= MAX(1,M).
C
C  B       (INPUT) COMPLEX*16 ARRAY, DIMENSION (LDB,N)
C          THE UPPER TRIANGULAR MATRIX B.
C
C  LDB     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY B. LDB >= MAX(1,N).
C
C  C       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDC,N)
C          ON ENTRY, THE M-BY-N RIGHT HAND SIDE MATRIX C.
C          ON EXIT, C IS OVERWRITTEN BY THE SOLUTION MATRIX X.
C
C  LDC     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY C. LDC >= MAX(1,M)
C
C  SCALE   (OUTPUT) DOUBLE PRECISION
C          THE SCALE FACTOR, SCALE, SET <= 1 TO AVOID OVERFLOW IN X.
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
C          = 1: A AND B HAVE COMMON OR VERY CLOSE EIGENVALUES; PERTURBED
C               VALUES WERE USED TO SOLVE THE EQUATION (BUT THE MATRICES
C               A AND B ARE UNCHANGED).
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
      CHARACTER*1        TRANA, TRANB
      INTEGER            INFO, ISGN, LDA, LDB, LDC, M, N
      REAL*8             SCALE
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * )
C     ..
C     .. PARAMETERS ..
      REAL*8             ONE
      PARAMETER          ( ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            NOTRNA, NOTRNB
      INTEGER            J, K, L
      REAL*8             BIGNUM, DA11, DB, EPS, SCALOC, SGN, SMIN,
     $                   SMLNUM,ULP
      COMPLEX*16         A11, SUML, SUMR, VEC, X11
C     ..
C     .. LOCAL ARRAYS ..
      REAL*8             DUM
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
      REAL*8             GLANGE, R8PREM, R8MIEM
      COMPLEX*16         GLDOTC, GLDOTU
      INTEGER            IDAMAX, ISBAEM
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     DECODE AND TEST INPUT PARAMETERS
C
      NOTRNA = LLSAME( TRANA, 'N' )
      NOTRNB = LLSAME( TRANB, 'N' )
C
      INFO = 0
      DUM = 0.0D0
      IF( .NOT.NOTRNA .AND. .NOT.LLSAME( TRANA, 'T' ) .AND. .NOT.
     $    LLSAME( TRANA, 'C' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRNB .AND. .NOT.LLSAME( TRANB, 'T' ) .AND. .NOT.
     $         LLSAME( TRANB, 'C' ) ) THEN
         INFO = -2
      ELSE IF( ISGN.NE.1 .AND. ISGN.NE.-1 ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'GTRSYL', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( M.EQ.0 .OR. N.EQ.0 )
     $    GOTO 1000
C
C     SET CONSTANTS TO CONTROL OVERFLOW
C
      EPS = R8PREM()
C DUE TO CRS512      OVFL = ONE / UNFL
      ULP = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = EPS*( N / ULP )
      BIGNUM = ( ONE-ULP ) / SMLNUM
      SMIN = MAX( SMLNUM, EPS*GLANGE( 'M', M, M, A, LDA, DUM ),
     $       EPS*GLANGE( 'M', N, N, B, LDB, DUM ) )
      SCALE = ONE
      SGN = ISGN
C
      IF( NOTRNA .AND. NOTRNB ) THEN
C
C        SOLVE    A*X + ISGN*X*B = SCALE*C.
C
C        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
C        BOTTOM-LEFT CORNER COLUMN BY COLUMN BY
C
C            A(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
C
C        WHERE
C                    M                        L-1
C          R(K,L) = SUM [A(K,I)*X(I,L)] +ISGN*SUM [X(K,J)*B(J,L)].
C                  I=K+1                      J=1
C
         DO 30 L = 1, N
            DO 20 K = M, 1, -1
C
               SUML = GLDOTU( M-K, A( K, MIN( K+1, M ) ), LDA,
     $                C( MIN( K+1, M ), L ), 1 )
               SUMR = GLDOTU( L-1, C( K, 1 ), LDC, B( 1, L ), 1 )
               VEC = C( K, L ) - ( SUML+SGN*SUMR )
C
               SCALOC = ONE
               A11 = A( K, K ) + SGN*B( L, L )
               DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) )
               IF( DA11.LE.SMIN ) THEN
                  A11 = SMIN
                  DA11 = SMIN
                  INFO = 1
               END IF
               DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) )
               IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                  IF( DB.GT.BIGNUM*DA11 )
     $               SCALOC = ONE / DB
               END IF
               X11 = ( VEC*DCMPLX( SCALOC ) ) / A11
C
               IF( SCALOC.NE.ONE ) THEN
                  DO 10 J = 1, N
                     CALL GLSCAL( M, SCALOC, C( 1, J ), 1 )
   10             CONTINUE
                  SCALE = SCALE*SCALOC
               END IF
               C( K, L ) = X11
C
   20       CONTINUE
   30    CONTINUE
C
      ELSE IF( .NOT.NOTRNA .AND. NOTRNB ) THEN
C
C        SOLVE    A' *X + ISGN*X*B = SCALE*C.
C
C        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
C        UPPER-LEFT CORNER COLUMN BY COLUMN BY
C
C            A'(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
C
C        WHERE
C                   K-1                         L-1
C          R(K,L) = SUM [A'(I,K)*X(I,L)] + ISGN*SUM [X(K,J)*B(J,L)]
C                   I=1                         J=1
C
         DO 60 L = 1, N
            DO 50 K = 1, M
C
               SUML = GLDOTC( K-1, A( 1, K ), 1, C( 1, L ), 1 )
               SUMR = GLDOTU( L-1, C( K, 1 ), LDC, B( 1, L ), 1 )
               VEC = C( K, L ) - ( SUML+SGN*SUMR )
C
               SCALOC = ONE
               A11 = DCONJG( A( K, K ) ) + SGN*B( L, L )
               DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) )
               IF( DA11.LE.SMIN ) THEN
                  A11 = SMIN
                  DA11 = SMIN
                  INFO = 1
               END IF
               DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) )
               IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                  IF( DB.GT.BIGNUM*DA11 )
     $               SCALOC = ONE / DB
               END IF
C
               X11 = ( VEC*DCMPLX( SCALOC ) ) / A11
C
               IF( SCALOC.NE.ONE ) THEN
                  DO 40 J = 1, N
                     CALL GLSCAL( M, SCALOC, C( 1, J ), 1 )
   40             CONTINUE
                  SCALE = SCALE*SCALOC
               END IF
               C( K, L ) = X11
C
   50       CONTINUE
   60    CONTINUE
C
      ELSE IF( .NOT.NOTRNA .AND. .NOT.NOTRNB ) THEN
C
C        SOLVE    A'*X + ISGN*X*B' = C.
C
C        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
C        UPPER-RIGHT CORNER COLUMN BY COLUMN BY
C
C            A'(K,K)*X(K,L) + ISGN*X(K,L)*B'(L,L) = C(K,L) - R(K,L)
C
C        WHERE
C                    K-1
C           R(K,L) = SUM [A'(I,K)*X(I,L)] +
C                    I=1
C                           N
C                     ISGN*SUM [X(K,J)*B'(L,J)].
C                          J=L+1
C
         DO 90 L = N, 1, -1
            DO 80 K = 1, M
C
               SUML = GLDOTC( K-1, A( 1, K ), 1, C( 1, L ), 1 )
               SUMR = GLDOTC( N-L, C( K, MIN( L+1, N ) ), LDC,
     $                B( L, MIN( L+1, N ) ), LDB )
               VEC = C( K, L ) - ( SUML+SGN*DCONJG( SUMR ) )
C
               SCALOC = ONE
               A11 = DCONJG( A( K, K )+SGN*B( L, L ) )
               DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) )
               IF( DA11.LE.SMIN ) THEN
                  A11 = SMIN
                  DA11 = SMIN
                  INFO = 1
               END IF
               DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) )
               IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                  IF( DB.GT.BIGNUM*DA11 )
     $               SCALOC = ONE / DB
               END IF
C
               X11 = ( VEC*DCMPLX( SCALOC ) ) / A11
C
               IF( SCALOC.NE.ONE ) THEN
                  DO 70 J = 1, N
                     CALL GLSCAL( M, SCALOC, C( 1, J ), 1 )
   70             CONTINUE
                  SCALE = SCALE*SCALOC
               END IF
               C( K, L ) = X11
C
   80       CONTINUE
   90    CONTINUE
C
      ELSE IF( NOTRNA .AND. .NOT.NOTRNB ) THEN
C
C        SOLVE    A*X + ISGN*X*B' = C.
C
C        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
C        BOTTOM-LEFT CORNER COLUMN BY COLUMN BY
C
C           A(K,K)*X(K,L) + ISGN*X(K,L)*B'(L,L) = C(K,L) - R(K,L)
C
C        WHERE
C                    M                          N
C          R(K,L) = SUM [A(K,I)*X(I,L)] + ISGN*SUM [X(K,J)*B'(L,J)]
C                  I=K+1                      J=L+1
C
         DO 120 L = N, 1, -1
            DO 110 K = M, 1, -1
C
               SUML = GLDOTU( M-K, A( K, MIN( K+1, M ) ), LDA,
     $                C( MIN( K+1, M ), L ), 1 )
               SUMR = GLDOTC( N-L, C( K, MIN( L+1, N ) ), LDC,
     $                B( L, MIN( L+1, N ) ), LDB )
               VEC = C( K, L ) - ( SUML+SGN*DCONJG( SUMR ) )
C
               SCALOC = ONE
               A11 = A( K, K ) + SGN*DCONJG( B( L, L ) )
               DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) )
               IF( DA11.LE.SMIN ) THEN
                  A11 = SMIN
                  DA11 = SMIN
                  INFO = 1
               END IF
               DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) )
               IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                  IF( DB.GT.BIGNUM*DA11 )
     $               SCALOC = ONE / DB
               END IF
C
               X11 = ( VEC*DCMPLX( SCALOC ) ) / A11
C
               IF( SCALOC.NE.ONE ) THEN
                  DO 100 J = 1, N
                     CALL GLSCAL( M, SCALOC, C( 1, J ), 1 )
  100             CONTINUE
                  SCALE = SCALE*SCALOC
               END IF
               C( K, L ) = X11
C
  110       CONTINUE
  120    CONTINUE
C
      END IF
C
 1000 CONTINUE
C
C     END OF GTRSYL
C
      END
