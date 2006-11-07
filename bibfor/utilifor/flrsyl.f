      SUBROUTINE FLRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,
     &                   LDC, SCALE, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 06/11/2006   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C
C     SUBROUTINE LAPACK RESOLVANT L'EQUATION DE SYLVESTER.
C-----------------------------------------------------------------------
C  -- LAPACK ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     MARCH 31, 1993
C
C  PURPOSE
C  =======
C
C  FLRSYL SOLVES THE REAL SYLVESTER MATRIX EQUATION:
C
C     OP(A)*X + X*OP(B) = SCALE*C OR
C     OP(A)*X - X*OP(B) = SCALE*C,
C
C  WHERE OP(A) = A OR A**T, AND  A AND B ARE BOTH UPPER QUASI-
C  TRIANGULAR. A IS M-BY-M AND B IS N-BY-N, THE RIGHT HAND SIDE C AND
C  THE SOLUTION X ARE M-BY-N, AND SCALE IS AN OUTPUT SCALE FACTOR, SET
C  <= 1 TO AVOID OVERFLOW IN X.
C
C  A AND B MUST BE IN SCHUR CANONICAL FORM (AS RETURNED BY DHSEQR), THAT
C  IS, BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS,
C  EACH 2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
C  OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.
C
C  ARGUMENTS
C  =========
C
C  TRANA   (INPUT) CHARACTER*1
C          SPECIFIES THE OPTION OP(A):
C          = 'N': OP(A) = A    (NO TRANSPOSE)
C          = 'T': OP(A) = A**T (TRANSPOSE)
C          = 'C': OP(A) = A**H (CONJUGATE TRANSPOSE = TRANSPOSE)
C
C  TRANB   (INPUT) CHARACTER*1
C          SPECIFIES THE OPTION OP(B):
C          = 'N': OP(B) = B    (NO TRANSPOSE)
C          = 'T': OP(B) = B**T (TRANSPOSE)
C          = 'C': OP(B) = B**H (CONJUGATE TRANSPOSE = TRANSPOSE)
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
C  A       (INPUT) REAL*8 ARRAY, DIMENSION (LDA,M)
C          THE UPPER QUASI-TRIANGULAR MATRIX A, IN SCHUR CANONICAL FORM.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A. LDA >= MAX(1,M).
C
C  B       (INPUT) REAL*8 ARRAY, DIMENSION (LDB,N)
C          THE UPPER QUASI-TRIANGULAR MATRIX B, IN SCHUR CANONICAL FORM.
C
C  LDB     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY B. LDB >= MAX(1,N).
C
C  C       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDC,N)
C          ON ENTRY, THE M-BY-N RIGHT HAND SIDE MATRIX C.
C          ON EXIT, C IS OVERWRITTEN BY THE SOLUTION MATRIX X.
C
C  LDC     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY C. LDC >= MAX(1,M)
C
C  SCALE   (OUTPUT) REAL*8
C          THE SCALE FACTOR, SCALE, SET <= 1 TO AVOID OVERFLOW IN X.
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
C          = 1: A AND B HAVE COMMON OR VERY CLOSE EIGENVALUES, PERTURBED
C               VALUES WERE USED TO SOLVE THE EQUATION (BUT THE MATRICES
C               A AND B ARE UNCHANGED).
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C         REMPLACEMENT DE DLAMCH ET DLABAD PAR R8PREM,R8MIEM ET ISBAEM,
C            REMPLACEMENT DE 2 RETURN PAR UN GOTO 1000,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C INTRINSIC FUNCTION
C   MAX, MIN, DBLE, ABS
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        TRANA, TRANB
      INTEGER            INFO, ISGN, LDA, LDB, LDC, M, N
      REAL*8   SCALE
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   A( LDA, * ), B( LDB, * ), C( LDC, * )
C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            NOTRNA, NOTRNB
      INTEGER            IERR, J, K, K1, K2, KNEXT, L, L1, L2, LNEXT
      REAL*8   A11, BIGNUM, DA11, DB, EPS, SCALOC, SGN, SMIN,
     &                   SMLNUM, SUML, SUMR, XNORM
C     ..
C     .. LOCAL ARRAYS ..
      REAL*8   DUM( 1 ), VEC( 2, 2 ), X( 2, 2 )
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL LSAME
      INTEGER ISBAEM
      REAL*8 DDOT, DLANGE, R8MIEM, R8PREM
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      CALL MATFPE(-1)
C
C DUE TO CRS513
      DUM(1) = 0.D0

C     DECODE AND TEST INPUT PARAMETERS
C
      NOTRNA = LSAME( TRANA, 'N' )
      NOTRNB = LSAME( TRANB, 'N' )
C
      INFO = 0
      IF( .NOT.NOTRNA .AND. .NOT.LSAME( TRANA, 'T' ) .AND. .NOT.
     &    LSAME( TRANA, 'C' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRNB .AND. .NOT.LSAME( TRANB, 'T' ) .AND. .NOT.
     &         LSAME( TRANB, 'C' ) ) THEN
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
         CALL XERBLA( 'FLRSYL', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( M.EQ.0 .OR. N.EQ.0 )
     &   GOTO 1000
C
C     SET CONSTANTS TO CONTROL OVERFLOW
C
      EPS = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = R8MIEM()
      BIGNUM = ONE / SMLNUM
      SMLNUM = SMLNUM*DBLE( M*N ) / EPS
      BIGNUM = ONE / SMLNUM
C
      SMIN = MAX( SMLNUM, EPS*DLANGE( 'M', M, M, A, LDA, DUM ),
     &       EPS*DLANGE( 'M', N, N, B, LDB, DUM ) )
C
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
C         A(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
C
C        WHERE
C                  M                         L-1
C        R(K,L) = SUM (A(K,I)*X(I,L)) + ISGN*SUM (X(K,J)*B(J,L)).
C                I=K+1                       J=1
C
C        START COLUMN LOOP (INDEX = L)
C        L1 (L2) : COLUMN INDEX OF THE FIRST (FIRST) ROW OF X(K,L).
C
         LNEXT = 1
         DO 60 L = 1, N
            IF( L.LT.LNEXT )
     &         GO TO 60
            IF( L.EQ.N ) THEN
               L1 = L
               L2 = L
            ELSE
               IF( B( L+1, L ).NE.ZERO ) THEN
                  L1 = L
                  L2 = L + 1
                  LNEXT = L + 2
               ELSE
                  L1 = L
                  L2 = L
                  LNEXT = L + 1
               END IF
            END IF
C
C           START ROW LOOP (INDEX = K)
C           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L).
C
            KNEXT = M
            DO 50 K = M, 1, -1
               IF( K.GT.KNEXT )
     &            GO TO 50
               IF( K.EQ.1 ) THEN
                  K1 = K
                  K2 = K
               ELSE
                  IF( A( K, K-1 ).NE.ZERO ) THEN
                     K1 = K - 1
                     K2 = K
                     KNEXT = K - 2
                  ELSE
                     K1 = K
                     K2 = K
                     KNEXT = K - 1
                  END IF
               END IF
C
               IF( L1.EQ.L2 .AND. K1.EQ.K2 ) THEN
                  SUML = DDOT( M-K1, A( K1, MIN( K1+1, M ) ), LDA,
     &                   C( MIN( K1+1, M ), L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
                  SCALOC = ONE
C
                  A11 = A( K1, K1 ) + SGN*B( L1, L1 )
                  DA11 = ABS( A11 )
                  IF( DA11.LE.SMIN ) THEN
                     A11 = SMIN
                     DA11 = SMIN
                     INFO = 1
                  END IF
                  DB = ABS( VEC( 1, 1 ) )
                  IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                     IF( DB.GT.BIGNUM*DA11 )
     &                  SCALOC = ONE / DB
                  END IF
                  X( 1, 1 ) = ( VEC( 1, 1 )*SCALOC ) / A11
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 10 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
   10                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
C
               ELSE IF( L1.EQ.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( M-K2, A( K1, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K2, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K2, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  CALL FLALN2( .FALSE., 2, 1, SMIN, ONE, A( K1, K1 ),
     &                         LDA, ONE, ONE, VEC, 2, -SGN*B( L1, L1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 20 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
   20                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K2, L1 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.EQ.K2 ) THEN
C
                  SUML = DDOT( M-K1, A( K1, MIN( K1+1, M ) ), LDA,
     &                   C( MIN( K1+1, M ), L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = SGN*( C( K1, L1 )-( SUML+SGN*SUMR ) )
C
                  SUML = DDOT( M-K1, A( K1, MIN( K1+1, M ) ), LDA,
     &                   C( MIN( K1+1, M ), L2 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L2 ), 1 )
                  VEC( 2, 1 ) = SGN*( C( K1, L2 )-( SUML+SGN*SUMR ) )
C
                  CALL FLALN2( .TRUE., 2, 1, SMIN, ONE, B( L1, L1 ),
     &                         LDB, ONE, ONE, VEC, 2, -SGN*A( K1, K1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 30 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
   30                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( M-K2, A( K1, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K1, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L2 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L2 ), 1 )
                  VEC( 1, 2 ) = C( K1, L2 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K2, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K2, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K2, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L2 ), 1 )
                  SUMR = DDOT( L1-1, C( K2, 1 ), LDC, B( 1, L2 ), 1 )
                  VEC( 2, 2 ) = C( K2, L2 ) - ( SUML+SGN*SUMR )
C
                  CALL FLASY2( .FALSE., .FALSE., ISGN, 2, 2,
     &                         A( K1, K1 ), LDA, B( L1, L1 ), LDB, VEC,
     &                         2, SCALOC, X, 2, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 40 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
   40                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 1, 2 )
                  C( K2, L1 ) = X( 2, 1 )
                  C( K2, L2 ) = X( 2, 2 )
               END IF
C
   50       CONTINUE
C
   60    CONTINUE
C
      ELSE IF( .NOT.NOTRNA .AND. NOTRNB ) THEN
C
C        SOLVE    A' *X + ISGN*X*B = SCALE*C.
C
C        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
C        UPPER-LEFT CORNER COLUMN BY COLUMN BY
C
C          A(K,K)'*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
C
C        WHERE
C                   K-1                        L-1
C          R(K,L) = SUM (A(I,K)'*X(I,L)) +ISGN*SUM (X(K,J)*B(J,L))
C                   I=1                        J=1
C
C        START COLUMN LOOP (INDEX = L)
C        L1 (L2): COLUMN INDEX OF THE FIRST (LAST) ROW OF X(K,L)
C
         LNEXT = 1
         DO 120 L = 1, N
            IF( L.LT.LNEXT )
     &         GO TO 120
            IF( L.EQ.N ) THEN
               L1 = L
               L2 = L
            ELSE
               IF( B( L+1, L ).NE.ZERO ) THEN
                  L1 = L
                  L2 = L + 1
                  LNEXT = L + 2
               ELSE
                  L1 = L
                  L2 = L
                  LNEXT = L + 1
               END IF
            END IF
C
C           START ROW LOOP (INDEX = K)
C           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L)
C
            KNEXT = 1
            DO 110 K = 1, M
               IF( K.LT.KNEXT )
     &            GO TO 110
               IF( K.EQ.M ) THEN
                  K1 = K
                  K2 = K
               ELSE
                  IF( A( K+1, K ).NE.ZERO ) THEN
                     K1 = K
                     K2 = K + 1
                     KNEXT = K + 2
                  ELSE
                     K1 = K
                     K2 = K
                     KNEXT = K + 1
                  END IF
               END IF
C
               IF( L1.EQ.L2 .AND. K1.EQ.K2 ) THEN
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
                  SCALOC = ONE
C
                  A11 = A( K1, K1 ) + SGN*B( L1, L1 )
                  DA11 = ABS( A11 )
                  IF( DA11.LE.SMIN ) THEN
                     A11 = SMIN
                     DA11 = SMIN
                     INFO = 1
                  END IF
                  DB = ABS( VEC( 1, 1 ) )
                  IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                     IF( DB.GT.BIGNUM*DA11 )
     &                  SCALOC = ONE / DB
                  END IF
                  X( 1, 1 ) = ( VEC( 1, 1 )*SCALOC ) / A11
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 70 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
   70                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
C
               ELSE IF( L1.EQ.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K2 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K2, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  CALL FLALN2( .TRUE., 2, 1, SMIN, ONE, A( K1, K1 ),
     &                         LDA, ONE, ONE, VEC, 2, -SGN*B( L1, L1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 80 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
   80                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K2, L1 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.EQ.K2 ) THEN
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = SGN*( C( K1, L1 )-( SUML+SGN*SUMR ) )
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L2 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L2 ), 1 )
                  VEC( 2, 1 ) = SGN*( C( K1, L2 )-( SUML+SGN*SUMR ) )
C
                  CALL FLALN2( .TRUE., 2, 1, SMIN, ONE, B( L1, L1 ),
     &                         LDB, ONE, ONE, VEC, 2, -SGN*A( K1, K1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 90 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
   90                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L2 ), 1 )
                  SUMR = DDOT( L1-1, C( K1, 1 ), LDC, B( 1, L2 ), 1 )
                  VEC( 1, 2 ) = C( K1, L2 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K2 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( L1-1, C( K2, 1 ), LDC, B( 1, L1 ), 1 )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K2 ), 1, C( 1, L2 ), 1 )
                  SUMR = DDOT( L1-1, C( K2, 1 ), LDC, B( 1, L2 ), 1 )
                  VEC( 2, 2 ) = C( K2, L2 ) - ( SUML+SGN*SUMR )
C
                  CALL FLASY2( .TRUE., .FALSE., ISGN, 2, 2, A( K1, K1 ),
     &                         LDA, B( L1, L1 ), LDB, VEC, 2, SCALOC, X,
     &                         2, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 100 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  100                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 1, 2 )
                  C( K2, L1 ) = X( 2, 1 )
                  C( K2, L2 ) = X( 2, 2 )
               END IF
C
  110       CONTINUE
  120    CONTINUE
C
      ELSE IF( .NOT.NOTRNA .AND. .NOT.NOTRNB ) THEN
C
C        SOLVE    A'*X + ISGN*X*B' = SCALE*C.
C
C        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
C        TOP-RIGHT CORNER COLUMN BY COLUMN BY
C
C           A(K,K)'*X(K,L) + ISGN*X(K,L)*B(L,L)' = C(K,L) - R(K,L)
C
C        WHERE
C                     K-1                          N
C            R(K,L) = SUM (A(I,K)'*X(I,L)) + ISGN*SUM (X(K,J)*B(L,J)').
C                     I=1                        J=L+1
C
C        START COLUMN LOOP (INDEX = L)
C        L1 (L2): COLUMN INDEX OF THE FIRST (LAST) ROW OF X(K,L)
C
         LNEXT = N
         DO 180 L = N, 1, -1
            IF( L.GT.LNEXT )
     &         GO TO 180
            IF( L.EQ.1 ) THEN
               L1 = L
               L2 = L
            ELSE
               IF( B( L, L-1 ).NE.ZERO ) THEN
                  L1 = L - 1
                  L2 = L
                  LNEXT = L - 2
               ELSE
                  L1 = L
                  L2 = L
                  LNEXT = L - 1
               END IF
            END IF
C
C           START ROW LOOP (INDEX = K)
C           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L)
C
            KNEXT = 1
            DO 170 K = 1, M
               IF( K.LT.KNEXT )
     &            GO TO 170
               IF( K.EQ.M ) THEN
                  K1 = K
                  K2 = K
               ELSE
                  IF( A( K+1, K ).NE.ZERO ) THEN
                     K1 = K
                     K2 = K + 1
                     KNEXT = K + 2
                  ELSE
                     K1 = K
                     K2 = K
                     KNEXT = K + 1
                  END IF
               END IF
C
               IF( L1.EQ.L2 .AND. K1.EQ.K2 ) THEN
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( N-L1, C( K1, MIN( L1+1, N ) ), LDC,
     &                   B( L1, MIN( L1+1, N ) ), LDB )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
                  SCALOC = ONE
C
                  A11 = A( K1, K1 ) + SGN*B( L1, L1 )
                  DA11 = ABS( A11 )
                  IF( DA11.LE.SMIN ) THEN
                     A11 = SMIN
                     DA11 = SMIN
                     INFO = 1
                  END IF
                  DB = ABS( VEC( 1, 1 ) )
                  IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                     IF( DB.GT.BIGNUM*DA11 )
     &                  SCALOC = ONE / DB
                  END IF
                  X( 1, 1 ) = ( VEC( 1, 1 )*SCALOC ) / A11
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 130 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  130                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
C
               ELSE IF( L1.EQ.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K2 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K2, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  CALL FLALN2( .TRUE., 2, 1, SMIN, ONE, A( K1, K1 ),
     &                         LDA, ONE, ONE, VEC, 2, -SGN*B( L1, L1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 140 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  140                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K2, L1 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.EQ.K2 ) THEN
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 1 ) = SGN*( C( K1, L1 )-( SUML+SGN*SUMR ) )
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L2 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L2, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 1 ) = SGN*( C( K1, L2 )-( SUML+SGN*SUMR ) )
C
                  CALL FLALN2( .FALSE., 2, 1, SMIN, ONE, B( L1, L1 ),
     &                         LDB, ONE, ONE, VEC, 2, -SGN*A( K1, K1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 150 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  150                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K1 ), 1, C( 1, L2 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L2, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 2 ) = C( K1, L2 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K2 ), 1, C( 1, L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K2, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( K1-1, A( 1, K2 ), 1, C( 1, L2 ), 1 )
                  SUMR = DDOT( N-L2, C( K2, MIN( L2+1, N ) ), LDC,
     &                   B( L2, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 2 ) = C( K2, L2 ) - ( SUML+SGN*SUMR )
C
                  CALL FLASY2( .TRUE., .TRUE., ISGN, 2, 2, A( K1, K1 ),
     &                         LDA, B( L1, L1 ), LDB, VEC, 2, SCALOC, X,
     &                         2, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 160 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  160                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 1, 2 )
                  C( K2, L1 ) = X( 2, 1 )
                  C( K2, L2 ) = X( 2, 2 )
               END IF
C
  170       CONTINUE
  180    CONTINUE
C
      ELSE IF( NOTRNA .AND. .NOT.NOTRNB ) THEN
C
C        SOLVE    A*X + ISGN*X*B' = SCALE*C.
C
C        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
C        BOTTOM-RIGHT CORNER COLUMN BY COLUMN BY
C
C            A(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L)' = C(K,L) - R(K,L)
C
C        WHERE
C                      M                          N
C            R(K,L) = SUM (A(K,I)*X(I,L)) + ISGN*SUM (X(K,J)*B(L,J)').
C                    I=K+1                      J=L+1
C
C        START COLUMN LOOP (INDEX = L)
C        L1 (L2): COLUMN INDEX OF THE FIRST (LAST) ROW OF X(K,L)
C
         LNEXT = N
         DO 240 L = N, 1, -1
            IF( L.GT.LNEXT )
     &         GO TO 240
            IF( L.EQ.1 ) THEN
               L1 = L
               L2 = L
            ELSE
               IF( B( L, L-1 ).NE.ZERO ) THEN
                  L1 = L - 1
                  L2 = L
                  LNEXT = L - 2
               ELSE
                  L1 = L
                  L2 = L
                  LNEXT = L - 1
               END IF
            END IF
C
C           START ROW LOOP (INDEX = K)
C           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L)
C
            KNEXT = M
            DO 230 K = M, 1, -1
               IF( K.GT.KNEXT )
     &            GO TO 230
               IF( K.EQ.1 ) THEN
                  K1 = K
                  K2 = K
               ELSE
                  IF( A( K, K-1 ).NE.ZERO ) THEN
                     K1 = K - 1
                     K2 = K
                     KNEXT = K - 2
                  ELSE
                     K1 = K
                     K2 = K
                     KNEXT = K - 1
                  END IF
               END IF
C
               IF( L1.EQ.L2 .AND. K1.EQ.K2 ) THEN
                  SUML = DDOT( M-K1, A( K1, MIN( K1+1, M ) ), LDA,
     &                   C( MIN( K1+1, M ), L1 ), 1 )
                  SUMR = DDOT( N-L1, C( K1, MIN( L1+1, N ) ), LDC,
     &                   B( L1, MIN( L1+1, N ) ), LDB )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
                  SCALOC = ONE
C
                  A11 = A( K1, K1 ) + SGN*B( L1, L1 )
                  DA11 = ABS( A11 )
                  IF( DA11.LE.SMIN ) THEN
                     A11 = SMIN
                     DA11 = SMIN
                     INFO = 1
                  END IF
                  DB = ABS( VEC( 1, 1 ) )
                  IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN
                     IF( DB.GT.BIGNUM*DA11 )
     &                  SCALOC = ONE / DB
                  END IF
                  X( 1, 1 ) = ( VEC( 1, 1 )*SCALOC ) / A11
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 190 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  190                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
C
               ELSE IF( L1.EQ.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( M-K2, A( K1, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K2, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K2, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  CALL FLALN2( .FALSE., 2, 1, SMIN, ONE, A( K1, K1 ),
     &                         LDA, ONE, ONE, VEC, 2, -SGN*B( L1, L1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 200 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  200                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K2, L1 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.EQ.K2 ) THEN
C
                  SUML = DDOT( M-K1, A( K1, MIN( K1+1, M ) ), LDA,
     &                   C( MIN( K1+1, M ), L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 1 ) = SGN*( C( K1, L1 )-( SUML+SGN*SUMR ) )
C
                  SUML = DDOT( M-K1, A( K1, MIN( K1+1, M ) ), LDA,
     &                   C( MIN( K1+1, M ), L2 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L2, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 1 ) = SGN*( C( K1, L2 )-( SUML+SGN*SUMR ) )
C
                  CALL FLALN2( .FALSE., 2, 1, SMIN, ONE, B( L1, L1 ),
     &                         LDB, ONE, ONE, VEC, 2, -SGN*A( K1, K1 ),
     &                         ZERO, X, 2, SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 210 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  210                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 2, 1 )
C
               ELSE IF( L1.NE.L2 .AND. K1.NE.K2 ) THEN
C
                  SUML = DDOT( M-K2, A( K1, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 1 ) = C( K1, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K1, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L2 ), 1 )
                  SUMR = DDOT( N-L2, C( K1, MIN( L2+1, N ) ), LDC,
     &                   B( L2, MIN( L2+1, N ) ), LDB )
                  VEC( 1, 2 ) = C( K1, L2 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K2, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L1 ), 1 )
                  SUMR = DDOT( N-L2, C( K2, MIN( L2+1, N ) ), LDC,
     &                   B( L1, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 1 ) = C( K2, L1 ) - ( SUML+SGN*SUMR )
C
                  SUML = DDOT( M-K2, A( K2, MIN( K2+1, M ) ), LDA,
     &                   C( MIN( K2+1, M ), L2 ), 1 )
                  SUMR = DDOT( N-L2, C( K2, MIN( L2+1, N ) ), LDC,
     &                   B( L2, MIN( L2+1, N ) ), LDB )
                  VEC( 2, 2 ) = C( K2, L2 ) - ( SUML+SGN*SUMR )
C
                  CALL FLASY2( .FALSE., .TRUE., ISGN, 2, 2, A( K1, K1 ),
     &                         LDA, B( L1, L1 ), LDB, VEC, 2, SCALOC, X,
     &                         2, XNORM, IERR )
                  IF( IERR.NE.0 )
     &               INFO = 1
C
                  IF( SCALOC.NE.ONE ) THEN
                     DO 220 J = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, J ), 1 )
  220                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
                  C( K1, L1 ) = X( 1, 1 )
                  C( K1, L2 ) = X( 1, 2 )
                  C( K2, L1 ) = X( 2, 1 )
                  C( K2, L2 ) = X( 2, 2 )
               END IF
C
  230       CONTINUE
  240    CONTINUE
C
      END IF
C
 1000 CONTINUE
      CALL MATFPE(1)
C
C     END OF FLRSYL
C
      END
