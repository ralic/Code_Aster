      SUBROUTINE FGEQR2( M, N, A, LDA, TAU, WORK, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK CALCULANT UNE FACTORISATION QR.
C-----------------------------------------------------------------------
C  -- LAPACK ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C  PURPOSE
C  =======
C
C  FGEQR2 COMPUTES A QR FACTORIZATION OF A REAL M BY N MATRIX A:
C  A = Q * R.
C
C  ARGUMENTS
C  =========
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDA,N)
C          ON ENTRY, THE M BY N MATRIX A.
C          ON EXIT, THE ELEMENTS ON AND ABOVE THE DIAGONAL OF THE ARRAY
C          CONTAIN THE MIN(M,N) BY N UPPER TRAPEZOIDAL MATRIX R (R IS
C          UPPER TRIANGULAR IF M >= N), THE ELEMENTS BELOW THE DIAGONAL,
C          WITH THE ARRAY TAU, REPRESENT THE ORTHOGONAL MATRIX Q AS A
C          PRODUCT OF ELEMENTARY REFLECTORS (SEE FURTHER DETAILS).
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
C
C  TAU     (OUTPUT) REAL*8 ARRAY, DIMENSION (MIN(M,N))
C          THE SCALAR FACTORS OF THE ELEMENTARY REFLECTORS (SEE FURTHER
C          DETAILS).
C
C  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (N)
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
C
C  FURTHER DETAILS
C  ===============
C
C  THE MATRIX Q IS REPRESENTED AS A PRODUCT OF ELEMENTARY REFLECTORS
C
C     Q = H(1) H(2) . . . H(K), WHERE K = MIN(M,N).
C
C  EACH H(I) HAS THE FORM
C
C     H(I) = I - TAU * V * V'
C
C  WHERE TAU IS A REAL SCALAR, AND V IS A REAL VECTOR WITH
C  V(1:I-1) = 0 AND V(I) = 1, V(I+1:M) IS STORED ON EXIT IN A(I+1:M,I),
C  AND TAU IN TAU(I).
C
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            MAX, MIN.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      INTEGER            INFO, LDA, M, N
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   A( LDA, * ), TAU( * ), WORK( * )
C     ..
C     .. PARAMETERS ..
      REAL*8   ONE
      PARAMETER          ( ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, K
      REAL*8   AII
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT ARGUMENTS
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'FGEQR2', -INFO )
         GOTO 1000
      END IF
C
      K = MIN( M, N )
C
      DO 10 I = 1, K
C
C        GENERATE ELEMENTARY REFLECTOR H(I) TO ANNIHILATE A(I+1:M,I)
C
         CALL FLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     &                TAU( I ) )
         IF( I.LT.N ) THEN
C
C           APPLY H(I) TO A(I:M,I+1:N) FROM THE LEFT
C
            AII = A( I, I )
            A( I, I ) = ONE
            CALL FLARF( 'L', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     &                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
 1000 CONTINUE
C
C     END OF FGEQR2
C
      END
