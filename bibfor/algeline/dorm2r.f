      SUBROUTINE DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     &                   WORK, INFO )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C 
C     SUBROUTINE LAPACK EFFECTUANT DIFFERENTS TYPES DE MULTIPLICATION
C     ENTRE UNE MATRICE QUELCONQUE ET UNE MATRICE ORTHOGONALE S'ECRIVANT
C     COMME LE PRODUIT DE MATRICES DE PROJECTION ELEMENTAIRES.
C-----------------------------------------------------------------------
C  -- LAPACK ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C  PURPOSE
C  =======
C
C  DORM2R OVERWRITES THE GENERAL REAL M BY N MATRIX C WITH
C
C        Q * C  IF SIDE = 'L' AND TRANS = 'N', OR
C
C        Q'* C  IF SIDE = 'L' AND TRANS = 'T', OR
C
C        C * Q  IF SIDE = 'R' AND TRANS = 'N', OR
C
C        C * Q' IF SIDE = 'R' AND TRANS = 'T',
C
C  WHERE Q IS A REAL ORTHOGONAL MATRIX DEFINED AS THE PRODUCT OF K
C  ELEMENTARY REFLECTORS
C
C        Q = H(1) H(2) . . . H(K)
C
C  AS RETURNED BY DGEQRF. Q IS OF ORDER M IF SIDE = 'L' AND OF ORDER N
C  IF SIDE = 'R'.
C
C  ARGUMENTS
C  =========
C
C  SIDE    (INPUT) CHARACTER*1
C          = 'L': APPLY Q OR Q' FROM THE LEFT
C          = 'R': APPLY Q OR Q' FROM THE RIGHT
C
C  TRANS   (INPUT) CHARACTER*1
C          = 'N': APPLY Q  (NO TRANSPOSE)
C          = 'T': APPLY Q' (TRANSPOSE)
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX C. M >= 0.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX C. N >= 0.
C
C  K       (INPUT) INTEGER
C          THE NUMBER OF ELEMENTARY REFLECTORS WHOSE PRODUCT DEFINES
C          THE MATRIX Q.
C          IF SIDE = 'L', M >= K >= 0,
C          IF SIDE = 'R', N >= K >= 0.
C
C  A       (INPUT) REAL*8 ARRAY, DIMENSION (LDA,K)
C          THE I-TH COLUMN MUST CONTAIN THE VECTOR WHICH DEFINES THE
C          ELEMENTARY REFLECTOR H(I), FOR I = 1,2,...,K, AS RETURNED BY
C          DGEQRF IN THE FIRST K COLUMNS OF ITS ARRAY ARGUMENT A.
C          A IS MODIFIED BY THE ROUTINE BUT RESTORED ON EXIT.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.
C          IF SIDE = 'L', LDA >= MAX(1,M),
C          IF SIDE = 'R', LDA >= MAX(1,N).
C
C  TAU     (INPUT) REAL*8 ARRAY, DIMENSION (K)
C          TAU(I) MUST CONTAIN THE SCALAR FACTOR OF THE ELEMENTARY
C          REFLECTOR H(I), AS RETURNED BY DGEQRF.
C
C  C       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDC,N)
C          ON ENTRY, THE M BY N MATRIX C.
C          ON EXIT, C IS OVERWRITTEN BY Q*C OR Q'*C OR C*Q' OR C*Q.
C
C  LDC     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY C. LDC >= MAX(1,M).
C
C  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION
C                                   (N) IF SIDE = 'L',
C                                   (M) IF SIDE = 'R'
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C   MAX
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
      
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
C     ..
C     .. PARAMETERS ..
      REAL*8   ONE
      PARAMETER          ( ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      REAL*8   AII
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT ARGUMENTS
C
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
C
C     NQ IS THE ORDER OF Q
C
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORM2R', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     &   GOTO 1000
C
      IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. ( .NOT.LEFT .AND. NOTRAN ) )
     &     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
C
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
C
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
C
C           H(I) IS APPLIED TO C(I:M,1:N)
C
            MI = M - I + 1
            IC = I
         ELSE
C
C           H(I) IS APPLIED TO C(1:M,I:N)
C
            NI = N - I + 1
            JC = I
         END IF
C
C        APPLY H(I)
C
         AII = A( I, I )
         A( I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ),
     &               LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
 1000 CONTINUE
C
C     END OF DORM2R
C
      END
