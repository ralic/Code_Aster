      SUBROUTINE FLAEXC( WANTQ, N, T, LDT, Q, LDQ, J1, N1, N2, WORK,
     &                   INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK PERMUTANT DEUX BLOCS DIAGONAUX D'UN MATRICE
C     TRIANGULAIRE SUPERIEURE.
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C  PURPOSE
C  =======
C
C  FLAEXC SWAPS ADJACENT DIAGONAL BLOCKS T11 AND T22 OF ORDER 1 OR 2 IN
C  AN UPPER QUASI-TRIANGULAR MATRIX T BY AN ORTHOGONAL SIMILARITY
C  TRANSFORMATION.
C
C  T MUST BE IN SCHUR CANONICAL FORM, THAT IS, BLOCK UPPER TRIANGULAR
C  WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS, EACH 2-BY-2 DIAGONAL BLOCK
C  HAS ITS DIAGONAL ELEMNTS EQUAL AND ITS OFF-DIAGONAL ELEMENTS OF
C  OPPOSITE SIGN.
C
C  ARGUMENTS
C  =========
C
C  WANTQ   (INPUT) LOGICAL
C          = .TRUE. : ACCUMULATE THE TRANSFORMATION IN THE MATRIX Q,
C          = .FALSE.: DO NOT ACCUMULATE THE TRANSFORMATION.
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX T. N >= 0.
C
C  T       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDT,N)
C          ON ENTRY, THE UPPER QUASI-TRIANGULAR MATRIX T, IN SCHUR
C          CANONICAL FORM.
C          ON EXIT, THE UPDATED MATRIX T, AGAIN IN SCHUR CANONICAL FORM.
C
C  LDT     (INPUT)  INTEGER
C          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
C
C  Q       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDQ,N)
C          ON ENTRY, IF WANTQ IS .TRUE., THE ORTHOGONAL MATRIX Q.
C          ON EXIT, IF WANTQ IS .TRUE., THE UPDATED MATRIX Q.
C          IF WANTQ IS .FALSE., Q IS NOT REFERENCED.
C
C  LDQ     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY Q.
C          LDQ >= 1, AND IF WANTQ IS .TRUE., LDQ >= N.
C
C  J1      (INPUT) INTEGER
C          THE INDEX OF THE FIRST ROW OF THE FIRST BLOCK T11.
C
C  N1      (INPUT) INTEGER
C          THE ORDER OF THE FIRST BLOCK T11. N1 = 0, 1 OR 2.
C
C  N2      (INPUT) INTEGER
C          THE ORDER OF THE SECOND BLOCK T22. N2 = 0, 1 OR 2.
C
C  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (N)
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          = 1: THE TRANSFORMED MATRIX T WOULD BE TOO FAR FROM SCHUR
C               FORM, THE BLOCKS ARE NOT SWAPPED AND T AND Q ARE
C               UNCHANGED.
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 3 RETURN PAR GOTO 1000,
C            REMPLACEMENT DE DLAMCH PAR R8PREM, R8MIEM ET ISBAEM,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            ABS, MAX.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      LOGICAL            WANTQ
      INTEGER            INFO, J1, LDQ, LDT, N, N1, N2
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   Q( LDQ, * ), T( LDT, * ), WORK( * )
C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      REAL*8   TEN
      PARAMETER          ( TEN = 1.0D+1 )
      INTEGER            LDD, LDX
      PARAMETER          ( LDD = 4, LDX = 2 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            IERR, J2, J3, J4, K, ND
      REAL*8   CS, DNORM, EPS, SCALE, SMLNUM, SN, T11, T22,
     &                   T33, TAU, TAU1, TAU2, TEMP, THRESH, WI1, WI2,
     &                   WR1, WR2, XNORM
C     ..
C     .. LOCAL ARRAYS ..
      REAL*8   D( LDD, 4 ), U( 3 ), U1( 3 ), U2( 3 ),
     &                   X( LDX, 2 )
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER ISBAEM
      REAL*8 FLANGE, R8MIEM, R8PREM
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      INFO = 0
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.EQ.0 .OR. N1.EQ.0 .OR. N2.EQ.0 )
     &   GOTO 1000
      IF( J1+N1.GT.N )
     &   GOTO 1000
C
      J2 = J1 + 1
      J3 = J1 + 2
      J4 = J1 + 3
C
      IF( N1.EQ.1 .AND. N2.EQ.1 ) THEN
C
C        SWAP TWO 1-BY-1 BLOCKS.
C
         T11 = T( J1, J1 )
         T22 = T( J2, J2 )
C
C        DETERMINE THE TRANSFORMATION TO PERFORM THE INTERCHANGE.
C
         CALL FLARTG( T( J1, J2 ), T22-T11, CS, SN, TEMP )
C
C        APPLY TRANSFORMATION TO THE MATRIX T.
C
         IF( J3.LE.N )
     &      CALL BLSROT( N-J1-1, T( J1, J3 ), LDT, T( J2, J3 ), LDT, CS,
     &                 SN )
         CALL BLSROT( J1-1, T( 1, J1 ), 1, T( 1, J2 ), 1, CS, SN )
C
         T( J1, J1 ) = T22
         T( J2, J2 ) = T11
C
         IF( WANTQ ) THEN
C
C           ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
C
            CALL BLSROT( N, Q( 1, J1 ), 1, Q( 1, J2 ), 1, CS, SN )
         END IF
C
      ELSE
C
C        SWAPPING INVOLVES AT LEAST ONE 2-BY-2 BLOCK.
C
C        COPY THE DIAGONAL BLOCK OF ORDER N1+N2 TO THE LOCAL ARRAY D
C        AND COMPUTE ITS NORM.
C
         ND = N1 + N2
C DUE TO CRP102 CALL FLACPY( 'FULL', ND, ND, T( J1, J1 ), LDT, D, LDD )
         CALL FLACPY( 'F', ND, ND, T( J1, J1 ), LDT, D, LDD )
C DUE TO CRP102 DNORM = FLANGE( 'MAX', ND, ND, D, LDD, WORK )
         DNORM = FLANGE( 'M', ND, ND, D, LDD, WORK )
C
C        COMPUTE MACHINE-DEPENDENT THRESHOLD FOR TEST FOR ACCEPTING
C        SWAP.
C
         EPS = R8PREM() * 0.5D0 * ISBAEM()
         SMLNUM = R8MIEM() / EPS
         THRESH = MAX( TEN*EPS*DNORM, SMLNUM )
C
C        SOLVE T11*X - X*T22 = SCALE*T12 FOR X.
C
         CALL FLASY2( .FALSE., .FALSE., -1, N1, N2, D, LDD,
     &                D( N1+1, N1+1 ), LDD, D( 1, N1+1 ), LDD, SCALE, X,
     &                LDX, XNORM, IERR )
C
C        SWAP THE ADJACENT DIAGONAL BLOCKS.
C
         K = N1 + N1 + N2 - 3
         GO TO ( 10, 20, 30 )K
C
   10    CONTINUE
C
C        N1 = 1, N2 = 2: GENERATE ELEMENTARY REFLECTOR H SO THAT:
C
C        ( SCALE, X11, X12 ) H = ( 0, 0, * )
C
         U( 1 ) = SCALE
         U( 2 ) = X( 1, 1 )
         U( 3 ) = X( 1, 2 )
         CALL FLARFG( 3, U( 3 ), U, 1, TAU )
         U( 3 ) = ONE
         T11 = T( J1, J1 )
C
C        PERFORM SWAP PROVISIONALLY ON DIAGONAL BLOCK IN D.
C
         CALL FLARFX( 'L', 3, 3, U, TAU, D, LDD, WORK )
         CALL FLARFX( 'R', 3, 3, U, TAU, D, LDD, WORK )
C
C        TEST WHETHER TO REJECT SWAP.
C
         IF( MAX( ABS( D( 3, 1 ) ), ABS( D( 3, 2 ) ), ABS( D( 3,
     &       3 )-T11 ) ).GT.THRESH )GO TO 50
C
C        ACCEPT SWAP: APPLY TRANSFORMATION TO THE ENTIRE MATRIX T.
C
         CALL FLARFX( 'L', 3, N-J1+1, U, TAU, T( J1, J1 ), LDT, WORK )
         CALL FLARFX( 'R', J2, 3, U, TAU, T( 1, J1 ), LDT, WORK )
C
         T( J3, J1 ) = ZERO
         T( J3, J2 ) = ZERO
         T( J3, J3 ) = T11
C
         IF( WANTQ ) THEN
C
C           ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
C
            CALL FLARFX( 'R', N, 3, U, TAU, Q( 1, J1 ), LDQ, WORK )
         END IF
         GO TO 40
C
   20    CONTINUE
C
C        N1 = 2, N2 = 1: GENERATE ELEMENTARY REFLECTOR H SO THAT:
C
C        H (  -X11 ) = ( * )
C          (  -X21 ) = ( 0 )
C          ( SCALE ) = ( 0 )
C
         U( 1 ) = -X( 1, 1 )
         U( 2 ) = -X( 2, 1 )
         U( 3 ) = SCALE
         CALL FLARFG( 3, U( 1 ), U( 2 ), 1, TAU )
         U( 1 ) = ONE
         T33 = T( J3, J3 )
C
C        PERFORM SWAP PROVISIONALLY ON DIAGONAL BLOCK IN D.
C
         CALL FLARFX( 'L', 3, 3, U, TAU, D, LDD, WORK )
         CALL FLARFX( 'R', 3, 3, U, TAU, D, LDD, WORK )
C
C        TEST WHETHER TO REJECT SWAP.
C
         IF( MAX( ABS( D( 2, 1 ) ), ABS( D( 3, 1 ) ), ABS( D( 1,
     &       1 )-T33 ) ).GT.THRESH )GO TO 50
C
C        ACCEPT SWAP: APPLY TRANSFORMATION TO THE ENTIRE MATRIX T.
C
         CALL FLARFX( 'R', J3, 3, U, TAU, T( 1, J1 ), LDT, WORK )
         CALL FLARFX( 'L', 3, N-J1, U, TAU, T( J1, J2 ), LDT, WORK )
C
         T( J1, J1 ) = T33
         T( J2, J1 ) = ZERO
         T( J3, J1 ) = ZERO
C
         IF( WANTQ ) THEN
C
C           ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
C
            CALL FLARFX( 'R', N, 3, U, TAU, Q( 1, J1 ), LDQ, WORK )
         END IF
         GO TO 40
C
   30    CONTINUE
C
C        N1 = 2, N2 = 2: GENERATE ELEMENTARY REFLECTORS H(1) AND H(2) SO
C        THAT:
C
C        H(2) H(1) (  -X11  -X12 ) = (  *  * )
C                  (  -X21  -X22 )   (  0  * )
C                  ( SCALE    0  )   (  0  0 )
C                  (    0  SCALE )   (  0  0 )
C
         U1( 1 ) = -X( 1, 1 )
         U1( 2 ) = -X( 2, 1 )
         U1( 3 ) = SCALE
         CALL FLARFG( 3, U1( 1 ), U1( 2 ), 1, TAU1 )
         U1( 1 ) = ONE
C
         TEMP = -TAU1*( X( 1, 2 )+U1( 2 )*X( 2, 2 ) )
         U2( 1 ) = -TEMP*U1( 2 ) - X( 2, 2 )
         U2( 2 ) = -TEMP*U1( 3 )
         U2( 3 ) = SCALE
         CALL FLARFG( 3, U2( 1 ), U2( 2 ), 1, TAU2 )
         U2( 1 ) = ONE
C
C        PERFORM SWAP PROVISIONALLY ON DIAGONAL BLOCK IN D.
C
         CALL FLARFX( 'L', 3, 4, U1, TAU1, D, LDD, WORK )
         CALL FLARFX( 'R', 4, 3, U1, TAU1, D, LDD, WORK )
         CALL FLARFX( 'L', 3, 4, U2, TAU2, D( 2, 1 ), LDD, WORK )
         CALL FLARFX( 'R', 4, 3, U2, TAU2, D( 1, 2 ), LDD, WORK )
C
C        TEST WHETHER TO REJECT SWAP.
C
         IF( MAX( ABS( D( 3, 1 ) ), ABS( D( 3, 2 ) ), ABS( D( 4, 1 ) ),
     &       ABS( D( 4, 2 ) ) ).GT.THRESH )GO TO 50
C
C        ACCEPT SWAP: APPLY TRANSFORMATION TO THE ENTIRE MATRIX T.
C
         CALL FLARFX( 'L', 3, N-J1+1, U1, TAU1, T( J1, J1 ), LDT, WORK )
         CALL FLARFX( 'R', J4, 3, U1, TAU1, T( 1, J1 ), LDT, WORK )
         CALL FLARFX( 'L', 3, N-J1+1, U2, TAU2, T( J2, J1 ), LDT, WORK )
         CALL FLARFX( 'R', J4, 3, U2, TAU2, T( 1, J2 ), LDT, WORK )
C
         T( J3, J1 ) = ZERO
         T( J3, J2 ) = ZERO
         T( J4, J1 ) = ZERO
         T( J4, J2 ) = ZERO
C
         IF( WANTQ ) THEN
C
C           ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
C
            CALL FLARFX( 'R', N, 3, U1, TAU1, Q( 1, J1 ), LDQ, WORK )
            CALL FLARFX( 'R', N, 3, U2, TAU2, Q( 1, J2 ), LDQ, WORK )
         END IF
C
   40    CONTINUE
C
         IF( N2.EQ.2 ) THEN
C
C           STANDARDIZE NEW 2-BY-2 BLOCK T11
C
            CALL FLANV2( T( J1, J1 ), T( J1, J2 ), T( J2, J1 ),
     &                   T( J2, J2 ), WR1, WI1, WR2, WI2, CS, SN )
            CALL BLSROT( N-J1-1, T( J1, J1+2 ), LDT, T( J2, J1+2 ), LDT,
     &                 CS, SN )
            CALL BLSROT( J1-1, T( 1, J1 ), 1, T( 1, J2 ), 1, CS, SN )
            IF( WANTQ )
     &         CALL BLSROT( N, Q( 1, J1 ), 1, Q( 1, J2 ), 1, CS, SN )
         END IF
C
         IF( N1.EQ.2 ) THEN
C
C           STANDARDIZE NEW 2-BY-2 BLOCK T22
C
            J3 = J1 + N2
            J4 = J3 + 1
            CALL FLANV2( T( J3, J3 ), T( J3, J4 ), T( J4, J3 ),
     &                   T( J4, J4 ), WR1, WI1, WR2, WI2, CS, SN )
            IF( J3+2.LE.N )
     &         CALL BLSROT( N-J3-1, T( J3, J3+2 ), LDT, T( J4, J3+2 ),
     &                    LDT, CS, SN )
            CALL BLSROT( J3-1, T( 1, J3 ), 1, T( 1, J4 ), 1, CS, SN )
            IF( WANTQ )
     &         CALL BLSROT( N, Q( 1, J3 ), 1, Q( 1, J4 ), 1, CS, SN )
         END IF
C
      END IF
      GOTO 1000
C
C     EXIT WITH INFO = 1 IF SWAP WAS REJECTED.
C
   50 CONTINUE
      INFO = 1
 1000 CONTINUE
C
C     END OF FLAEXC
C
      END
