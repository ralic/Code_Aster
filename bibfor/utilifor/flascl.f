      SUBROUTINE FLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK MULTIPLIANT UNE MATRICE PAR UN REEL PROPREMENT.
C-----------------------------------------------------------------------
C
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C  PURPOSE
C  =======
C
C  FLASCL MULTIPLIES THE M BY N REAL MATRIX A BY THE REAL SCALAR
C  CTO/CFROM.  THIS IS DONE WITHOUT OVER/UNDERFLOW AS LONG AS THE FINAL
C  RESULT CTOCA(I,J)/CFROM DOES NOT OVER/UNDERFLOW. TYPE SPECIFIES THAT
C  A MAY BE FULL, UPPER TRIANGULAR, LOWER TRIANGULAR, UPPER HESSENBERG,
C  OR BANDED.
C
C  ARGUMENTS
C  =========
C
C  TYPE    (INPUT) CHARACTER*1
C          TYPE INDICES THE STORAGE TYPE OF THE INPUT MATRIX.
C          = 'G':  A IS A FULL MATRIX.
C          = 'L':  A IS A LOWER TRIANGULAR MATRIX.
C          = 'U':  A IS AN UPPER TRIANGULAR MATRIX.
C          = 'H':  A IS AN UPPER HESSENBERG MATRIX.
C          = 'B':  A IS A SYMMETRIC BAND MATRIX WITH LOWER BANDWIDTH KL
C                  AND UPPER BANDWIDTH KU AND WITH THE ONLY THE LOWER
C                  HALF STORED.
C          = 'Q':  A IS A SYMMETRIC BAND MATRIX WITH LOWER BANDWIDTH KL
C                  AND UPPER BANDWIDTH KU AND WITH THE ONLY THE UPPER
C                  HALF STORED.
C          = 'Z':  A IS A BAND MATRIX WITH LOWER BANDWIDTH KL AND UPPER
C                  BANDWIDTH KU.
C
C  KL      (INPUT) INTEGER
C          THE LOWER BANDWIDTH OF A.  REFERENCED ONLY IF TYPE = 'B',
C          'Q' OR 'Z'.
C
C  KU      (INPUT) INTEGER
C          THE UPPER BANDWIDTH OF A.  REFERENCED ONLY IF TYPE = 'B',
C          'Q' OR 'Z'.
C
C  CFROM   (INPUT) REAL*8
C  CTO     (INPUT) REAL*8
C          THE MATRIX A IS MULTIPLIED BY CTO/CFROM. A(I,J) IS COMPUTED
C          WITHOUT OVER/UNDERFLOW IF THE FINAL RESULT CTOCA(I,J)/CFROM
C          CAN BE REPRESENTED WITHOUT OVER/UNDERFLOW.  CFROM MUST BE
C          NONZERO.
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDA,M)
C          THE MATRIX TO BE MULTIPLIED BY CTO/CFROM.  SEE TYPE FOR THE
C          STORAGE TYPE.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
C
C  INFO    (OUTPUT) INTEGER
C          0  - SUCCESSFUL EXIT
C          <0 - IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE.
C
C-----------------------------------------------------------------------
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
C            REMPLACEMENT DE DLAMCH PAR R8MIEM,
C            REMPLACEMENT DE 2 RETURN PAR 2 GOTO 1000.
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      REAL*8   CFROM, CTO
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   A( LDA, * )
C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      REAL*8   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL  LLSAME
      REAL*8   R8MIEM
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT ARGUMENTS
C
      INFO = 0
C
      IF( LLSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LLSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LLSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LLSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LLSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LLSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LLSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      END IF
C
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     $         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     $            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     $             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     $            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     $            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'FLASCL', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   GOTO 1000
C
C     GET MACHINE PARAMETERS
C
      SMLNUM = R8MIEM()
      BIGNUM = ONE / SMLNUM
C
      CFROMC = CFROM
      CTOC = CTO
C
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      CTO1 = CTOC / BIGNUM
      IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
         MUL = SMLNUM
         DONE = .FALSE.
         CFROMC = CFROM1
      ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
         MUL = BIGNUM
         DONE = .FALSE.
         CTOC = CTO1
      ELSE
         MUL = CTOC / CFROMC
         DONE = .TRUE.
      END IF
C
      IF( ITYPE.EQ.0 ) THEN
C
C        FULL MATRIX
C
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
C
      ELSE IF( ITYPE.EQ.1 ) THEN
C
C        LOWER TRIANGULAR MATRIX
C
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
C
      ELSE IF( ITYPE.EQ.2 ) THEN
C
C        UPPER TRIANGULAR MATRIX
C
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
C
      ELSE IF( ITYPE.EQ.3 ) THEN
C
C        UPPER HESSENBERG MATRIX
C
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
C
      ELSE IF( ITYPE.EQ.4 ) THEN
C
C        LOWER HALF OF A SYMMETRIC BAND MATRIX
C
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
C
      ELSE IF( ITYPE.EQ.5 ) THEN
C
C        UPPER HALF OF A SYMMETRIC BAND MATRIX
C
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
C
      ELSE IF( ITYPE.EQ.6 ) THEN
C
C        BAND MATRIX
C
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
C
      END IF
C
      IF( .NOT.DONE )
     $   GO TO 10
 1000 CONTINUE
C
C
C     END OF FLASCL
      END
