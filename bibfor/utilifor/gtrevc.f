      SUBROUTINE GTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
     $                   LDVR, MM, M, WORK, RWORK, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C  PURPOSE
C  =======
C
C  GTREVC COMPUTES SOME OR ALL OF THE RIGHT AND/OR LEFT EIGENVECTORS OF
C  A COMPLEX UPPER TRIANGULAR MATRIX T.
C
C  THE RIGHT EIGENVECTOR X AND THE LEFT EIGENVECTOR Y OF T CORRESPONDING
C  TO AN EIGENVALUE W ARE DEFINED BY:
C
C               T*X = W*X,     Y'*T = W*Y'
C
C  WHERE Y' DENOTES THE CONJUGATE TRANSPOSE OF THE VECTOR Y.
C
C  IF ALL EIGENVECTORS ARE REQUESTED, THE ROUTINE MAY EITHER RETURN THE
C  MATRICES X AND/OR Y OF RIGHT OR LEFT EIGENVECTORS OF T, OR THE
C  PRODUCTS Q*X AND/OR Q*Y, WHERE Q IS AN INPUT UNITARY
C  MATRIX. IF T WAS OBTAINED FROM THE SCHUR FACTORIZATION OF AN
C  ORIGINAL MATRIX A = Q*T*Q', THEN Q*X AND Q*Y ARE THE MATRICES OF
C  RIGHT OR LEFT EIGENVECTORS OF A.
C
C  ARGUMENTS
C  =========
C
C  SIDE    (INPUT) CHARACTER*1
C          = 'R':  COMPUTE RIGHT EIGENVECTORS ONLY;
C          = 'L':  COMPUTE LEFT EIGENVECTORS ONLY;
C          = 'B':  COMPUTE BOTH RIGHT AND LEFT EIGENVECTORS.
C
C  HOWMNY  (INPUT) CHARACTER*1
C          = 'A':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS;
C          = 'B':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS,
C                  AND BACKTRANSFORM THEM USING THE INPUT MATRICES
C                  SUPPLIED IN VR AND/OR VL;
C          = 'S':  COMPUTE SELECTED RIGHT AND/OR LEFT EIGENVECTORS,
C                  SPECIFIED BY THE LOGICAL ARRAY SELECT.
C
C  SELECT  (INPUT) LOGICAL ARRAY, DIMENSION (N)
C          IF HOWMNY = 'S', SELECT SPECIFIES THE EIGENVECTORS TO BE
C          COMPUTED.
C          IF HOWMNY = 'A' OR 'B', SELECT IS NOT REFERENCED.
C          TO SELECT THE EIGENVECTOR CORRESPONDING TO THE J-TH
C          EIGENVALUE, SELECT(J) MUST BE SET TO .TRUE..
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX T. N >= 0.
C
C  T       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDT,N)
C          THE UPPER TRIANGULAR MATRIX T.  T IS MODIFIED, BUT RESTORED
C          ON EXIT.
C
C  LDT     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
C
C  VL      (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDVL,MM)
C          ON ENTRY, IF SIDE = 'L' OR 'B' AND HOWMNY = 'B', VL MUST
C          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE UNITARY MATRIX Q OF
C          SCHUR VECTORS RETURNED BY ZHSEQR).
C          ON EXIT, IF SIDE = 'L' OR 'B', VL CONTAINS:
C          IF HOWMNY = 'A', THE MATRIX Y OF LEFT EIGENVECTORS OF T;
C          IF HOWMNY = 'B', THE MATRIX Q*Y;
C          IF HOWMNY = 'S', THE LEFT EIGENVECTORS OF T SPECIFIED BY
C                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
C                           OF VL, IN THE SAME ORDER AS THEIR
C                           EIGENVALUES.
C          IF SIDE = 'R', VL IS NOT REFERENCED.
C
C  LDVL    (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY VL.  LDVL >= MAX(1,N) IF
C          SIDE = 'L' OR 'B'; LDVL >= 1 OTHERWISE.
C
C  VR      (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDVR,MM)
C          ON ENTRY, IF SIDE = 'R' OR 'B' AND HOWMNY = 'B', VR MUST
C          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE UNITARY MATRIX Q OF
C          SCHUR VECTORS RETURNED BY ZHSEQR).
C          ON EXIT, IF SIDE = 'R' OR 'B', VR CONTAINS:
C          IF HOWMNY = 'A', THE MATRIX X OF RIGHT EIGENVECTORS OF T;
C          IF HOWMNY = 'B', THE MATRIX Q*X;
C          IF HOWMNY = 'S', THE RIGHT EIGENVECTORS OF T SPECIFIED BY
C                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
C                           OF VR, IN THE SAME ORDER AS THEIR
C                           EIGENVALUES.
C          IF SIDE = 'L', VR IS NOT REFERENCED.
C
C  LDVR    (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY VR.  LDVR >= MAX(1,N) IF
C           SIDE = 'R' OR 'B'; LDVR >= 1 OTHERWISE.
C
C  MM      (INPUT) INTEGER
C          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR. MM >= M.
C
C  M       (OUTPUT) INTEGER
C          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR ACTUALLY
C          USED TO STORE THE EIGENVECTORS.  IF HOWMNY = 'A' OR 'B', M
C          IS SET TO N.  EACH SELECTED EIGENVECTOR OCCUPIES ONE
C          COLUMN.
C
C  WORK    (WORKSPACE) COMPLEX*16 ARRAY, DIMENSION (2*N)
C
C  RWORK   (WORKSPACE) DOUBLE PRECISION ARRAY, DIMENSION (N)
C
C  INFO    (OUTPUT) INTEGER
C          = 0:  SUCCESSFUL EXIT
C          < 0:  IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
C
C  FURTHER DETAILS
C  ===============
C
C  THE ALGORITHM USED IN THIS PROGRAM IS BASICALLY BACKWARD (FORWARD)
C  SUBSTITUTION, WITH SCALING TO MAKE THE THE CODE ROBUST AGAINST
C  POSSIBLE OVERFLOW.
C
C  EACH EIGENVECTOR IS NORMALIZED SO THAT THE ELEMENT OF LARGEST
C  MAGNITUDE HAS MAGNITUDE 1; HERE THE MAGNITUDE OF A COMPLEX NUMBER
C  (X,Y) IS TAKEN TO BE |X| + |Y|.
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
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        HOWMNY, SIDE
      INTEGER            INFO, LDT, LDVL, LDVR, M, MM, N
C     ..
C     .. ARRAY ARGUMENTS ..
      LOGICAL            SELECT( * )
      REAL*8             RWORK( * )
      COMPLEX*16         T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WORK( * )
C     ..
C     .. PARAMETERS ..
      REAL*8             ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CMZERO, CMONE
      PARAMETER          ( CMZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CMONE = ( 1.0D+0, 0.0D+0 ) )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            ALLV, BOTHV, LEFTV, OVER, RIGHTV, SOMEV
      INTEGER            I, II, IS, J, K, KI
      REAL*8             OVFL, REMAX, SCALE, SMIN, SMLNUM, ULP, UNFL
      COMPLEX*16         CDUM
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
      INTEGER            IZAMAX, ISBAEM
      REAL*8             DZASUM, R8PREM, R8MIEM
C     ..
C     .. STATEMENT FUNCTIONS ..
      REAL*8             CABS1
C     ..
C     .. STATEMENT FUNCTION DEFINITIONS ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     DECODE AND TEST THE INPUT PARAMETERS
C
      BOTHV = LLSAME( SIDE, 'B' )
      RIGHTV = LLSAME( SIDE, 'R' ) .OR. BOTHV
      LEFTV = LLSAME( SIDE, 'L' ) .OR. BOTHV
C
      ALLV = LLSAME( HOWMNY, 'A' )
      OVER = LLSAME( HOWMNY, 'B' ) .OR. LLSAME( HOWMNY, 'O' )
      SOMEV = LLSAME( HOWMNY, 'S' )
C
C     SET M TO THE NUMBER OF COLUMNS REQUIRED TO STORE THE SELECTED
C     EIGENVECTORS.
C
      IF( SOMEV ) THEN
         M = 0
         DO 10 J = 1, N
            IF( SELECT( J ) )
     $         M = M + 1
   10    CONTINUE
      ELSE
         M = N
      END IF
C
      INFO = 0
      IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN
         INFO = -1
      ELSE IF( .NOT.ALLV .AND. .NOT.OVER .AND. .NOT.SOMEV ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDT.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDVL.LT.1 .OR. ( LEFTV .AND. LDVL.LT.N ) ) THEN
         INFO = -8
      ELSE IF( LDVR.LT.1 .OR. ( RIGHTV .AND. LDVR.LT.N ) ) THEN
         INFO = -10
      ELSE IF( MM.LT.M ) THEN
         INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'GTREVC', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( N.EQ.0 )
     $    GOTO 1000
C
C     SET THE CONSTANTS TO CONTROL OVERFLOW.
C
      UNFL = R8MIEM()
C DUE TO CRS512      OVFL = ONE / UNFL
      ULP = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = UNFL*( N / ULP )
C
C     STORE THE DIAGONAL ELEMENTS OF T IN WORKING ARRAY WORK.
C
      DO 20 I = 1, N
         WORK( I+N ) = T( I, I )
   20 CONTINUE
C
C     COMPUTE 1-NORM OF EACH COLUMN OF STRICTLY UPPER TRIANGULAR
C     PART OF T TO CONTROL OVERFLOW IN TRIANGULAR SOLVER.
C
      RWORK( 1 ) = ZERO
      DO 30 J = 2, N
         RWORK( J ) = DZASUM( J-1, T( 1, J ), 1 )
   30 CONTINUE
C
      IF( RIGHTV ) THEN
C
C        COMPUTE RIGHT EIGENVECTORS.
C
         IS = M
         DO 80 KI = N, 1, -1
C
            IF( SOMEV ) THEN
               IF( .NOT.SELECT( KI ) )
     $            GO TO 80
            END IF
            SMIN = MAX( ULP*( CABS1( T( KI, KI ) ) ), SMLNUM )
C
            WORK( 1 ) = CMONE
C
C           FORM RIGHT-HAND SIDE.
C
            DO 40 K = 1, KI - 1
               WORK( K ) = -T( K, KI )
   40       CONTINUE
C
C           SOLVE THE TRIANGULAR SYSTEM:
C              (T(1:KI-1,1:KI-1) - T(KI,KI))*X = SCALE*WORK.
C
            DO 50 K = 1, KI - 1
               T( K, K ) = T( K, K ) - T( KI, KI )
               IF( CABS1( T( K, K ) ).LT.SMIN )
     $            T( K, K ) = SMIN
   50       CONTINUE
C
            IF( KI.GT.1 ) THEN
               CALL ZLATRS( 'U', 'N', 'N', 'Y',
     $                      KI-1, T, LDT, WORK( 1 ), SCALE, RWORK,
     $                      INFO )
               WORK( KI ) = SCALE
            END IF
C
C           COPY THE VECTOR X OR Q*X TO VR AND NORMALIZE.
C
            IF( .NOT.OVER ) THEN
               CALL ZCOPY( KI, WORK( 1 ), 1, VR( 1, IS ), 1 )
C
               II = IZAMAX( KI, VR( 1, IS ), 1 )
               REMAX = ONE / CABS1( VR( II, IS ) )
               CALL ZDSCAL( KI, REMAX, VR( 1, IS ), 1 )
C
               DO 60 K = KI + 1, N
                  VR( K, IS ) = CMZERO
   60          CONTINUE
            ELSE
               IF( KI.GT.1 )
     $            CALL ZGEMV( 'N', N, KI-1, CMONE, VR, LDVR, WORK( 1 ),
     $                        1, DCMPLX( SCALE ), VR( 1, KI ), 1 )
C
               II = IZAMAX( N, VR( 1, KI ), 1 )
               REMAX = ONE / CABS1( VR( II, KI ) )
               CALL ZDSCAL( N, REMAX, VR( 1, KI ), 1 )
            END IF
C
C           SET BACK THE ORIGINAL DIAGONAL ELEMENTS OF T.
C
            DO 70 K = 1, KI - 1
               T( K, K ) = WORK( K+N )
   70       CONTINUE
C
            IS = IS - 1
   80    CONTINUE
      END IF
C
      IF( LEFTV ) THEN
C
C        COMPUTE LEFT EIGENVECTORS.
C
         IS = 1
         DO 130 KI = 1, N
C
            IF( SOMEV ) THEN
               IF( .NOT.SELECT( KI ) )
     $            GO TO 130
            END IF
            SMIN = MAX( ULP*( CABS1( T( KI, KI ) ) ), SMLNUM )
C
            WORK( N ) = CMONE
C
C           FORM RIGHT-HAND SIDE.
C
            DO 90 K = KI + 1, N
               WORK( K ) = -DCONJG( T( KI, K ) )
   90       CONTINUE
C
C           SOLVE THE TRIANGULAR SYSTEM:
C              (T(KI+1:N,KI+1:N) - T(KI,KI))'*X = SCALE*WORK.
C
            DO 100 K = KI + 1, N
               T( K, K ) = T( K, K ) - T( KI, KI )
               IF( CABS1( T( K, K ) ).LT.SMIN )
     $            T( K, K ) = SMIN
  100       CONTINUE
C
            IF( KI.LT.N ) THEN
               CALL ZLATRS( 'U', 'C', 'N',
     $                      'Y', N-KI, T( KI+1, KI+1 ), LDT,
     $                      WORK( KI+1 ), SCALE, RWORK, INFO )
               WORK( KI ) = SCALE
            END IF
C
C           COPY THE VECTOR X OR Q*X TO VL AND NORMALIZE.
C
            IF( .NOT.OVER ) THEN
               CALL ZCOPY( N-KI+1, WORK( KI ), 1, VL( KI, IS ), 1 )
C
               II = IZAMAX( N-KI+1, VL( KI, IS ), 1 ) + KI - 1
               REMAX = ONE / CABS1( VL( II, IS ) )
               CALL ZDSCAL( N-KI+1, REMAX, VL( KI, IS ), 1 )
C
               DO 110 K = 1, KI - 1
                  VL( K, IS ) = CMZERO
  110          CONTINUE
            ELSE
               IF( KI.LT.N )
     $            CALL ZGEMV( 'N', N, N-KI, CMONE, VL( 1, KI+1 ), LDVL,
     $                        WORK( KI+1 ), 1, DCMPLX( SCALE ),
     $                        VL( 1, KI ), 1 )
C
               II = IZAMAX( N, VL( 1, KI ), 1 )
               REMAX = ONE / CABS1( VL( II, KI ) )
               CALL ZDSCAL( N, REMAX, VL( 1, KI ), 1 )
            END IF
C
C           SET BACK THE ORIGINAL DIAGONAL ELEMENTS OF T.
C
            DO 120 K = KI + 1, N
               T( K, K ) = WORK( K+N )
  120       CONTINUE
C
            IS = IS + 1
  130    CONTINUE
      END IF
C
 1000 CONTINUE
C
C     END OF GTREVC
C
      END
