      SUBROUTINE FTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
     &                   LDVR, MM, M, WORK, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C
C     SUBROUTINE LAPACK CALCULANT DES VECTEUR PROPRES.
C-----------------------------------------------------------------------
C  -- LAPACK ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C  PURPOSE
C  =======
C
C  FTREVC COMPUTES SOME OR ALL OF THE RIGHT AND/OR LEFT EIGENVECTORS OF
C  A REAL UPPER QUASI-TRIANGULAR MATRIX T.
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
C  PRODUCTS Q*X AND/OR Q*Y, WHERE Q IS AN INPUT ORTHOGONAL
C  MATRIX. IF T WAS OBTAINED FROM THE REAL-SCHUR FACTORIZATION OF AN
C  ORIGINAL MATRIX A = Q*T*Q', THEN Q*X AND Q*Y ARE THE MATRICES OF
C  RIGHT OR LEFT EIGENVECTORS OF A.
C
C  T MUST BE IN SCHUR CANONICAL FORM (AS RETURNED BY DHSEQR), THAT IS,
C  BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS, EACH
C  2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
C  OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.  CORRESPONDING TO EACH 2-BY-2
C  DIAGONAL BLOCK IS A COMPLEX CONJUGATE PAIR OF EIGENVALUES AND
C  EIGENVECTORS, ONLY ONE EIGENVECTOR OF THE PAIR IS COMPUTED, NAMELY
C  THE ONE CORRESPONDING TO THE EIGENVALUE WITH POSITIVE IMAGINARY PART.
C
C
C  ARGUMENTS
C  =========
C
C  SIDE    (INPUT) CHARACTER*1
C          = 'R':  COMPUTE RIGHT EIGENVECTORS ONLY,
C          = 'L':  COMPUTE LEFT EIGENVECTORS ONLY,
C          = 'B':  COMPUTE BOTH RIGHT AND LEFT EIGENVECTORS.
C
C  HOWMNY  (INPUT) CHARACTER*1
C          = 'A':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS,
C          = 'B':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS,
C                  AND BACKTRANSFORM THEM USING THE INPUT MATRICES
C                  SUPPLIED IN VR AND/OR VL,
C          = 'S':  COMPUTE SELECTED RIGHT AND/OR LEFT EIGENVECTORS,
C                  SPECIFIED BY THE LOGICAL ARRAY SELECT.
C
C  SELECT  (INPUT/OUTPUT) LOGICAL ARRAY, DIMENSION (N)
C          IF HOWMNY = 'S', SELECT SPECIFIES THE EIGENVECTORS TO BE
C          COMPUTED.
C          IF HOWMNY = 'A' OR 'B', SELECT IS NOT REFERENCED.
C          TO SELECT THE REAL EIGENVECTOR CORRESPONDING TO A REAL
C          EIGENVALUE W(J), SELECT(J) MUST BE SET TO .TRUE..  TO SELECT
C          THE COMPLEX EIGENVECTOR CORRESPONDING TO A COMPLEX CONJUGATE
C          PAIR W(J) AND W(J+1), EITHER SELECT(J) OR SELECT(J+1) MUST BE
C          SET TO .TRUE., THEN ON EXIT SELECT(J) IS .TRUE. AND
C          SELECT(J+1) IS .FALSE..
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX T. N >= 0.
C
C  T       (INPUT) REAL*8 ARRAY, DIMENSION (LDT,N)
C          THE UPPER QUASI-TRIANGULAR MATRIX T IN SCHUR CANONICAL FORM.
C
C  LDT     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
C
C  VL      (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDVL,MM)
C          ON ENTRY, IF SIDE = 'L' OR 'B' AND HOWMNY = 'B', VL MUST
C          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE ORTHOGONAL MATRIX Q
C          OF SCHUR VECTORS RETURNED BY DHSEQR).
C          ON EXIT, IF SIDE = 'L' OR 'B', VL CONTAINS:
C          IF HOWMNY = 'A', THE MATRIX Y OF LEFT EIGENVECTORS OF T,
C          IF HOWMNY = 'B', THE MATRIX Q*Y,
C          IF HOWMNY = 'S', THE LEFT EIGENVECTORS OF T SPECIFIED BY
C                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
C                           OF VL, IN THE SAME ORDER AS THEIR
C                           EIGENVALUES.
C          A COMPLEX EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE
C          IS STORED IN TWO CONSECUTIVE COLUMNS, THE FIRST HOLDING THE
C          REAL PART, AND THE SECOND THE IMAGINARY PART.
C          IF SIDE = 'R', VL IS NOT REFERENCED.
C
C  LDVL    (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY VL.  LDVL >= MAX(1,N) IF
C          SIDE = 'L' OR 'B', LDVL >= 1 OTHERWISE.
C
C  VR      (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDVR,MM)
C          ON ENTRY, IF SIDE = 'R' OR 'B' AND HOWMNY = 'B', VR MUST
C          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE ORTHOGONAL MATRIX Q
C          OF SCHUR VECTORS RETURNED BY DHSEQR).
C          ON EXIT, IF SIDE = 'R' OR 'B', VR CONTAINS:
C          IF HOWMNY = 'A', THE MATRIX X OF RIGHT EIGENVECTORS OF T,
C          IF HOWMNY = 'B', THE MATRIX Q*X,
C          IF HOWMNY = 'S', THE RIGHT EIGENVECTORS OF T SPECIFIED BY
C                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
C                           OF VR, IN THE SAME ORDER AS THEIR
C                           EIGENVALUES.
C          A COMPLEX EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE
C          IS STORED IN TWO CONSECUTIVE COLUMNS, THE FIRST HOLDING THE
C          REAL PART AND THE SECOND THE IMAGINARY PART.
C          IF SIDE = 'L', VR IS NOT REFERENCED.
C
C  LDVR    (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY VR.  LDVR >= MAX(1,N) IF
C          SIDE = 'R' OR 'B', LDVR >= 1 OTHERWISE.
C
C  MM      (INPUT) INTEGER
C          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR. MM >= M.
C
C  M       (OUTPUT) INTEGER
C          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR ACTUALLY
C          USED TO STORE THE EIGENVECTORS.
C          IF HOWMNY = 'A' OR 'B', M IS SET TO N.
C          EACH SELECTED REAL EIGENVECTOR OCCUPIES ONE COLUMN AND EACH
C          SELECTED COMPLEX EIGENVECTOR OCCUPIES TWO COLUMNS.
C
C  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (3*N)
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
C  MAGNITUDE HAS MAGNITUDE 1, HERE THE MAGNITUDE OF A COMPLEX NUMBER
C  (X,Y) IS TAKEN TO BE |X| + |Y|.
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE DLAMCH ET DLABAD,
C            UTILISATION DE R8PREM(), R8MIEM() ET ISBAEM(),
C            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            ABS, MAX, SQRT.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        HOWMNY, SIDE
      INTEGER            INFO, LDT, LDVL, LDVR, M, MM, N
C     ..
C     .. ARRAY ARGUMENTS ..
      LOGICAL            SELECT( * )
      REAL*8   T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
     &                   WORK( * )
C     .. PARAMETERS ..
      REAL*8   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            ALLV, BOTHV, LEFTV, OVER, PAIR, RIGHTV, SOMEV
      INTEGER            I, IERR, II, IP, IS, J, J1, J2, JNXT, K, KI, N2
      REAL*8   BETA, BIGNUM, EMAX, REC, REMAX, SCALE,
     &                   SMIN, SMLNUM, ULP, UNFL, VCRIT, VMAX, WI, WR,
     &                   XNORM
C DUE TO CRS512       REAL*8 OVFL
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
      INTEGER            IDAMAX, ISBAEM
      REAL*8   BLSDOT, R8PREM, R8MIEM
C     ..
C     .. LOCAL ARRAYS ..
      REAL*8   X( 2, 2 )
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
      ELSE
C
C        SET M TO THE NUMBER OF COLUMNS REQUIRED TO STORE THE SELECTED
C        EIGENVECTORS, STANDARDIZE THE ARRAY SELECT IF NECESSARY, AND
C        TEST MM.
C
         IF( SOMEV ) THEN
            M = 0
            PAIR = .FALSE.
            DO 10 J = 1, N
               IF( PAIR ) THEN
                  PAIR = .FALSE.
                  SELECT( J ) = .FALSE.
               ELSE
                  IF( J.LT.N ) THEN
                     IF( T( J+1, J ).EQ.ZERO ) THEN
                        IF( SELECT( J ) )
     &                     M = M + 1
                     ELSE
                        PAIR = .TRUE.
                        IF( SELECT( J ) .OR. SELECT( J+1 ) ) THEN
                           SELECT( J ) = .TRUE.
                           M = M + 2
                        END IF
                     END IF
                  ELSE
                     IF( SELECT( N ) )
     &                  M = M + 1
                  END IF
               END IF
   10       CONTINUE
         ELSE
            M = N
         END IF
C
         IF( MM.LT.M ) THEN
            INFO = -11
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'FTREVC', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( N.EQ.0 )
     &   GOTO 1000
C
C     SET THE CONSTANTS TO CONTROL OVERFLOW.
C
      UNFL = R8MIEM()
C DUE TO CRS512      OVFL = ONE / UNFL
      ULP = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = UNFL*( N / ULP )
      BIGNUM = ( ONE-ULP ) / SMLNUM
C
C     COMPUTE 1-NORM OF EACH COLUMN OF STRICTLY UPPER TRIANGULAR
C     PART OF T TO CONTROL OVERFLOW IN TRIANGULAR SOLVER.
C
      WORK( 1 ) = ZERO
      DO 30 J = 2, N
         WORK( J ) = ZERO
         DO 20 I = 1, J - 1
            WORK( J ) = WORK( J ) + ABS( T( I, J ) )
   20    CONTINUE
   30 CONTINUE
C
C     INDEX IP IS USED TO SPECIFY THE REAL OR COMPLEX EIGENVALUE:
C       IP = 0, REAL EIGENVALUE,
C            1, FIRST OF CONJUGATE COMPLEX PAIR: (WR,WI)
C           -1, SECOND OF CONJUGATE COMPLEX PAIR: (WR,WI)
C
      N2 = 2*N
C
      IF( RIGHTV ) THEN
C
C        COMPUTE RIGHT EIGENVECTORS.
C
         IP = 0
         IS = M
         DO 140 KI = N, 1, -1
C
            IF( IP.EQ.1 )
     &         GO TO 130
            IF( KI.EQ.1 )
     &         GO TO 40
            IF( T( KI, KI-1 ).EQ.ZERO )
     &         GO TO 40
            IP = -1
C
   40       CONTINUE
            IF( SOMEV ) THEN
               IF( IP.EQ.0 ) THEN
                  IF( .NOT.SELECT( KI ) )
     &               GO TO 130
               ELSE
                  IF( .NOT.SELECT( KI-1 ) )
     &               GO TO 130
               END IF
            END IF
C
C           COMPUTE THE KI-TH EIGENVALUE (WR,WI).
C
            WR = T( KI, KI )
            WI = ZERO
            IF( IP.NE.0 )
     &         WI = SQRT( ABS( T( KI, KI-1 ) ) )*
     &              SQRT( ABS( T( KI-1, KI ) ) )
            SMIN = MAX( ULP*( ABS( WR )+ABS( WI ) ), SMLNUM )
C
            IF( IP.EQ.0 ) THEN
C
C              REAL RIGHT EIGENVECTOR
C
               WORK( KI+N ) = ONE
C
C              FORM RIGHT-HAND SIDE
C
               DO 50 K = 1, KI - 1
                  WORK( K+N ) = -T( K, KI )
   50          CONTINUE
C
C              SOLVE THE UPPER QUASI-TRIANGULAR SYSTEM:
C                 (T(1:KI-1,1:KI-1) - WR)*X = SCALE*WORK.
C
               JNXT = KI - 1
               DO 60 J = KI - 1, 1, -1
                  IF( J.GT.JNXT )
     &               GO TO 60
                  J1 = J
                  J2 = J
                  JNXT = J - 1
                  IF( J.GT.1 ) THEN
                     IF( T( J, J-1 ).NE.ZERO ) THEN
                        J1 = J - 1
                        JNXT = J - 2
                     END IF
                  END IF
C
                  IF( J1.EQ.J2 ) THEN
C
C                    1-BY-1 DIAGONAL BLOCK
C
                     CALL FLALN2( .FALSE., 1, 1, SMIN, ONE, T( J, J ),
     &                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     &                            ZERO, X, 2, SCALE, XNORM, IERR )
C
C                    SCALE X(1,1) TO AVOID OVERFLOW WHEN UPDATING
C                    THE RIGHT-HAND SIDE.
C
                     IF( XNORM.GT.ONE ) THEN
                        IF( WORK( J ).GT.BIGNUM / XNORM ) THEN
                           X( 1, 1 ) = X( 1, 1 ) / XNORM
                           SCALE = SCALE / XNORM
                        END IF
                     END IF
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE )
     &                  CALL BLSCAL( KI, SCALE, WORK( 1+N ), 1 )
                     WORK( J+N ) = X( 1, 1 )
C
C                    UPDATE RIGHT-HAND SIDE
C
                     CALL BLAXPY( J-1, -X( 1, 1 ), T( 1, J ), 1,
     &                           WORK( 1+N ), 1 )
C
                  ELSE
C
C                    2-BY-2 DIAGONAL BLOCK
C
                     CALL FLALN2( .FALSE., 2, 1, SMIN, ONE,
     &                            T( J-1, J-1 ), LDT, ONE, ONE,
     &                            WORK( J-1+N ), N, WR, ZERO, X, 2,
     &                            SCALE, XNORM, IERR )
C
C                    SCALE X(1,1) AND X(2,1) TO AVOID OVERFLOW WHEN
C                    UPDATING THE RIGHT-HAND SIDE.
C
                     IF( XNORM.GT.ONE ) THEN
                        BETA = MAX( WORK( J-1 ), WORK( J ) )
                        IF( BETA.GT.BIGNUM / XNORM ) THEN
                           X( 1, 1 ) = X( 1, 1 ) / XNORM
                           X( 2, 1 ) = X( 2, 1 ) / XNORM
                           SCALE = SCALE / XNORM
                        END IF
                     END IF
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE )
     &                  CALL BLSCAL( KI, SCALE, WORK( 1+N ), 1 )
                     WORK( J-1+N ) = X( 1, 1 )
                     WORK( J+N ) = X( 2, 1 )
C
C                    UPDATE RIGHT-HAND SIDE
C
                     CALL BLAXPY( J-2, -X( 1, 1 ), T( 1, J-1 ), 1,
     &                           WORK( 1+N ), 1 )
                     CALL BLAXPY( J-2, -X( 2, 1 ), T( 1, J ), 1,
     &                           WORK( 1+N ), 1 )
                  END IF
   60          CONTINUE
C
C              COPY THE VECTOR X OR Q*X TO VR AND NORMALIZE.
C
               IF( .NOT.OVER ) THEN
                  CALL BLCOPY( KI, WORK( 1+N ), 1, VR( 1, IS ), 1 )
C
                  II = IDAMAX( KI, VR( 1, IS ), 1 )
                  REMAX = ONE / ABS( VR( II, IS ) )
                  CALL BLSCAL( KI, REMAX, VR( 1, IS ), 1 )
C
                  DO 70 K = KI + 1, N
                     VR( K, IS ) = ZERO
   70             CONTINUE
               ELSE
                  IF( KI.GT.1 )
     &               CALL BLGEMV( 'N', N, KI-1, ONE, VR, LDVR,
     &                           WORK( 1+N ), 1, WORK( KI+N ),
     &                           VR( 1, KI ), 1 )
C
                  II = IDAMAX( N, VR( 1, KI ), 1 )
                  REMAX = ONE / ABS( VR( II, KI ) )
                  CALL BLSCAL( N, REMAX, VR( 1, KI ), 1 )
               END IF
C
            ELSE
C
C              COMPLEX RIGHT EIGENVECTOR.
C
C              INITIAL SOLVE
C                ( (T(KI-1,KI-1) T(KI-1,KI) ) - (WR + I* WI))*X = 0.
C                ( (T(KI,KI-1)   T(KI,KI)   )               )
C
               IF( ABS( T( KI-1, KI ) ).GE.ABS( T( KI, KI-1 ) ) ) THEN
                  WORK( KI-1+N ) = ONE
                  WORK( KI+N2 ) = WI / T( KI-1, KI )
               ELSE
                  WORK( KI-1+N ) = -WI / T( KI, KI-1 )
                  WORK( KI+N2 ) = ONE
               END IF
               WORK( KI+N ) = ZERO
               WORK( KI-1+N2 ) = ZERO
C
C              FORM RIGHT-HAND SIDE
C
               DO 80 K = 1, KI - 2
                  WORK( K+N ) = -WORK( KI-1+N )*T( K, KI-1 )
                  WORK( K+N2 ) = -WORK( KI+N2 )*T( K, KI )
   80          CONTINUE
C
C              SOLVE UPPER QUASI-TRIANGULAR SYSTEM:
C              (T(1:KI-2,1:KI-2) - (WR+I*WI))*X = SCALE*(WORK+I*WORK2)
C
               JNXT = KI - 2
               DO 90 J = KI - 2, 1, -1
                  IF( J.GT.JNXT )
     &               GO TO 90
                  J1 = J
                  J2 = J
                  JNXT = J - 1
                  IF( J.GT.1 ) THEN
                     IF( T( J, J-1 ).NE.ZERO ) THEN
                        J1 = J - 1
                        JNXT = J - 2
                     END IF
                  END IF
C
                  IF( J1.EQ.J2 ) THEN
C
C                    1-BY-1 DIAGONAL BLOCK
C
                     CALL FLALN2( .FALSE., 1, 2, SMIN, ONE, T( J, J ),
     &                            LDT, ONE, ONE, WORK( J+N ), N, WR, WI,
     &                            X, 2, SCALE, XNORM, IERR )
C
C                    SCALE X(1,1) AND X(1,2) TO AVOID OVERFLOW WHEN
C                    UPDATING THE RIGHT-HAND SIDE.
C
                     IF( XNORM.GT.ONE ) THEN
                        IF( WORK( J ).GT.BIGNUM / XNORM ) THEN
                           X( 1, 1 ) = X( 1, 1 ) / XNORM
                           X( 1, 2 ) = X( 1, 2 ) / XNORM
                           SCALE = SCALE / XNORM
                        END IF
                     END IF
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE ) THEN
                        CALL BLSCAL( KI, SCALE, WORK( 1+N ), 1 )
                        CALL BLSCAL( KI, SCALE, WORK( 1+N2 ), 1 )
                     END IF
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+N2 ) = X( 1, 2 )
C
C                    UPDATE THE RIGHT-HAND SIDE
C
                     CALL BLAXPY( J-1, -X( 1, 1 ), T( 1, J ), 1,
     &                           WORK( 1+N ), 1 )
                     CALL BLAXPY( J-1, -X( 1, 2 ), T( 1, J ), 1,
     &                           WORK( 1+N2 ), 1 )
C
                  ELSE
C
C                    2-BY-2 DIAGONAL BLOCK
C
                     CALL FLALN2( .FALSE., 2, 2, SMIN, ONE,
     &                            T( J-1, J-1 ), LDT, ONE, ONE,
     &                            WORK( J-1+N ), N, WR, WI, X, 2, SCALE,
     &                            XNORM, IERR )
C
C                    SCALE X TO AVOID OVERFLOW WHEN UPDATING
C                    THE RIGHT-HAND SIDE.
C
                     IF( XNORM.GT.ONE ) THEN
                        BETA = MAX( WORK( J-1 ), WORK( J ) )
                        IF( BETA.GT.BIGNUM / XNORM ) THEN
                           REC = ONE / XNORM
                           X( 1, 1 ) = X( 1, 1 )*REC
                           X( 1, 2 ) = X( 1, 2 )*REC
                           X( 2, 1 ) = X( 2, 1 )*REC
                           X( 2, 2 ) = X( 2, 2 )*REC
                           SCALE = SCALE*REC
                        END IF
                     END IF
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE ) THEN
                        CALL BLSCAL( KI, SCALE, WORK( 1+N ), 1 )
                        CALL BLSCAL( KI, SCALE, WORK( 1+N2 ), 1 )
                     END IF
                     WORK( J-1+N ) = X( 1, 1 )
                     WORK( J+N ) = X( 2, 1 )
                     WORK( J-1+N2 ) = X( 1, 2 )
                     WORK( J+N2 ) = X( 2, 2 )
C
C                    UPDATE THE RIGHT-HAND SIDE
C
                     CALL BLAXPY( J-2, -X( 1, 1 ), T( 1, J-1 ), 1,
     &                           WORK( 1+N ), 1 )
                     CALL BLAXPY( J-2, -X( 2, 1 ), T( 1, J ), 1,
     &                           WORK( 1+N ), 1 )
                     CALL BLAXPY( J-2, -X( 1, 2 ), T( 1, J-1 ), 1,
     &                           WORK( 1+N2 ), 1 )
                     CALL BLAXPY( J-2, -X( 2, 2 ), T( 1, J ), 1,
     &                           WORK( 1+N2 ), 1 )
                  END IF
   90          CONTINUE
C
C              COPY THE VECTOR X OR Q*X TO VR AND NORMALIZE.
C
               IF( .NOT.OVER ) THEN
                  CALL BLCOPY( KI, WORK( 1+N ), 1, VR( 1, IS-1 ), 1 )
                  CALL BLCOPY( KI, WORK( 1+N2 ), 1, VR( 1, IS ), 1 )
C
                  EMAX = ZERO
                  DO 100 K = 1, KI
                     EMAX = MAX( EMAX, ABS( VR( K, IS-1 ) )+
     &                      ABS( VR( K, IS ) ) )
  100             CONTINUE
C
                  REMAX = ONE / EMAX
                  CALL BLSCAL( KI, REMAX, VR( 1, IS-1 ), 1 )
                  CALL BLSCAL( KI, REMAX, VR( 1, IS ), 1 )
C
                  DO 110 K = KI + 1, N
                     VR( K, IS-1 ) = ZERO
                     VR( K, IS ) = ZERO
  110             CONTINUE
C
               ELSE
C
                  IF( KI.GT.2 ) THEN
                     CALL BLGEMV( 'N', N, KI-2, ONE, VR, LDVR,
     &                           WORK( 1+N ), 1, WORK( KI-1+N ),
     &                           VR( 1, KI-1 ), 1 )
                     CALL BLGEMV( 'N', N, KI-2, ONE, VR, LDVR,
     &                           WORK( 1+N2 ), 1, WORK( KI+N2 ),
     &                           VR( 1, KI ), 1 )
                  ELSE
                     CALL BLSCAL( N, WORK( KI-1+N ), VR( 1, KI-1 ), 1 )
                     CALL BLSCAL( N, WORK( KI+N2 ), VR( 1, KI ), 1 )
                  END IF
C
                  EMAX = ZERO
                  DO 120 K = 1, N
                     EMAX = MAX( EMAX, ABS( VR( K, KI-1 ) )+
     &                      ABS( VR( K, KI ) ) )
  120             CONTINUE
                  REMAX = ONE / EMAX
                  CALL BLSCAL( N, REMAX, VR( 1, KI-1 ), 1 )
                  CALL BLSCAL( N, REMAX, VR( 1, KI ), 1 )
               END IF
            END IF
C
            IS = IS - 1
            IF( IP.NE.0 )
     &         IS = IS - 1
  130       CONTINUE
            IF( IP.EQ.1 )
     &         IP = 0
            IF( IP.EQ.-1 )
     &         IP = 1
  140    CONTINUE
      END IF
C
      IF( LEFTV ) THEN
C
C        COMPUTE LEFT EIGENVECTORS.
C
         IP = 0
         IS = 1
         DO 260 KI = 1, N
C
            IF( IP.EQ.-1 )
     &         GO TO 250
            IF( KI.EQ.N )
     &         GO TO 150
            IF( T( KI+1, KI ).EQ.ZERO )
     &         GO TO 150
            IP = 1
C
  150       CONTINUE
            IF( SOMEV ) THEN
               IF( .NOT.SELECT( KI ) )
     &            GO TO 250
            END IF
C
C           COMPUTE THE KI-TH EIGENVALUE (WR,WI).
C
            WR = T( KI, KI )
            WI = ZERO
            IF( IP.NE.0 )
     &         WI = SQRT( ABS( T( KI, KI+1 ) ) )*
     &              SQRT( ABS( T( KI+1, KI ) ) )
            SMIN = MAX( ULP*( ABS( WR )+ABS( WI ) ), SMLNUM )
C
            IF( IP.EQ.0 ) THEN
C
C              REAL LEFT EIGENVECTOR.
C
               WORK( KI+N ) = ONE
C
C              FORM RIGHT-HAND SIDE
C
               DO 160 K = KI + 1, N
                  WORK( K+N ) = -T( KI, K )
  160          CONTINUE
C
C              SOLVE THE QUASI-TRIANGULAR SYSTEM:
C                 (T(KI+1:N,KI+1:N) - WR)'*X = SCALE*WORK
C
               VMAX = ONE
               VCRIT = BIGNUM
C
               JNXT = KI + 1
               DO 170 J = KI + 1, N
                  IF( J.LT.JNXT )
     &               GO TO 170
                  J1 = J
                  J2 = J
                  JNXT = J + 1
                  IF( J.LT.N ) THEN
                     IF( T( J+1, J ).NE.ZERO ) THEN
                        J2 = J + 1
                        JNXT = J + 2
                     END IF
                  END IF
C
                  IF( J1.EQ.J2 ) THEN
C
C                    1-BY-1 DIAGONAL BLOCK
C
C                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN FORMING
C                    THE RIGHT-HAND SIDE.
C
                     IF( WORK( J ).GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL BLSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
C
                     WORK( J+N ) = WORK( J+N ) -
     &                             BLSDOT( J-KI-1, T( KI+1, J ), 1,
     &                             WORK( KI+1+N ), 1 )
C
C                    SOLVE (T(J,J)-WR)'*X = WORK
C
                     CALL FLALN2( .FALSE., 1, 1, SMIN, ONE, T( J, J ),
     &                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     &                            ZERO, X, 2, SCALE, XNORM, IERR )
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE )
     &                  CALL BLSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                     WORK( J+N ) = X( 1, 1 )
                     VMAX = MAX( ABS( WORK( J+N ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
C
                  ELSE
C
C                    2-BY-2 DIAGONAL BLOCK
C
C                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN FORMING
C                    THE RIGHT-HAND SIDE.
C
                     BETA = MAX( WORK( J ), WORK( J+1 ) )
                     IF( BETA.GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL BLSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
C
                     WORK( J+N ) = WORK( J+N ) -
     &                             BLSDOT( J-KI-1, T( KI+1, J ), 1,
     &                             WORK( KI+1+N ), 1 )
C
                     WORK( J+1+N ) = WORK( J+1+N ) -
     &                               BLSDOT( J-KI-1, T( KI+1, J+1 ), 1,
     &                               WORK( KI+1+N ), 1 )
C
C                    SOLVE
C                      (T(J,J)-WR   T(J,J+1)     )'* X = SCALE*( WORK1 )
C                      (T(J+1,J)    T(J+1,J+1)-WR)             ( WORK2 )
C
                     CALL FLALN2( .TRUE., 2, 1, SMIN, ONE, T( J, J ),
     &                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     &                            ZERO, X, 2, SCALE, XNORM, IERR )
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE )
     &                  CALL BLSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+1+N ) = X( 2, 1 )
C
                     VMAX = MAX( ABS( WORK( J+N ) ),
     &                      ABS( WORK( J+1+N ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
C
                  END IF
  170          CONTINUE
C
C              COPY THE VECTOR X OR Q*X TO VL AND NORMALIZE.
C
               IF( .NOT.OVER ) THEN
                  CALL BLCOPY( N-KI+1, WORK( KI+N ), 1, VL( KI, IS ),1)
C
                  II = IDAMAX( N-KI+1, VL( KI, IS ), 1 ) + KI - 1
                  REMAX = ONE / ABS( VL( II, IS ) )
                  CALL BLSCAL( N-KI+1, REMAX, VL( KI, IS ), 1 )
C
                  DO 180 K = 1, KI - 1
                     VL( K, IS ) = ZERO
  180             CONTINUE
C
               ELSE
C
                  IF( KI.LT.N )
     &               CALL BLGEMV( 'N', N, N-KI, ONE,VL( 1, KI+1 ),LDVL,
     &                           WORK( KI+1+N ), 1, WORK( KI+N ),
     &                           VL( 1, KI ), 1 )
C
                  II = IDAMAX( N, VL( 1, KI ), 1 )
                  REMAX = ONE / ABS( VL( II, KI ) )
                  CALL BLSCAL( N, REMAX, VL( 1, KI ), 1 )
C
               END IF
C
            ELSE
C
C              COMPLEX LEFT EIGENVECTOR.
C
C               INITIAL SOLVE:
C                 ((T(KI,KI)    T(KI,KI+1) )' - (WR - I* WI))*X = 0.
C                 ((T(KI+1,KI) T(KI+1,KI+1))                )
C
               IF( ABS( T( KI, KI+1 ) ).GE.ABS( T( KI+1, KI ) ) ) THEN
                  WORK( KI+N ) = WI / T( KI, KI+1 )
                  WORK( KI+1+N2 ) = ONE
               ELSE
                  WORK( KI+N ) = ONE
                  WORK( KI+1+N2 ) = -WI / T( KI+1, KI )
               END IF
               WORK( KI+1+N ) = ZERO
               WORK( KI+N2 ) = ZERO
C
C              FORM RIGHT-HAND SIDE
C
               DO 190 K = KI + 2, N
                  WORK( K+N ) = -WORK( KI+N )*T( KI, K )
                  WORK( K+N2 ) = -WORK( KI+1+N2 )*T( KI+1, K )
  190          CONTINUE
C
C              SOLVE COMPLEX QUASI-TRIANGULAR SYSTEM:
C              ( T(KI+2,N:KI+2,N) - (WR-I*WI) )*X = WORK1+I*WORK2
C
               VMAX = ONE
               VCRIT = BIGNUM
C
               JNXT = KI + 2
               DO 200 J = KI + 2, N
                  IF( J.LT.JNXT )
     &               GO TO 200
                  J1 = J
                  J2 = J
                  JNXT = J + 1
                  IF( J.LT.N ) THEN
                     IF( T( J+1, J ).NE.ZERO ) THEN
                        J2 = J + 1
                        JNXT = J + 2
                     END IF
                  END IF
C
                  IF( J1.EQ.J2 ) THEN
C
C                    1-BY-1 DIAGONAL BLOCK
C
C                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN
C                    FORMING THE RIGHT-HAND SIDE ELEMENTS.
C
                     IF( WORK( J ).GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL BLSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        CALL BLSCAL( N-KI+1, REC, WORK( KI+N2 ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
C
                     WORK( J+N ) = WORK( J+N ) -
     &                             BLSDOT( J-KI-2, T( KI+2, J ), 1,
     &                             WORK( KI+2+N ), 1 )
                     WORK( J+N2 ) = WORK( J+N2 ) -
     &                              BLSDOT( J-KI-2, T( KI+2, J ), 1,
     &                              WORK( KI+2+N2 ), 1 )
C
C                    SOLVE (T(J,J)-(WR-I*WI))*(X11+I*X12)= WK+I*WK2
C
                     CALL FLALN2( .FALSE., 1, 2, SMIN, ONE, T( J, J ),
     &                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     &                            -WI, X, 2, SCALE, XNORM, IERR )
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE ) THEN
                        CALL BLSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                        CALL BLSCAL( N-KI+1, SCALE, WORK( KI+N2 ), 1 )
                     END IF
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+N2 ) = X( 1, 2 )
                     VMAX = MAX( ABS( WORK( J+N ) ),
     &                      ABS( WORK( J+N2 ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
C
                  ELSE
C
C                    2-BY-2 DIAGONAL BLOCK
C
C                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN FORMING
C                    THE RIGHT-HAND SIDE ELEMENTS.
C
                     BETA = MAX( WORK( J ), WORK( J+1 ) )
                     IF( BETA.GT.VCRIT ) THEN
                        REC = ONE / VMAX
                        CALL BLSCAL( N-KI+1, REC, WORK( KI+N ), 1 )
                        CALL BLSCAL( N-KI+1, REC, WORK( KI+N2 ), 1 )
                        VMAX = ONE
                        VCRIT = BIGNUM
                     END IF
C
                     WORK( J+N ) = WORK( J+N ) -
     &                             BLSDOT( J-KI-2, T( KI+2, J ), 1,
     &                             WORK( KI+2+N ), 1 )
C
                     WORK( J+N2 ) = WORK( J+N2 ) -
     &                              BLSDOT( J-KI-2, T( KI+2, J ), 1,
     &                              WORK( KI+2+N2 ), 1 )
C
                     WORK( J+1+N ) = WORK( J+1+N ) -
     &                               BLSDOT( J-KI-2, T( KI+2, J+1 ), 1,
     &                               WORK( KI+2+N ), 1 )
C
                     WORK( J+1+N2 ) = WORK( J+1+N2 ) -
     &                                BLSDOT( J-KI-2, T( KI+2, J+1 ), 1,
     &                                WORK( KI+2+N2 ), 1 )
C
C                    SOLVE 2-BY-2 COMPLEX LINEAR EQUATION
C                      ((T(J,J)   T(J,J+1)  )'-(WR-I*WI)*I)*X = SCALE*B
C                      ((T(J+1,J) T(J+1,J+1))             )
C
                     CALL FLALN2( .TRUE., 2, 2, SMIN, ONE, T( J, J ),
     &                            LDT, ONE, ONE, WORK( J+N ), N, WR,
     &                            -WI, X, 2, SCALE, XNORM, IERR )
C
C                    SCALE IF NECESSARY
C
                     IF( SCALE.NE.ONE ) THEN
                        CALL BLSCAL( N-KI+1, SCALE, WORK( KI+N ), 1 )
                        CALL BLSCAL( N-KI+1, SCALE, WORK( KI+N2 ), 1 )
                     END IF
                     WORK( J+N ) = X( 1, 1 )
                     WORK( J+N2 ) = X( 1, 2 )
                     WORK( J+1+N ) = X( 2, 1 )
                     WORK( J+1+N2 ) = X( 2, 2 )
                     VMAX = MAX( ABS( X( 1, 1 ) ), ABS( X( 1, 2 ) ),
     &                      ABS( X( 2, 1 ) ), ABS( X( 2, 2 ) ), VMAX )
                     VCRIT = BIGNUM / VMAX
C
                  END IF
  200          CONTINUE
C
C              COPY THE VECTOR X OR Q*X TO VL AND NORMALIZE.
C
  210          CONTINUE
               IF( .NOT.OVER ) THEN
                  CALL BLCOPY( N-KI+1, WORK( KI+N ), 1, VL( KI, IS ),1)
                  CALL BLCOPY( N-KI+1, WORK( KI+N2 ), 1, VL( KI, IS+1 ),
     &                        1 )
C
                  EMAX = ZERO
                  DO 220 K = KI, N
                     EMAX = MAX( EMAX, ABS( VL( K, IS ) )+
     &                      ABS( VL( K, IS+1 ) ) )
  220             CONTINUE
                  REMAX = ONE / EMAX
                  CALL BLSCAL( N-KI+1, REMAX, VL( KI, IS ), 1 )
                  CALL BLSCAL( N-KI+1, REMAX, VL( KI, IS+1 ), 1 )
C
                  DO 230 K = 1, KI - 1
                     VL( K, IS ) = ZERO
                     VL( K, IS+1 ) = ZERO
  230             CONTINUE
               ELSE
                  IF( KI.LT.N-1 ) THEN
                     CALL BLGEMV( 'N', N, N-KI-1, ONE, VL( 1, KI+2 ),
     &                           LDVL, WORK( KI+2+N ), 1, WORK( KI+N ),
     &                           VL( 1, KI ), 1 )
                     CALL BLGEMV( 'N', N, N-KI-1, ONE, VL( 1, KI+2 ),
     &                           LDVL, WORK( KI+2+N2 ), 1,
     &                           WORK( KI+1+N2 ), VL( 1, KI+1 ), 1 )
                  ELSE
                     CALL BLSCAL( N, WORK( KI+N ), VL( 1, KI ), 1 )
                     CALL BLSCAL( N, WORK( KI+1+N2 ), VL( 1, KI+1 ), 1 )
                  END IF
C
                  EMAX = ZERO
                  DO 240 K = 1, N
                     EMAX = MAX( EMAX, ABS( VL( K, KI ) )+
     &                      ABS( VL( K, KI+1 ) ) )
  240             CONTINUE
                  REMAX = ONE / EMAX
                  CALL BLSCAL( N, REMAX, VL( 1, KI ), 1 )
                  CALL BLSCAL( N, REMAX, VL( 1, KI+1 ), 1 )
C
               END IF
C
            END IF
C
            IS = IS + 1
            IF( IP.NE.0 )
     &         IS = IS + 1
  250       CONTINUE
            IF( IP.EQ.-1 )
     &         IP = 0
            IF( IP.EQ.1 )
     &         IP = -1
C
  260    CONTINUE
C
      END IF
C
 1000 CONTINUE
C
C     END OF FTREVC
C
      END
