      SUBROUTINE GLACON( N, V, X, EST, KASE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C  PURPOSE
C  =======
C
C  GLACON ESTIMATES THE 1-NORM OF A SQUARE, COMPLEX MATRIX A.
C  REVERSE COMMUNICATION IS USED FOR EVALUATING MATRIX-VECTOR PRODUCTS.
C
C  ARGUMENTS
C  =========
C
C  N      (INPUT) INTEGER
C         THE ORDER OF THE MATRIX.  N >= 1.
C
C  V      (WORKSPACE) COMPLEX*16 ARRAY, DIMENSION (N)
C         ON THE FINAL RETURN, V = A*W,  WHERE  EST = NORM(V)/NORM(W)
C         (W IS NOT RETURNED).
C
C  X      (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (N)
C         ON AN INTERMEDIATE RETURN, X SHOULD BE OVERWRITTEN BY
C               A * X,   IF KASE=1,
C               A' * X,  IF KASE=2,
C         WHERE A' IS THE CONJUGATE TRANSPOSE OF A, AND GLACON MUST BE
C         RE-CALLED WITH ALL THE OTHER PARAMETERS UNCHANGED.
C
C  EST    (OUTPUT) DOUBLE PRECISION
C         AN ESTIMATE (A LOWER BOUND) FOR NORM(A).
C
C  KASE   (INPUT/OUTPUT) INTEGER
C         ON THE INITIAL CALL TO GLACON, KASE SHOULD BE 0.
C         ON AN INTERMEDIATE RETURN, KASE WILL BE 1 OR 2, INDICATING
C         WHETHER X SHOULD BE OVERWRITTEN BY A * X  OR A' * X.
C         ON THE FINAL RETURN FROM GLACON, KASE WILL AGAIN BE 0.
C
C  FURTHER DETAILS
C  ======= =======
C
C  CONTRIBUTED BY NICK HIGHAM, UNIVERSITY OF MANCHESTER.
C  ORIGINALLY NAMED CONEST, DATED MARCH 16, 1988.
C
C  REFERENCE: N.J. HIGHAM, "FORTRAN CODES FOR ESTIMATING THE ONE-NORM OF
C  A REAL OR COMPLEX MATRIX, WITH APPLICATIONS TO CONDITION ESTIMATION",
C  ACM TRANS. MATH. SOFT., VOL. 14, NO. 4, PP. 381-396, DECEMBER 1988.
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
      INTEGER            KASE, N
      REAL*8             EST
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         V( N ), X( N )
C     ..
C
C     .. PARAMETERS ..
      INTEGER            ITMAX
      PARAMETER          ( ITMAX = 5 )
      REAL*8             ONE, TWO
      PARAMETER          ( ONE = 1.0D0, TWO = 2.0D0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D0, 0.0D0 ),
     $                   CONE = ( 1.0D0, 0.0D0 ) )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, ITER, J, JLAST, JUMP
      REAL*8             ALTSGN, ESTOLD, SAFMIN, TEMP
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER            GZMAX1
      REAL*8             GZSUM1,R8PREM, R8MIEM
C     ..
C     .. SAVE STATEMENT ..
      SAVE
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      SAFMIN = R8MIEM() / (R8PREM()*0.5D0)
      IF( KASE.EQ.0 ) THEN
         DO 10 I = 1, N
            X( I ) = DCMPLX( ONE / DBLE( N ) )
   10    CONTINUE
         KASE = 1
         JUMP = 1
         GOTO 1000
      END IF
C
      GO TO ( 20, 40, 70, 90, 120 )JUMP
C
C     ................ ENTRY   (JUMP = 1)
C     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
C
   20 CONTINUE
      IF( N.EQ.1 ) THEN
         V( 1 ) = X( 1 )
         EST = ABS( V( 1 ) )
C        ... QUIT
         GO TO 130
      END IF
      EST = GZSUM1( N, X, 1 )
C
      DO 30 I = 1, N
         IF( ABS( X( I ) ).GT.SAFMIN ) THEN
            X( I ) = X( I ) / DCMPLX( ABS( X( I ) ) )
         ELSE
            X( I ) = CONE
         END IF
   30 CONTINUE
      KASE = 2
      JUMP = 2
      GOTO 1000
C
C     ................ ENTRY   (JUMP = 2)
C     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY ZTRANS(A)*X.
C
   40 CONTINUE
      J = GZMAX1( N, X, 1 )
      ITER = 2
C
C     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
C
   50 CONTINUE
      DO 60 I = 1, N
         X( I ) = CZERO
   60 CONTINUE
      X( J ) = CONE
      KASE = 1
      JUMP = 3
      GOTO 1000
C
C     ................ ENTRY   (JUMP = 3)
C     X HAS BEEN OVERWRITTEN BY A*X.
C
   70 CONTINUE
      CALL GLCOPY( N, X, 1, V, 1 )
      ESTOLD = EST
      EST = GZSUM1( N, V, 1 )
C
C     TEST FOR CYCLING.
      IF( EST.LE.ESTOLD )
     $   GO TO 100
C
      DO 80 I = 1, N
         IF( ABS( X( I ) ).GT.SAFMIN ) THEN
            X( I ) = X( I ) / DCMPLX( ABS( X( I ) ) )
         ELSE
            X( I ) = CONE
         END IF
   80 CONTINUE
      KASE = 2
      JUMP = 4
      GOTO 1000
C
C     ................ ENTRY   (JUMP = 4)
C     X HAS BEEN OVERWRITTEN BY ZTRANS(A)*X.
C
   90 CONTINUE
      JLAST = J
      J = GZMAX1( N, X, 1 )
      IF( ( DBLE( X( JLAST ) ).NE.ABS( DBLE( X( J ) ) ) ) .AND.
     $    ( ITER.LT.ITMAX ) ) THEN
         ITER = ITER + 1
         GO TO 50
      END IF
C
C     ITERATION COMPLETE.  FINAL STAGE.
C
  100 CONTINUE
      ALTSGN = ONE
      DO 110 I = 1, N
         X( I ) = DCMPLX( ALTSGN*( ONE+DBLE( I-1 ) / DBLE( N-1 ) ) )
         ALTSGN = -ALTSGN
  110 CONTINUE
      KASE = 1
      JUMP = 5
      GOTO 1000
C
C     ................ ENTRY   (JUMP = 5)
C     X HAS BEEN OVERWRITTEN BY A*X.
C
  120 CONTINUE
      TEMP = TWO*( GZSUM1( N, X, 1 ) / DBLE( 3*N ) )
      IF( TEMP.GT.EST ) THEN
         CALL GLCOPY( N, X, 1, V, 1 )
         EST = TEMP
      END IF
C
  130 CONTINUE
      KASE = 0
 1000 CONTINUE
C
C     END OF GLACON
C
      END
