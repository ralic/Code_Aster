      SUBROUTINE GLTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C  PURPOSE
C  =======
C
C  GLTRMM  PERFORMS ONE OF THE MATRIX-MATRIX OPERATIONS
C
C     B := ALPHA*OP( A )*B,   OR   B := ALPHA*B*OP( A )
C
C  WHERE  ALPHA  IS A SCALAR,  B  IS AN M BY N MATRIX,  A  IS A UNIT, OR
C  NON-UNIT,  UPPER OR LOWER TRIANGULAR MATRIX  AND  OP( A )  IS ONE  OF
C
C     OP( A ) = A   OR   OP( A ) = A'   OR   OP( A ) = CONJG( A' ).
C
C  PARAMETERS
C  ==========
C
C  SIDE   - CHARACTER*1.
C           ON ENTRY,  SIDE SPECIFIES WHETHER  OP( A ) MULTIPLIES B FROM
C           THE LEFT OR RIGHT AS FOLLOWS:
C
C              SIDE = 'L' OR 'L'   B := ALPHA*OP( A )*B.
C
C              SIDE = 'R' OR 'R'   B := ALPHA*B*OP( A ).
C
C           UNCHANGED ON EXIT.
C
C  UPLO   - CHARACTER*1.
C           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX A IS AN UPPER OR
C           LOWER TRIANGULAR MATRIX AS FOLLOWS:
C
C              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
C
C              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
C
C           UNCHANGED ON EXIT.
C
C  TRANSA - CHARACTER*1.
C           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
C           THE MATRIX MULTIPLICATION AS FOLLOWS:
C
C              TRANSA = 'N' OR 'N'   OP( A ) = A.
C
C              TRANSA = 'T' OR 'T'   OP( A ) = A'.
C
C              TRANSA = 'C' OR 'C'   OP( A ) = CONJG( A' ).
C
C           UNCHANGED ON EXIT.
C
C  DIAG   - CHARACTER*1.
C           ON ENTRY, DIAG SPECIFIES WHETHER OR NOT A IS UNIT TRIANGULAR
C           AS FOLLOWS:
C
C              DIAG = 'U' OR 'U'   A IS ASSUMED TO BE UNIT TRIANGULAR.
C
C              DIAG = 'N' OR 'N'   A IS NOT ASSUMED TO BE UNIT
C                                  TRIANGULAR.
C
C           UNCHANGED ON EXIT.
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF B. M MUST BE AT
C           LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF B.  N MUST BE
C           AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - COMPLEX*16      .
C           ON ENTRY,  ALPHA SPECIFIES THE SCALAR  ALPHA. WHEN  ALPHA IS
C           ZERO THEN  A IS NOT REFERENCED AND  B NEED NOT BE SET BEFORE
C           ENTRY.
C           UNCHANGED ON EXIT.
C
C  A      - COMPLEX*16       ARRAY OF DIMENSION ( LDA, K ), WHERE K IS M
C           WHEN  SIDE = 'L' OR 'L'  AND IS  N  WHEN  SIDE = 'R' OR 'R'.
C           BEFORE ENTRY  WITH  UPLO = 'U' OR 'U',  THE  LEADING  K BY K
C           UPPER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE UPPER
C           TRIANGULAR MATRIX  AND THE STRICTLY LOWER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           BEFORE ENTRY  WITH  UPLO = 'L' OR 'L',  THE  LEADING  K BY K
C           LOWER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE LOWER
C           TRIANGULAR MATRIX  AND THE STRICTLY UPPER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           NOTE THAT WHEN  DIAG = 'U' OR 'U',  THE DIAGONAL ELEMENTS OF
C           A  ARE NOT REFERENCED EITHER,  BUT ARE ASSUMED TO BE  UNITY.
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM.  WHEN  SIDE = 'L' OR 'L'  THEN
C           LDA  MUST BE AT LEAST  MAX( 1, M ),  WHEN  SIDE = 'R' OR 'R'
C           THEN LDA MUST BE AT LEAST MAX( 1, N ).
C           UNCHANGED ON EXIT.
C
C  B      - COMPLEX*16       ARRAY OF DIMENSION ( LDB, N ).
C           BEFORE ENTRY,  THE LEADING  M BY N PART OF THE ARRAY  B MUST
C           CONTAIN THE MATRIX  B,  AND  ON EXIT  IS OVERWRITTEN  BY THE
C           TRANSFORMED MATRIX.
C
C  LDB    - INTEGER.
C           ON ENTRY, LDB SPECIFIES THE FIRST DIMENSION OF B AS DECLARED
C           IN  THE  CALLING  (SUB)  PROGRAM.   LDB  MUST  BE  AT  LEAST
C           MAX( 1, M ).
C           UNCHANGED ON EXIT.
C
C
C  LEVEL 3 BLAS ROUTINE.
C
C  -- WRITTEN ON 8-FEBRUARY-1989.
C     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
C     IAIN DUFF, AERE HARWELL.
C     JEREMY DU CROZ, NUMERICAL ALGORITHMS GROUP LTD.
C     SVEN HAMMARLING, NUMERICAL ALGORITHMS GROUP LTD.
C
C
C ======================================================================
C REMPLACE LA BLAS SAXPY SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      COMPLEX*16         ALPHA
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         A( LDA, * ), B( LDB, * )
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     .. LOCAL SCALARS ..
      LOGICAL            LSIDE, NOCONJ, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      COMPLEX*16         TEMP
C     .. PARAMETERS ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      LSIDE  = LLSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOCONJ = LLSAME( TRANSA, 'T' )
      NOUNIT = LLSAME( DIAG  , 'N' )
      UPPER  = LLSAME( UPLO  , 'U' )
C
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LLSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LLSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LLSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LLSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LLSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LLSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LLSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'GLTRMM', INFO )
         GOTO 9999
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( N.EQ.0 )
     $   GOTO 9999
C
C     AND WHEN  ALPHA.EQ.ZERO.
C
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         GOTO 9999
      END IF
C
C     START THE OPERATIONS.
C
      IF( LSIDE )THEN
         IF( LLSAME( TRANSA, 'N' ) )THEN
C
C           FORM  B := ALPHA*A*B.
C
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
C
C           FORM  B := ALPHA*B*A'   OR   B := ALPHA*B*CONJG( A' ).
C
            IF( UPPER )THEN
               DO 120, J = 1, N
                  DO 110, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOCONJ )THEN
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( I, I )
                        DO 90, K = 1, I - 1
                           TEMP = TEMP + A( K, I )*B( K, J )
   90                   CONTINUE
                     ELSE
                        IF( NOUNIT )
     $                     TEMP = TEMP*DCONJG( A( I, I ) )
                        DO 100, K = 1, I - 1
                           TEMP = TEMP + DCONJG( A( K, I ) )*B( K, J )
  100                   CONTINUE
                     END IF
                     B( I, J ) = ALPHA*TEMP
  110             CONTINUE
  120          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = 1, M
                     TEMP = B( I, J )
                     IF( NOCONJ )THEN
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( I, I )
                        DO 130, K = I + 1, M
                           TEMP = TEMP + A( K, I )*B( K, J )
  130                   CONTINUE
                     ELSE
                        IF( NOUNIT )
     $                     TEMP = TEMP*DCONJG( A( I, I ) )
                        DO 140, K = I + 1, M
                           TEMP = TEMP + DCONJG( A( K, I ) )*B( K, J )
  140                   CONTINUE
                     END IF
                     B( I, J ) = ALPHA*TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LLSAME( TRANSA, 'N' ) )THEN
C
C           FORM  B := ALPHA*B*A.
C
            IF( UPPER )THEN
               DO 200, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 170, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  170             CONTINUE
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
  200          CONTINUE
            ELSE
               DO 240, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 210, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  210             CONTINUE
                  DO 230, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 220, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  220                   CONTINUE
                     END IF
  230             CONTINUE
  240          CONTINUE
            END IF
         ELSE
C
C           FORM  B := ALPHA*B*A'   OR   B := ALPHA*B*CONJG( A' ).
C
            IF( UPPER )THEN
               DO 280, K = 1, N
                  DO 260, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        IF( NOCONJ )THEN
                           TEMP = ALPHA*A( J, K )
                        ELSE
                           TEMP = ALPHA*DCONJG( A( J, K ) )
                        END IF
                        DO 250, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  250                   CONTINUE
                     END IF
  260             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )THEN
                     IF( NOCONJ )THEN
                        TEMP = TEMP*A( K, K )
                     ELSE
                        TEMP = TEMP*DCONJG( A( K, K ) )
                     END IF
                  END IF
                  IF( TEMP.NE.ONE )THEN
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
  280          CONTINUE
            ELSE
               DO 320, K = N, 1, -1
                  DO 300, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        IF( NOCONJ )THEN
                           TEMP = ALPHA*A( J, K )
                        ELSE
                           TEMP = ALPHA*DCONJG( A( J, K ) )
                        END IF
                        DO 290, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  290                   CONTINUE
                     END IF
  300             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )THEN
                     IF( NOCONJ )THEN
                        TEMP = TEMP*A( K, K )
                     ELSE
                        TEMP = TEMP*DCONJG( A( K, K ) )
                     END IF
                  END IF
                  IF( TEMP.NE.ONE )THEN
                     DO 310, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  310                CONTINUE
                  END IF
  320          CONTINUE
            END IF
         END IF
      END IF
C
9999  CONTINUE
C
C     END OF GLTRMM .
C
      END
