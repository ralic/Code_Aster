      SUBROUTINE GLGEMM ( TRANSA, TRANSB, M, N, K, ALPHA,A,LDA,B,LDB,
     $                   BETA, C, LDC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C
C  PURPOSE
C  =======
C
C  GLGEMM  PERFORMS ONE OF THE MATRIX-MATRIX OPERATIONS
C
C     C := ALPHA*OP( A )*OP( B ) + BETA*C,
C
C  WHERE  OP( X ) IS ONE OF
C
C     OP( X ) = X   OR   OP( X ) = X'   OR   OP( X ) = CONJG( X' ),
C
C  ALPHA AND BETA ARE SCALARS, AND A, B AND C ARE MATRICES, WITH OP( A )
C  AN M BY K MATRIX,  OP( B )  A  K BY N MATRIX AND  C AN M BY N MATRIX.
C
C  PARAMETERS
C  ==========
C
C  TRANSA - CHARACTER*1.
C           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
C           THE MATRIX MULTIPLICATION AS FOLLOWS:
C
C              TRANSA = 'N' OR 'N',  OP( A ) = A.
C
C              TRANSA = 'T' OR 'T',  OP( A ) = A'.
C
C              TRANSA = 'C' OR 'C',  OP( A ) = CONJG( A' ).
C
C           UNCHANGED ON EXIT.
C
C  TRANSB - CHARACTER*1.
C           ON ENTRY, TRANSB SPECIFIES THE FORM OF OP( B ) TO BE USED IN
C           THE MATRIX MULTIPLICATION AS FOLLOWS:
C
C              TRANSB = 'N' OR 'N',  OP( B ) = B.
C
C              TRANSB = 'T' OR 'T',  OP( B ) = B'.
C
C              TRANSB = 'C' OR 'C',  OP( B ) = CONJG( B' ).
C
C           UNCHANGED ON EXIT.
C
C  M      - INTEGER.
C           ON ENTRY,  M  SPECIFIES  THE NUMBER  OF ROWS  OF THE  MATRIX
C           OP( A )  AND OF THE  MATRIX  C.  M  MUST  BE AT LEAST  ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY,  N  SPECIFIES THE NUMBER  OF COLUMNS OF THE MATRIX
C           OP( B ) AND THE NUMBER OF COLUMNS OF THE MATRIX C. N MUST BE
C           AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  K      - INTEGER.
C           ON ENTRY,  K  SPECIFIES  THE NUMBER OF COLUMNS OF THE MATRIX
C           OP( A ) AND THE NUMBER OF ROWS OF THE MATRIX OP( B ). K MUST
C           BE AT LEAST  ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - COMPLEX*16      .
C           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
C           UNCHANGED ON EXIT.
C
C  A      - COMPLEX*16       ARRAY OF DIMENSION ( LDA, KA ), WHERE KA IS
C           K  WHEN  TRANSA = 'N' OR 'N',  AND IS  M  OTHERWISE.
C           BEFORE ENTRY WITH  TRANSA = 'N' OR 'N',  THE LEADING  M BY K
C           PART OF THE ARRAY  A  MUST CONTAIN THE MATRIX  A,  OTHERWISE
C           THE LEADING  K BY M  PART OF THE ARRAY  A  MUST CONTAIN  THE
C           MATRIX A.
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. WHEN  TRANSA = 'N' OR 'N' THEN
C           LDA MUST BE AT LEAST  MAX( 1, M ), OTHERWISE  LDA MUST BE AT
C           LEAST  MAX( 1, K ).
C           UNCHANGED ON EXIT.
C
C  B      - COMPLEX*16       ARRAY OF DIMENSION ( LDB, KB ), WHERE KB IS
C           N  WHEN  TRANSB = 'N' OR 'N',  AND IS  K  OTHERWISE.
C           BEFORE ENTRY WITH  TRANSB = 'N' OR 'N',  THE LEADING  K BY N
C           PART OF THE ARRAY  B  MUST CONTAIN THE MATRIX  B,  OTHERWISE
C           THE LEADING  N BY K  PART OF THE ARRAY  B  MUST CONTAIN  THE
C           MATRIX B.
C           UNCHANGED ON EXIT.
C
C  LDB    - INTEGER.
C           ON ENTRY, LDB SPECIFIES THE FIRST DIMENSION OF B AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. WHEN  TRANSB = 'N' OR 'N' THEN
C           LDB MUST BE AT LEAST  MAX( 1, K ), OTHERWISE  LDB MUST BE AT
C           LEAST  MAX( 1, N ).
C           UNCHANGED ON EXIT.
C
C  BETA   - COMPLEX*16      .
C           ON ENTRY,  BETA  SPECIFIES THE SCALAR  BETA.  WHEN  BETA  IS
C           SUPPLIED AS ZERO THEN C NEED NOT BE SET ON INPUT.
C           UNCHANGED ON EXIT.
C
C  C      - COMPLEX*16       ARRAY OF DIMENSION ( LDC, N ).
C           BEFORE ENTRY, THE LEADING  M BY N  PART OF THE ARRAY  C MUST
C           CONTAIN THE MATRIX  C,  EXCEPT WHEN  BETA  IS ZERO, IN WHICH
C           CASE C NEED NOT BE SET ON ENTRY.
C           ON EXIT, THE ARRAY  C  IS OVERWRITTEN BY THE  M BY N  MATRIX
C           ( ALPHA*OP( A )*OP( B ) + BETA*C ).
C
C  LDC    - INTEGER.
C           ON ENTRY, LDC SPECIFIES THE FIRST DIMENSION OF C AS DECLARED
C           IN  THE  CALLING  (SUB)  PROGRAM.   LDC  MUST  BE  AT  LEAST
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
C ======================================================================
C REMPLACE LA BLAS ZGEMM SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      COMPLEX*16         ALPHA, BETA
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * )
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     .. LOCAL SCALARS ..
      LOGICAL            CONJA, CONJB, NOTA, NOTB
      INTEGER            I, INFO, J, L, NROWA, NROWB
      COMPLEX*16         TEMP
C     .. PARAMETERS ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     SET  NOTA  AND  NOTB  AS  TRUE IF  A  AND  B  RESPECTIVELY ARE NOT
C     CONJUGATED OR TRANSPOSED, SET  CONJA AND CONJB  AS TRUE IF  A  AND
C     B  RESPECTIVELY ARE TO BE  TRANSPOSED BUT  NOT CONJUGATED  AND SET
C     NROWA AND  NROWB  AS THE NUMBER OF ROWS AND  COLUMNS  OF  A
C     AND THE NUMBER OF ROWS OF  B  RESPECTIVELY.
C
      NOTA  = LLSAME( TRANSA, 'N' )
      NOTB  = LLSAME( TRANSB, 'N' )
      CONJA = LLSAME( TRANSA, 'C' )
      CONJB = LLSAME( TRANSB, 'C' )
      IF( NOTA )THEN
         NROWA = M
      ELSE
         NROWA = K
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.CONJA                ).AND.
     $         ( .NOT.LLSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.CONJB                ).AND.
     $         ( .NOT.LLSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'GLGEMM', INFO )
         GOTO 9999
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   GOTO 9999
C
C     AND WHEN  ALPHA.EQ.ZERO.
C
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         GOTO 9999
      END IF
C
C     START THE OPERATIONS.
C
      IF( NOTB )THEN
         IF( NOTA )THEN
C
C           FORM  C := ALPHA*A*B + BETA*C.
C
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE IF( CONJA )THEN
C
C           FORM  C := ALPHA*CONJG( A' )*B + BETA*C.
C
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + DCONJG( A( L, I ) )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         ELSE
C
C           FORM  C := ALPHA*A'*B + BETA*C
C
            DO 150, J = 1, N
               DO 140, I = 1, M
                  TEMP = ZERO
                  DO 130, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  130             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  140          CONTINUE
  150       CONTINUE
         END IF
      ELSE IF( NOTA )THEN
         IF( CONJB )THEN
C
C           FORM  C := ALPHA*A*CONJG( B' ) + BETA*C.
C
            DO 200, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 160, I = 1, M
                     C( I, J ) = ZERO
  160             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 170, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  170             CONTINUE
               END IF
               DO 190, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*DCONJG( B( J, L ) )
                     DO 180, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  180                CONTINUE
                  END IF
  190          CONTINUE
  200       CONTINUE
         ELSE
C
C           FORM  C := ALPHA*A*B'          + BETA*C
C
            DO 250, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 210, I = 1, M
                     C( I, J ) = ZERO
  210             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 220, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  220             CONTINUE
               END IF
               DO 240, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 230, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  230                CONTINUE
                  END IF
  240          CONTINUE
  250       CONTINUE
         END IF
      ELSE IF( CONJA )THEN
         IF( CONJB )THEN
C
C           FORM  C := ALPHA*CONJG( A' )*CONJG( B' ) + BETA*C.
C
            DO 280, J = 1, N
               DO 270, I = 1, M
                  TEMP = ZERO
                  DO 260, L = 1, K
                     TEMP = TEMP +
     $                      DCONJG( A( L, I ) )*DCONJG( B( J, L ) )
  260             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  270          CONTINUE
  280       CONTINUE
         ELSE
C
C           FORM  C := ALPHA*CONJG( A' )*B' + BETA*C
C
            DO 310, J = 1, N
               DO 300, I = 1, M
                  TEMP = ZERO
                  DO 290, L = 1, K
                     TEMP = TEMP + DCONJG( A( L, I ) )*B( J, L )
  290             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  300          CONTINUE
  310       CONTINUE
         END IF
      ELSE
         IF( CONJB )THEN
C
C           FORM  C := ALPHA*A'*CONJG( B' ) + BETA*C
C
            DO 340, J = 1, N
               DO 330, I = 1, M
                  TEMP = ZERO
                  DO 320, L = 1, K
                     TEMP = TEMP + A( L, I )*DCONJG( B( J, L ) )
  320             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  330          CONTINUE
  340       CONTINUE
         ELSE
C
C           FORM  C := ALPHA*A'*B' + BETA*C
C
            DO 370, J = 1, N
               DO 360, I = 1, M
                  TEMP = ZERO
                  DO 350, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  350             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  360          CONTINUE
  370       CONTINUE
         END IF
      END IF
C
9999  CONTINUE
C
C     END OF GLGEMM .
C
      END
