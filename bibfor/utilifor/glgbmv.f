      SUBROUTINE GLGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
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
C  GLGBMV  PERFORMS ONE OF THE MATRIX-VECTOR OPERATIONS
C
C     Y := ALPHA*A*X + BETA*Y,   OR   Y := ALPHA*A'*X + BETA*Y,   OR
C
C     Y := ALPHA*CONJG( A' )*X + BETA*Y,
C
C  WHERE ALPHA AND BETA ARE SCALARS, X AND Y ARE VECTORS AND A IS AN
C  M BY N BAND MATRIX, WITH KL SUB-DIAGONALS AND KU SUPER-DIAGONALS.
C
C  PARAMETERS
C  ==========
C
C  TRANS  - CHARACTER*1.
C           ON ENTRY, TRANS SPECIFIES THE OPERATION TO BE PERFORMED AS
C           FOLLOWS:
C
C              TRANS = 'N' OR 'N'   Y := ALPHA*A*X + BETA*Y.
C
C              TRANS = 'T' OR 'T'   Y := ALPHA*A'*X + BETA*Y.
C
C              TRANS = 'C' OR 'C'   Y := ALPHA*CONJG( A' )*X + BETA*Y.
C
C           UNCHANGED ON EXIT.
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
C           M MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
C           N MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  KL     - INTEGER.
C           ON ENTRY, KL SPECIFIES THE NUMBER OF SUB-DIAGONALS OF THE
C           MATRIX A. KL MUST SATISFY  0 .LE. KL.
C           UNCHANGED ON EXIT.
C
C  KU     - INTEGER.
C           ON ENTRY, KU SPECIFIES THE NUMBER OF SUPER-DIAGONALS OF THE
C           MATRIX A. KU MUST SATISFY  0 .LE. KU.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - COMPLEX*16      .
C           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
C           UNCHANGED ON EXIT.
C
C  A      - COMPLEX*16       ARRAY OF DIMENSION ( LDA, N ).
C           BEFORE ENTRY, THE LEADING ( KL + KU + 1 ) BY N PART OF THE
C           ARRAY A MUST CONTAIN THE MATRIX OF COEFFICIENTS, SUPPLIED
C           COLUMN BY COLUMN, WITH THE LEADING DIAGONAL OF THE MATRIX IN
C           ROW ( KU + 1 ) OF THE ARRAY, THE FIRST SUPER-DIAGONAL
C           STARTING AT POSITION 2 IN ROW KU, THE FIRST SUB-DIAGONAL
C           STARTING AT POSITION 1 IN ROW ( KU + 2 ), AND SO ON.
C           ELEMENTS IN THE ARRAY A THAT DO NOT CORRESPOND TO ELEMENTS
C           IN THE BAND MATRIX (SUCH AS THE TOP LEFT KU BY KU TRIANGLE)
C           ARE NOT REFERENCED.
C           THE FOLLOWING PROGRAM SEGMENT WILL TRANSFER A BAND MATRIX
C           FROM CONVENTIONAL FULL MATRIX STORAGE TO BAND STORAGE:
C
C                 DO 20, J = 1, N
C                    K = KU + 1 - J
C                    DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )
C                       A( K + I, J ) = MATRIX( I, J )
C              10    CONTINUE
C              20 CONTINUE
C
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
C           ( KL + KU + 1 ).
C           UNCHANGED ON EXIT.
C
C  X      - COMPLEX*16       ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCX ) ) WHEN TRANS = 'N' OR 'N'
C           AND AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCX ) ) OTHERWISE.
C           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE
C           VECTOR X.
C           UNCHANGED ON EXIT.
C
C  INCX   - INTEGER.
C           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           X. INCX MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C  BETA   - COMPLEX*16      .
C           ON ENTRY, BETA SPECIFIES THE SCALAR BETA. WHEN BETA IS
C           SUPPLIED AS ZERO THEN Y NEED NOT BE SET ON INPUT.
C           UNCHANGED ON EXIT.
C
C  Y      - COMPLEX*16       ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCY ) ) WHEN TRANS = 'N' OR 'N'
C           AND AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCY ) ) OTHERWISE.
C           BEFORE ENTRY, THE INCREMENTED ARRAY Y MUST CONTAIN THE
C           VECTOR Y. ON EXIT, Y IS OVERWRITTEN BY THE UPDATED VECTOR Y.
C
C
C  INCY   - INTEGER.
C           ON ENTRY, INCY SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           Y. INCY MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C
C  LEVEL 2 BLAS ROUTINE.
C
C  -- WRITTEN ON 22-OCTOBER-1986.
C     JACK DONGARRA, ARGONNE NATIONAL LAB.
C     JEREMY DU CROZ, NAG CENTRAL OFFICE.
C     SVEN HAMMARLING, NAG CENTRAL OFFICE.
C     RICHARD HANSON, SANDIA NATIONAL LABS.
C
C ======================================================================
C REMPLACE LA BLAS SAXPY SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      COMPLEX*16         ALPHA, BETA
      INTEGER            INCX, INCY, KL, KU, LDA, M, N
      CHARACTER*1        TRANS
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
C     .. PARAMETERS ..
      COMPLEX*16         ONE
      PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) )
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. LOCAL SCALARS ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KUP1, KX, KY,
     $                   LENX, LENY
      LOGICAL            NOCONJ
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF     ( .NOT.LLSAME( TRANS, 'N' ).AND.
     $         .NOT.LLSAME( TRANS, 'T' ).AND.
     $         .NOT.LLSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( KL.LT.0 )THEN
         INFO = 4
      ELSE IF( KU.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.( KL + KU + 1 ) )THEN
         INFO = 8
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 10
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'ZGBMV ', INFO )
         GOTO 9999
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   GOTO 9999
C
      NOCONJ = LLSAME( TRANS, 'T' )
C
C     SET  LENX  AND  LENY, THE LENGTHS OF THE VECTORS X AND Y, AND SET
C     UP THE START POINTS IN  X  AND  Y.
C
      IF( LLSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
C
C     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
C     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH THE BAND PART OF A.
C
C     FIRST FORM  Y := BETA*Y.
C
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   GOTO 9999
      KUP1 = KU + 1
      IF( LLSAME( TRANS, 'N' ) )THEN
C
C        FORM  Y := ALPHA*A*X + Y.
C
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  K    = KUP1 - J
                  DO 50, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( I ) = Y( I ) + TEMP*A( K + I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  K    = KUP1 - J
                  DO 70, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     Y( IY ) = Y( IY ) + TEMP*A( K + I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
               IF( J.GT.KU )
     $            KY = KY + INCY
   80       CONTINUE
         END IF
      ELSE
C
C        FORM  Y := ALPHA*A'*X + Y  OR  Y := ALPHA*CONJG( A' )*X + Y.
C
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 110, J = 1, N
               TEMP = ZERO
               K    = KUP1 - J
               IF( NOCONJ )THEN
                  DO 90, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + A( K + I, J )*X( I )
   90             CONTINUE
               ELSE
                  DO 100, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + DCONJG( A( K + I, J ) )*X( I )
  100             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  110       CONTINUE
         ELSE
            DO 140, J = 1, N
               TEMP = ZERO
               IX   = KX
               K    = KUP1 - J
               IF( NOCONJ )THEN
                  DO 120, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + A( K + I, J )*X( IX )
                     IX   = IX   + INCX
  120             CONTINUE
               ELSE
                  DO 130, I = MAX( 1, J - KU ), MIN( M, J + KL )
                     TEMP = TEMP + DCONJG( A( K + I, J ) )*X( IX )
                     IX   = IX   + INCX
  130             CONTINUE
               END IF
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
               IF( J.GT.KU )
     $            KX = KX + INCX
  140       CONTINUE
         END IF
      END IF
C
9999  CONTINUE
C
C     END OF GLGBMV .
C
      END
