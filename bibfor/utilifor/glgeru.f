      SUBROUTINE GLGERU ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C  PURPOSE
C  =======
C
C  GLGERU  PERFORMS THE RANK 1 OPERATION
C
C     A := ALPHA*X*Y' + A,
C
C  WHERE ALPHA IS A SCALAR, X IS AN M ELEMENT VECTOR, Y IS AN N ELEMENT
C  VECTOR AND A IS AN M BY N MATRIX.
C
C  PARAMETERS
C  ==========
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
C           M MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C          ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
C           N MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - COMPLEX*16      .
C           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
C           UNCHANGED ON EXIT.
C
C  X      - COMPLEX*16       ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCX ) ).
C           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE M
C           ELEMENT VECTOR X.
C           UNCHANGED ON EXIT.
C
C  INCX   - INTEGER.
C           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           X. INCX MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C  Y      - COMPLEX*16       ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCY ) ).
C           BEFORE ENTRY, THE INCREMENTED ARRAY Y MUST CONTAIN THE N
C           ELEMENT VECTOR Y.
C           UNCHANGED ON EXIT.
C
C  INCY   - INTEGER.
C           ON ENTRY, INCY SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           Y. INCY MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C  A      - COMPLEX*16       ARRAY OF DIMENSION ( LDA, N ).
C           BEFORE ENTRY, THE LEADING M BY N PART OF THE ARRAY A MUST
C           CONTAIN THE MATRIX OF COEFFICIENTS. ON EXIT, A IS
C           OVERWRITTEN BY THE UPDATED MATRIX.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
C           MAX( 1, M ).
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
C REMPLACE LA BLAS ZGERU SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      COMPLEX*16         ALPHA
      INTEGER            INCX, INCY, LDA, M, N
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         A( LDA, * ), X( * ), Y( * )
C     ..
C
C     .. PARAMETERS ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. LOCAL SCALARS ..
      COMPLEX*16         TEMP
      INTEGER            I, INFO, IX, J, JY, KX
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'GLGERU', INFO )
         GOTO 9999
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   GOTO 9999
C
C     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
C     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
C
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
C
9999  CONTINUE
C
C     END OF GLGERU .
C
      END
