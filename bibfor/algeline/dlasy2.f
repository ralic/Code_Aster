      SUBROUTINE DLASY2( LTRANL, LTRANR, ISGN, N1, N2, TL, LDTL, TR,
     &                   LDTR, B, LDB, SCALE, X, LDX, XNORM, INFO )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/02/2000   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_21
C 
C     SUBROUTINE LAPACK RESOLVANT L'EQUATION MATRICIELLE CI-DESSOUS.
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C
C  PURPOSE
C  =======
C
C  DLASY2 SOLVES FOR THE N1 BY N2 MATRIX X, 1 <= N1,N2 <= 2, IN
C
C         OP(TL)*X + ISGN*X*OP(TR) = SCALE*B,
C
C  WHERE TL IS N1 BY N1, TR IS N2 BY N2, B IS N1 BY N2, AND ISGN = 1 OR
C  -1.  OP(T) = T OR T', WHERE T' DENOTES THE TRANSPOSE OF T.
C
C  ARGUMENTS
C  =========
C
C  LTRANL  (INPUT) LOGICAL
C          ON ENTRY, LTRANL SPECIFIES THE OP(TL):
C             = .FALSE., OP(TL) = TL,
C             = .TRUE., OP(TL) = TL'.
C
C  LTRANR  (INPUT) LOGICAL
C          ON ENTRY, LTRANR SPECIFIES THE OP(TR):
C            = .FALSE., OP(TR) = TR,
C            = .TRUE., OP(TR) = TR'.
C
C  ISGN    (INPUT) INTEGER
C          ON ENTRY, ISGN SPECIFIES THE SIGN OF THE EQUATION
C          AS DESCRIBED BEFORE. ISGN MAY ONLY BE 1 OR -1.
C
C  N1      (INPUT) INTEGER
C          ON ENTRY, N1 SPECIFIES THE ORDER OF MATRIX TL.
C          N1 MAY ONLY BE 0, 1 OR 2.
C
C  N2      (INPUT) INTEGER
C          ON ENTRY, N2 SPECIFIES THE ORDER OF MATRIX TR.
C          N2 MAY ONLY BE 0, 1 OR 2.
C
C  TL      (INPUT) REAL*8 ARRAY, DIMENSION (LDTL,2)
C          ON ENTRY, TL CONTAINS AN N1 BY N1 MATRIX.
C
C  LDTL    (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE MATRIX TL. LDTL >= MAX(1,N1).
C
C  TR      (INPUT) REAL*8 ARRAY, DIMENSION (LDTR,2)
C          ON ENTRY, TR CONTAINS AN N2 BY N2 MATRIX.
C
C  LDTR    (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE MATRIX TR. LDTR >= MAX(1,N2).
C
C  B       (INPUT) REAL*8 ARRAY, DIMENSION (LDB,2)
C          ON ENTRY, THE N1 BY N2 MATRIX B CONTAINS THE RIGHT-HAND
C          SIDE OF THE EQUATION.
C
C  LDB     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE MATRIX B. LDB >= MAX(1,N1).
C
C  SCALE   (OUTPUT) REAL*8
C          ON EXIT, SCALE CONTAINS THE SCALE FACTOR. SCALE IS CHOSEN
C          LESS THAN OR EQUAL TO 1 TO PREVENT THE SOLUTION OVERFLOWING.
C
C  X       (OUTPUT) REAL*8 ARRAY, DIMENSION (LDX,2)
C          ON EXIT, X CONTAINS THE N1 BY N2 SOLUTION.
C
C  LDX     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE MATRIX X. LDX >= MAX(1,N1).
C
C  XNORM   (OUTPUT) REAL*8
C          ON EXIT, XNORM IS THE INFINITY-NORM OF THE SOLUTION.
C
C  INFO    (OUTPUT) INTEGER
C          ON EXIT, INFO IS SET TO
C             0: SUCCESSFUL EXIT.
C             1: TL AND TR HAVE TOO CLOSE EIGENVALUES, SO TL OR
C                TR IS PERTURBED TO GET A NONSINGULAR EQUATION.
C          NOTE: IN THE INTERESTS OF SPEED, THIS ROUTINE DOES NOT
C                CHECK THE INPUTS FOR ERRORS.
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
      LOGICAL            LTRANL, LTRANR
      INTEGER            INFO, ISGN, LDB, LDTL, LDTR, LDX, N1, N2
      REAL*8   SCALE, XNORM
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   B( LDB, * ), TL( LDTL, * ), TR( LDTR, * ),
     &                   X( LDX, * )
C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      REAL*8   TWO, HALF, EIGHT
      PARAMETER          ( TWO = 2.0D+0, HALF = 0.5D+0, EIGHT = 8.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            BSWAP, XSWAP
      INTEGER            I, IP, IPIV, IPSV, J, JP, JPSV, K
      REAL*8   BET, EPS, GAM, L21, SGN, SMIN, SMLNUM, TAU1,
     &                   TEMP, U11, U12, U22, XMAX
C     ..
C     .. LOCAL ARRAYS ..
      LOGICAL            BSWPIV( 4 ), XSWPIV( 4 )
      INTEGER            JPIV( 4 ), LOCL21( 4 ), LOCU12( 4 ),
     &                   LOCU22( 4 )
      REAL*8   BTMP( 4 ), T16( 4, 4 ), TMP( 4 ), X2( 2 )
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER  IDAMAX, ISBAEM
      REAL*8   R8PREM, R8MIEM
C     ..
C     .. DATA STATEMENTS ..
      DATA               LOCU12 / 3, 4, 1, 2 / , LOCL21 / 2, 1, 4, 3 / ,
     &                   LOCU22 / 4, 3, 2, 1 /
      DATA               XSWPIV / .FALSE., .FALSE., .TRUE., .TRUE. /
      DATA               BSWPIV / .FALSE., .TRUE., .FALSE., .TRUE. /
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     DO NOT CHECK THE INPUT PARAMETERS FOR ERRORS
C
      INFO = 0
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N1.EQ.0 .OR. N2.EQ.0 )
     &   GOTO 1000
C
C     SET CONSTANTS TO CONTROL OVERFLOW
C
      EPS = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = R8MIEM() / EPS
      SGN = ISGN
C
      K = N1 + N1 + N2 - 2
      GO TO ( 10, 20, 30, 50 )K
C
C     1 BY 1: TL11*X + SGN*X*TR11 = B11
C
   10 CONTINUE
      TAU1 = TL( 1, 1 ) + SGN*TR( 1, 1 )
      BET = ABS( TAU1 )
      IF( BET.LE.SMLNUM ) THEN
         TAU1 = SMLNUM
         BET = SMLNUM
         INFO = 1
      END IF
C
      SCALE = ONE
      GAM = ABS( B( 1, 1 ) )
      IF( SMLNUM*GAM.GT.BET )
     &   SCALE = ONE / GAM
C
      X( 1, 1 ) = ( B( 1, 1 )*SCALE ) / TAU1
      XNORM = ABS( X( 1, 1 ) )
      GOTO 1000
C
C     1 BY 2:
C     TL11*(X11 X12) + ISGN*(X11 X12)*OP(TR11 TR12)  = (B11 B12)
C                                       (TR21 TR22)
C
   20 CONTINUE
C
      SMIN = MAX( EPS*MAX( ABS( TL( 1, 1 ) ), ABS( TR( 1, 1 ) ),
     &       ABS( TR( 1, 2 ) ), ABS( TR( 2, 1 ) ), ABS( TR( 2, 2 ) ) ),
     &       SMLNUM )
      TMP( 1 ) = TL( 1, 1 ) + SGN*TR( 1, 1 )
      TMP( 4 ) = TL( 1, 1 ) + SGN*TR( 2, 2 )
      IF( LTRANR ) THEN
         TMP( 2 ) = SGN*TR( 2, 1 )
         TMP( 3 ) = SGN*TR( 1, 2 )
      ELSE
         TMP( 2 ) = SGN*TR( 1, 2 )
         TMP( 3 ) = SGN*TR( 2, 1 )
      END IF
      BTMP( 1 ) = B( 1, 1 )
      BTMP( 2 ) = B( 1, 2 )
      GO TO 40
C
C     2 BY 1:
C          OP(TL11 TL12)*(X11) + ISGN* (X11)*TR11  = (B11)
C            (TL21 TL22) (X21)         (X21)         (B21)
C
   30 CONTINUE
      SMIN = MAX( EPS*MAX( ABS( TR( 1, 1 ) ), ABS( TL( 1, 1 ) ),
     &       ABS( TL( 1, 2 ) ), ABS( TL( 2, 1 ) ), ABS( TL( 2, 2 ) ) ),
     &       SMLNUM )
      TMP( 1 ) = TL( 1, 1 ) + SGN*TR( 1, 1 )
      TMP( 4 ) = TL( 2, 2 ) + SGN*TR( 1, 1 )
      IF( LTRANL ) THEN
         TMP( 2 ) = TL( 1, 2 )
         TMP( 3 ) = TL( 2, 1 )
      ELSE
         TMP( 2 ) = TL( 2, 1 )
         TMP( 3 ) = TL( 1, 2 )
      END IF
      BTMP( 1 ) = B( 1, 1 )
      BTMP( 2 ) = B( 2, 1 )
   40 CONTINUE
C
C     SOLVE 2 BY 2 SYSTEM USING COMPLETE PIVOTING.
C     SET PIVOTS LESS THAN SMIN TO SMIN.
C
      IPIV = IDAMAX( 4, TMP, 1 )
      U11 = TMP( IPIV )
      IF( ABS( U11 ).LE.SMIN ) THEN
         INFO = 1
         U11 = SMIN
      END IF
      U12 = TMP( LOCU12( IPIV ) )
      L21 = TMP( LOCL21( IPIV ) ) / U11
      U22 = TMP( LOCU22( IPIV ) ) - U12*L21
      XSWAP = XSWPIV( IPIV )
      BSWAP = BSWPIV( IPIV )
      IF( ABS( U22 ).LE.SMIN ) THEN
         INFO = 1
         U22 = SMIN
      END IF
      IF( BSWAP ) THEN
         TEMP = BTMP( 2 )
         BTMP( 2 ) = BTMP( 1 ) - L21*TEMP
         BTMP( 1 ) = TEMP
      ELSE
         BTMP( 2 ) = BTMP( 2 ) - L21*BTMP( 1 )
      END IF
      SCALE = ONE
      IF( ( TWO*SMLNUM )*ABS( BTMP( 2 ) ).GT.ABS( U22 ) .OR.
     &    ( TWO*SMLNUM )*ABS( BTMP( 1 ) ).GT.ABS( U11 ) ) THEN
         SCALE = HALF / MAX( ABS( BTMP( 1 ) ), ABS( BTMP( 2 ) ) )
         BTMP( 1 ) = BTMP( 1 )*SCALE
         BTMP( 2 ) = BTMP( 2 )*SCALE
      END IF
      X2( 2 ) = BTMP( 2 ) / U22
      X2( 1 ) = BTMP( 1 ) / U11 - ( U12 / U11 )*X2( 2 )
      IF( XSWAP ) THEN
         TEMP = X2( 2 )
         X2( 2 ) = X2( 1 )
         X2( 1 ) = TEMP
      END IF
      X( 1, 1 ) = X2( 1 )
      IF( N1.EQ.1 ) THEN
         X( 1, 2 ) = X2( 2 )
         XNORM = ABS( X( 1, 1 ) ) + ABS( X( 1, 2 ) )
      ELSE
         X( 2, 1 ) = X2( 2 )
         XNORM = MAX( ABS( X( 1, 1 ) ), ABS( X( 2, 1 ) ) )
      END IF
      GOTO 1000
C
C     2 BY 2:
C     OP(TL11 TL12)*(X11 X12) +ISGN* (X11 X12)*OP(TR11 TR12) = (B11 B12)
C       (TL21 TL22) (X21 X22)        (X21 X22)   (TR21 TR22)   (B21 B22)
C
C     SOLVE EQUIVALENT 4 BY 4 SYSTEM USING COMPLETE PIVOTING.
C     SET PIVOTS LESS THAN SMIN TO SMIN.
C
   50 CONTINUE
      SMIN = MAX( ABS( TR( 1, 1 ) ), ABS( TR( 1, 2 ) ),
     &       ABS( TR( 2, 1 ) ), ABS( TR( 2, 2 ) ) )
      SMIN = MAX( SMIN, ABS( TL( 1, 1 ) ), ABS( TL( 1, 2 ) ),
     &       ABS( TL( 2, 1 ) ), ABS( TL( 2, 2 ) ) )
      SMIN = MAX( EPS*SMIN, SMLNUM )
      BTMP( 1 ) = ZERO
      CALL BLCOPY( 16, BTMP, 0, T16, 1 )
      T16( 1, 1 ) = TL( 1, 1 ) + SGN*TR( 1, 1 )
      T16( 2, 2 ) = TL( 2, 2 ) + SGN*TR( 1, 1 )
      T16( 3, 3 ) = TL( 1, 1 ) + SGN*TR( 2, 2 )
      T16( 4, 4 ) = TL( 2, 2 ) + SGN*TR( 2, 2 )
      IF( LTRANL ) THEN
         T16( 1, 2 ) = TL( 2, 1 )
         T16( 2, 1 ) = TL( 1, 2 )
         T16( 3, 4 ) = TL( 2, 1 )
         T16( 4, 3 ) = TL( 1, 2 )
      ELSE
         T16( 1, 2 ) = TL( 1, 2 )
         T16( 2, 1 ) = TL( 2, 1 )
         T16( 3, 4 ) = TL( 1, 2 )
         T16( 4, 3 ) = TL( 2, 1 )
      END IF
      IF( LTRANR ) THEN
         T16( 1, 3 ) = SGN*TR( 1, 2 )
         T16( 2, 4 ) = SGN*TR( 1, 2 )
         T16( 3, 1 ) = SGN*TR( 2, 1 )
         T16( 4, 2 ) = SGN*TR( 2, 1 )
      ELSE
         T16( 1, 3 ) = SGN*TR( 2, 1 )
         T16( 2, 4 ) = SGN*TR( 2, 1 )
         T16( 3, 1 ) = SGN*TR( 1, 2 )
         T16( 4, 2 ) = SGN*TR( 1, 2 )
      END IF
      BTMP( 1 ) = B( 1, 1 )
      BTMP( 2 ) = B( 2, 1 )
      BTMP( 3 ) = B( 1, 2 )
      BTMP( 4 ) = B( 2, 2 )
C
C     PERFORM ELIMINATION
C
      DO 100 I = 1, 3
         XMAX = ZERO
         DO 70 IP = I, 4
            DO 60 JP = I, 4
               IF( ABS( T16( IP, JP ) ).GE.XMAX ) THEN
                  XMAX = ABS( T16( IP, JP ) )
                  IPSV = IP
                  JPSV = JP
               END IF
   60       CONTINUE
   70    CONTINUE
         IF( IPSV.NE.I ) THEN
            CALL BLSWAP( 4, T16( IPSV, 1 ), 4, T16( I, 1 ), 4 )
            TEMP = BTMP( I )
            BTMP( I ) = BTMP( IPSV )
            BTMP( IPSV ) = TEMP
         END IF
         IF( JPSV.NE.I )
     &      CALL BLSWAP( 4, T16( 1, JPSV ), 1, T16( 1, I ), 1 )
         JPIV( I ) = JPSV
         IF( ABS( T16( I, I ) ).LT.SMIN ) THEN
            INFO = 1
            T16( I, I ) = SMIN
         END IF
         DO 90 J = I + 1, 4
            T16( J, I ) = T16( J, I ) / T16( I, I )
            BTMP( J ) = BTMP( J ) - T16( J, I )*BTMP( I )
            DO 80 K = I + 1, 4
               T16( J, K ) = T16( J, K ) - T16( J, I )*T16( I, K )
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
      IF( ABS( T16( 4, 4 ) ).LT.SMIN )
     &   T16( 4, 4 ) = SMIN
      SCALE = ONE
      IF( ( EIGHT*SMLNUM )*ABS( BTMP( 1 ) ).GT.ABS( T16( 1, 1 ) ) .OR.
     &    ( EIGHT*SMLNUM )*ABS( BTMP( 2 ) ).GT.ABS( T16( 2, 2 ) ) .OR.
     &    ( EIGHT*SMLNUM )*ABS( BTMP( 3 ) ).GT.ABS( T16( 3, 3 ) ) .OR.
     &    ( EIGHT*SMLNUM )*ABS( BTMP( 4 ) ).GT.ABS( T16( 4, 4 ) ) ) THEN
         SCALE = ( ONE / EIGHT ) / MAX( ABS( BTMP( 1 ) ),
     &           ABS( BTMP( 2 ) ), ABS( BTMP( 3 ) ), ABS( BTMP( 4 ) ) )
         BTMP( 1 ) = BTMP( 1 )*SCALE
         BTMP( 2 ) = BTMP( 2 )*SCALE
         BTMP( 3 ) = BTMP( 3 )*SCALE
         BTMP( 4 ) = BTMP( 4 )*SCALE
      END IF
      DO 120 I = 1, 4
         K = 5 - I
         TEMP = ONE / T16( K, K )
         TMP( K ) = BTMP( K )*TEMP
         DO 110 J = K + 1, 4
            TMP( K ) = TMP( K ) - ( TEMP*T16( K, J ) )*TMP( J )
  110    CONTINUE
  120 CONTINUE
      DO 130 I = 1, 3
         IF( JPIV( 4-I ).NE.4-I ) THEN
            TEMP = TMP( 4-I )
            TMP( 4-I ) = TMP( JPIV( 4-I ) )
            TMP( JPIV( 4-I ) ) = TEMP
         END IF
  130 CONTINUE
      X( 1, 1 ) = TMP( 1 )
      X( 2, 1 ) = TMP( 2 )
      X( 1, 2 ) = TMP( 3 )
      X( 2, 2 ) = TMP( 4 )
      XNORM = MAX( ABS( TMP( 1 ) )+ABS( TMP( 3 ) ),
     &        ABS( TMP( 2 ) )+ABS( TMP( 4 ) ) )
 1000 CONTINUE
C
C     END OF DLASY2
C
      END
