      SUBROUTINE DLALN2( LTRANS, NA, NW, SMIN, CA, A, LDA, D1, D2, B,
     &                   LDB, WR, WI, X, LDX, SCALE, XNORM, INFO )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C 
C     SUBROUTINE LAPACK RESOLVANT UN SYSTEME LINEAIRE PERTURBE
C     PARTICULIER (DU TYPE MENTIONNE CI DESSOUS).
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C     ..
C
C  PURPOSE
C  =======
C
C  DLALN2 SOLVES A SYSTEM OF THE FORM  (CA A - W D ) X = S B
C  OR (CA A' - W D) X = S B   WITH POSSIBLE SCALING ("S") AND
C  PERTURBATION OF A.  (A' MEANS A-TRANSPOSE.)
C
C  A IS AN NA X NA REAL MATRIX, CA IS A REAL SCALAR, D IS AN NA X NA
C  REAL DIAGONAL MATRIX, W IS A REAL OR COMPLEX VALUE, AND X AND B ARE
C  NA X 1 MATRICES -- REAL IF W IS REAL, COMPLEX IF W IS COMPLEX.  NA
C  MAY BE 1 OR 2.
C
C  IF W IS COMPLEX, X AND B ARE REPRESENTED AS NA X 2 MATRICES,
C  THE FIRST COLUMN OF EACH BEING THE REAL PART AND THE SECOND
C  BEING THE IMAGINARY PART.
C
C  "S" IS A SCALING FACTOR (.LE. 1), COMPUTED BY DLALN2, WHICH IS
C  SO CHOSEN THAT X CAN BE COMPUTED WITHOUT OVERFLOW.  X IS FURTHER
C  SCALED IF NECESSARY TO ASSURE THAT NORM(CA A - W D)*NORM(X) IS LESS
C  THAN OVERFLOW.
C
C  IF BOTH SINGULAR VALUES OF (CA A - W D) ARE LESS THAN SMIN,
C  SMIN*IDENTITY WILL BE USED INSTEAD OF (CA A - W D).  IF ONLY ONE
C  SINGULAR VALUE IS LESS THAN SMIN, ONE ELEMENT OF (CA A - W D) WILL BE
C  PERTURBED ENOUGH TO MAKE THE SMALLEST SINGULAR VALUE ROUGHLY SMIN.
C  IF BOTH SINGULAR VALUES ARE AT LEAST SMIN, (CA A - W D) WILL NOT BE
C  PERTURBED.  IN ANY CASE, THE PERTURBATION WILL BE AT MOST SOME SMALL
C  MULTIPLE OF MAX( SMIN, ULP*NORM(CA A - W D) ).  THE SINGULAR VALUES
C  ARE COMPUTED BY INFINITY-NORM APPROXIMATIONS, AND THUS WILL ONLY BE
C  CORRECT TO A FACTOR OF 2 OR SO.
C
C  NOTE: ALL INPUT QUANTITIES ARE ASSUMED TO BE SMALLER THAN OVERFLOW
C  BY A REASONABLE FACTOR.  (SEE BIGNUM.)
C
C  ARGUMENTS
C  ==========
C
C  LTRANS  (INPUT) LOGICAL
C          =.TRUE.:  A-TRANSPOSE WILL BE USED.
C          =.FALSE.: A WILL BE USED (NOT TRANSPOSED.)
C
C  NA      (INPUT) INTEGER
C          THE SIZE OF THE MATRIX A.  IT MAY (ONLY) BE 1 OR 2.
C
C  NW      (INPUT) INTEGER
C          1 IF "W" IS REAL, 2 IF "W" IS COMPLEX.  IT MAY ONLY BE 1
C          OR 2.
C
C  SMIN    (INPUT) REAL*8
C          THE DESIRED LOWER BOUND ON THE SINGULAR VALUES OF A.  THIS
C          SHOULD BE A SAFE DISTANCE AWAY FROM UNDERFLOW OR OVERFLOW,
C          SAY, BETWEEN (UNDERFLOW/MACHINE PRECISION) AND  (MACHINE
C          PRECISION * OVERFLOW ).  (SEE BIGNUM AND ULP.)
C
C  CA      (INPUT) REAL*8
C          THE COEFFICIENT C, WHICH A IS MULTIPLIED BY.
C
C  A       (INPUT) REAL*8 ARRAY, DIMENSION (LDA,NA)
C          THE NA X NA MATRIX A.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF A.  IT MUST BE AT LEAST NA.
C
C  D1      (INPUT) REAL*8
C          THE 1,1 ELEMENT IN THE DIAGONAL MATRIX D.
C
C  D2      (INPUT) REAL*8
C          THE 2,2 ELEMENT IN THE DIAGONAL MATRIX D.  NOT USED IF NW=1.
C
C  B       (INPUT) REAL*8 ARRAY, DIMENSION (LDB,NW)
C          THE NA X NW MATRIX B (RIGHT-HAND SIDE).  IF NW=2 ("W" IS
C          COMPLEX), COLUMN 1 CONTAINS THE REAL PART OF B AND COLUMN 2
C          CONTAINS THE IMAGINARY PART.
C
C  LDB     (INPUT) INTEGER
C          THE LEADING DIMENSION OF B.  IT MUST BE AT LEAST NA.
C
C  WR      (INPUT) REAL*8
C          THE REAL PART OF THE SCALAR "W".
C
C  WI      (INPUT) REAL*8
C          THE IMAGINARY PART OF THE SCALAR "W".  NOT USED IF NW=1.
C
C  X       (OUTPUT) REAL*8 ARRAY, DIMENSION (LDX,NW)
C          THE NA X NW MATRIX X (UNKNOWNS), AS COMPUTED BY DLALN2.
C          IF NW=2 ("W" IS COMPLEX), ON EXIT, COLUMN 1 WILL CONTAIN
C          THE REAL PART OF X AND COLUMN 2 WILL CONTAIN THE IMAGINARY
C          PART.
C
C  LDX     (INPUT) INTEGER
C          THE LEADING DIMENSION OF X.  IT MUST BE AT LEAST NA.
C
C  SCALE   (OUTPUT) REAL*8
C          THE SCALE FACTOR THAT B MUST BE MULTIPLIED BY TO INSURE
C          THAT OVERFLOW DOES NOT OCCUR WHEN COMPUTING X.  THUS,
C          (CA A - W D) X  WILL BE SCALE*B, NOT B (IGNORING
C          PERTURBATIONS OF A.)  IT WILL BE AT MOST 1.
C
C  XNORM   (OUTPUT) REAL*8
C          THE INFINITY-NORM OF X, WHEN X IS REGARDED AS AN NA X NW
C          REAL MATRIX.
C
C  INFO    (OUTPUT) INTEGER
C          AN ERROR FLAG.  IT WILL BE SET TO ZERO IF NO ERROR OCCURS,
C          A NEGATIVE NUMBER IF AN ARGUMENT IS IN ERROR, OR A POSITIVE
C          NUMBER IF  CA A - W D  HAD TO BE PERTURBED.
C          THE POSSIBLE VALUES ARE:
C          = 0: NO ERROR OCCURRED, AND (CA A - W D) DID NOT HAVE TO BE
C                 PERTURBED.
C          = 1: (CA A - W D) HAD TO BE PERTURBED TO MAKE ITS SMALLEST
C               (OR ONLY) SINGULAR VALUE GREATER THAN SMIN.
C          NOTE: IN THE INTERESTS OF SPEED, THIS ROUTINE DOES NOT
C                CHECK THE INPUTS FOR ERRORS.
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE DLAMCH REMPLACE PAR R8MIEM(),
C            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
C            DISPARITION DE L'EQUIVALENCE DUE TO CRP18,
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C   ABS, MAX
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      LOGICAL            LTRANS
      INTEGER            INFO, LDA, LDB, LDX, NA, NW
      REAL*8   CA, D1, D2, SCALE, SMIN, WI, WR, XNORM
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   A( LDA, * ), B( LDB, * ), X( LDX, * )
      
C     .. PARAMETERS ..
      REAL*8   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
      REAL*8   TWO
      PARAMETER          ( TWO = 2.0D0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            ICMAX, J
      REAL*8   BBND, BI1, BI2, BIGNUM, BNORM, BR1, BR2, CI21,
     &                   CI22, CMAX, CNORM, CR21, CR22, CSI, CSR, LI21,
     &                   LR21, SMINI, SMLNUM, TEMP, U22ABS, UI11, UI11R,
     &                   UI12, UI12S, UI22, UR11, UR11R, UR12, UR12S,
     &                   UR22, XI1, XI2, XR1, XR2
C     ..
C     .. LOCAL ARRAYS ..
      LOGICAL            RSWAP( 4 ), ZSWAP( 4 )
      INTEGER            IPIVOT( 4, 4 )
      REAL*8   CI( 2, 2 ), CIV( 4 ), CR( 2, 2 ), CRV( 4 )
C     ..
C     .. EXTERNAL FUNCTIONS ..
      REAL*8   R8MIEM
C
C DUE TO CRP_18     ..
C     .. EQUIVALENCES ..
C      EQUIVALENCE        ( CI( 1, 1 ), CIV( 1 ) ),
C     &                   ( CR( 1, 1 ), CRV( 1 ) )
C     ..
C     .. DATA STATEMENTS ..
      DATA               ZSWAP / .FALSE., .FALSE., .TRUE., .TRUE. /
      DATA               RSWAP / .FALSE., .TRUE., .FALSE., .TRUE. /
      DATA               IPIVOT / 1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4,
     &                   3, 2, 1 /
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     COMPUTE BIGNUM
C
      SMLNUM = TWO*R8MIEM()
      BIGNUM = ONE / SMLNUM
      SMINI = MAX( SMIN, SMLNUM )
C
C     DON'T CHECK FOR INPUT ERRORS
C
      INFO = 0
C
C     STANDARD INITIALIZATIONS
C
      SCALE = ONE
C
      IF( NA.EQ.1 ) THEN
C
C        1 X 1  (I.E., SCALAR) SYSTEM   C X = B
C
         IF( NW.EQ.1 ) THEN
C
C           REAL 1X1 SYSTEM.
C
C           C = CA A - W D
C
            CSR = CA*A( 1, 1 ) - WR*D1
            CNORM = ABS( CSR )
C
C           IF | C | < SMINI, USE C = SMINI
C
            IF( CNORM.LT.SMINI ) THEN
               CSR = SMINI
               CNORM = SMINI
               INFO = 1
            END IF
C
C           CHECK SCALING FOR  X = B / C
C
            BNORM = ABS( B( 1, 1 ) )
            IF( CNORM.LT.ONE .AND. BNORM.GT.ONE ) THEN
               IF( BNORM.GT.BIGNUM*CNORM )
     &            SCALE = ONE / BNORM
            END IF
C
C           COMPUTE X
C
            X( 1, 1 ) = ( B( 1, 1 )*SCALE ) / CSR
            XNORM = ABS( X( 1, 1 ) )
         ELSE
C
C           COMPLEX 1X1 SYSTEM (W IS COMPLEX)
C
C           C = CA A - W D
C
            CSR = CA*A( 1, 1 ) - WR*D1
            CSI = -WI*D1
            CNORM = ABS( CSR ) + ABS( CSI )
C
C           IF | C | < SMINI, USE C = SMINI
C
            IF( CNORM.LT.SMINI ) THEN
               CSR = SMINI
               CSI = ZERO
               CNORM = SMINI
               INFO = 1
            END IF
C
C           CHECK SCALING FOR  X = B / C
C
            BNORM = ABS( B( 1, 1 ) ) + ABS( B( 1, 2 ) )
            IF( CNORM.LT.ONE .AND. BNORM.GT.ONE ) THEN
               IF( BNORM.GT.BIGNUM*CNORM )
     &            SCALE = ONE / BNORM
            END IF
C
C           COMPUTE X
C
            CALL DLADIV( SCALE*B( 1, 1 ), SCALE*B( 1, 2 ), CSR, CSI,
     &                   X( 1, 1 ), X( 1, 2 ) )
            XNORM = ABS( X( 1, 1 ) ) + ABS( X( 1, 2 ) )
         END IF
C
      ELSE
C
C        2X2 SYSTEM
C
C        COMPUTE THE REAL PART OF  C = CA A - W D  (OR  CA A' - W D )
C
         CR( 1, 1 ) = CA*A( 1, 1 ) - WR*D1
         CR( 2, 2 ) = CA*A( 2, 2 ) - WR*D2
         IF( LTRANS ) THEN
            CR( 1, 2 ) = CA*A( 2, 1 )
            CR( 2, 1 ) = CA*A( 1, 2 )
         ELSE
            CR( 2, 1 ) = CA*A( 2, 1 )
            CR( 1, 2 ) = CA*A( 1, 2 )
         END IF
         
C DUE TO CRP_18
         CRV(1) = CR(1,1)
         CRV(2) = CR(2,1)
         CRV(3) = CR(1,2)
         CRV(4) = CR(2,2)
C
         IF( NW.EQ.1 ) THEN
C
C           REAL 2X2 SYSTEM  (W IS REAL)
C
C           FIND THE LARGEST ELEMENT IN C
C
            CMAX = ZERO
            ICMAX = 0
C
            DO 10 J = 1, 4
               IF( ABS( CRV( J ) ).GT.CMAX ) THEN
                  CMAX = ABS( CRV( J ) )
                  ICMAX = J
               END IF
   10       CONTINUE
C
C           IF NORM(C) < SMINI, USE SMINI*IDENTITY.
C
            IF( CMAX.LT.SMINI ) THEN
               BNORM = MAX( ABS( B( 1, 1 ) ), ABS( B( 2, 1 ) ) )
               IF( SMINI.LT.ONE .AND. BNORM.GT.ONE ) THEN
                  IF( BNORM.GT.BIGNUM*SMINI )
     &               SCALE = ONE / BNORM
               END IF
               TEMP = SCALE / SMINI
               X( 1, 1 ) = TEMP*B( 1, 1 )
               X( 2, 1 ) = TEMP*B( 2, 1 )
               XNORM = TEMP*BNORM
               INFO = 1
               GOTO 1000
            END IF
C
C           GAUSSIAN ELIMINATION WITH COMPLETE PIVOTING.
C
            UR11 = CRV( ICMAX )
            CR21 = CRV( IPIVOT( 2, ICMAX ) )
            UR12 = CRV( IPIVOT( 3, ICMAX ) )
            CR22 = CRV( IPIVOT( 4, ICMAX ) )
            UR11R = ONE / UR11
            LR21 = UR11R*CR21
            UR22 = CR22 - UR12*LR21
C
C           IF SMALLER PIVOT < SMINI, USE SMINI
C
            IF( ABS( UR22 ).LT.SMINI ) THEN
               UR22 = SMINI
               INFO = 1
            END IF
            IF( RSWAP( ICMAX ) ) THEN
               BR1 = B( 2, 1 )
               BR2 = B( 1, 1 )
            ELSE
               BR1 = B( 1, 1 )
               BR2 = B( 2, 1 )
            END IF
            BR2 = BR2 - LR21*BR1
            BBND = MAX( ABS( BR1*( UR22*UR11R ) ), ABS( BR2 ) )
            IF( BBND.GT.ONE .AND. ABS( UR22 ).LT.ONE ) THEN
               IF( BBND.GE.BIGNUM*ABS( UR22 ) )
     &            SCALE = ONE / BBND
            END IF
C
            XR2 = ( BR2*SCALE ) / UR22
            XR1 = ( SCALE*BR1 )*UR11R - XR2*( UR11R*UR12 )
            IF( ZSWAP( ICMAX ) ) THEN
               X( 1, 1 ) = XR2
               X( 2, 1 ) = XR1
            ELSE
               X( 1, 1 ) = XR1
               X( 2, 1 ) = XR2
            END IF
            XNORM = MAX( ABS( XR1 ), ABS( XR2 ) )
C
C           FURTHER SCALING IF  NORM(A) NORM(X) > OVERFLOW
C
            IF( XNORM.GT.ONE .AND. CMAX.GT.ONE ) THEN
               IF( XNORM.GT.BIGNUM / CMAX ) THEN
                  TEMP = CMAX / BIGNUM
                  X( 1, 1 ) = TEMP*X( 1, 1 )
                  X( 2, 1 ) = TEMP*X( 2, 1 )
                  XNORM = TEMP*XNORM
                  SCALE = TEMP*SCALE
               END IF
            END IF
         ELSE
C
C           COMPLEX 2X2 SYSTEM  (W IS COMPLEX)
C
C           FIND THE LARGEST ELEMENT IN C
C
            CI( 1, 1 ) = -WI*D1
            CI( 2, 1 ) = ZERO
            CI( 1, 2 ) = ZERO
            CI( 2, 2 ) = -WI*D2
            CMAX = ZERO
            ICMAX = 0
C DUE TO CRP_18
            CIV(1) = CI(1,1)
            CIV(2) = CI(2,1)
            CIV(3) = CI(1,2)
            CIV(4) = CI(2,2)
C
            DO 20 J = 1, 4
               IF( ABS( CRV( J ) )+ABS( CIV( J ) ).GT.CMAX ) THEN
                  CMAX = ABS( CRV( J ) ) + ABS( CIV( J ) )
                  ICMAX = J
               END IF
   20       CONTINUE
C
C           IF NORM(C) < SMINI, USE SMINI*IDENTITY.
C
            IF( CMAX.LT.SMINI ) THEN
               BNORM = MAX( ABS( B( 1, 1 ) )+ABS( B( 1, 2 ) ),
     &                 ABS( B( 2, 1 ) )+ABS( B( 2, 2 ) ) )
               IF( SMINI.LT.ONE .AND. BNORM.GT.ONE ) THEN
                  IF( BNORM.GT.BIGNUM*SMINI )
     &               SCALE = ONE / BNORM
               END IF
               TEMP = SCALE / SMINI
               X( 1, 1 ) = TEMP*B( 1, 1 )
               X( 2, 1 ) = TEMP*B( 2, 1 )
               X( 1, 2 ) = TEMP*B( 1, 2 )
               X( 2, 2 ) = TEMP*B( 2, 2 )
               XNORM = TEMP*BNORM
               INFO = 1
               GOTO 1000
            END IF
C
C           GAUSSIAN ELIMINATION WITH COMPLETE PIVOTING.
C
            UR11 = CRV( ICMAX )
            UI11 = CIV( ICMAX )
            CR21 = CRV( IPIVOT( 2, ICMAX ) )
            CI21 = CIV( IPIVOT( 2, ICMAX ) )
            UR12 = CRV( IPIVOT( 3, ICMAX ) )
            UI12 = CIV( IPIVOT( 3, ICMAX ) )
            CR22 = CRV( IPIVOT( 4, ICMAX ) )
            CI22 = CIV( IPIVOT( 4, ICMAX ) )
            IF( ICMAX.EQ.1 .OR. ICMAX.EQ.4 ) THEN
C
C              CODE WHEN OFF-DIAGONALS OF PIVOTED C ARE REAL
C
               IF( ABS( UR11 ).GT.ABS( UI11 ) ) THEN
                  TEMP = UI11 / UR11
                  UR11R = ONE / ( UR11*( ONE+TEMP**2 ) )
                  UI11R = -TEMP*UR11R
               ELSE
                  TEMP = UR11 / UI11
                  UI11R = -ONE / ( UI11*( ONE+TEMP**2 ) )
                  UR11R = -TEMP*UI11R
               END IF
               LR21 = CR21*UR11R
               LI21 = CR21*UI11R
               UR12S = UR12*UR11R
               UI12S = UR12*UI11R
               UR22 = CR22 - UR12*LR21
               UI22 = CI22 - UR12*LI21
            ELSE
C
C              CODE WHEN DIAGONALS OF PIVOTED C ARE REAL
C
               UR11R = ONE / UR11
               UI11R = ZERO
               LR21 = CR21*UR11R
               LI21 = CI21*UR11R
               UR12S = UR12*UR11R
               UI12S = UI12*UR11R
               UR22 = CR22 - UR12*LR21 + UI12*LI21
               UI22 = -UR12*LI21 - UI12*LR21
            END IF
            U22ABS = ABS( UR22 ) + ABS( UI22 )
C
C           IF SMALLER PIVOT < SMINI, USE SMINI
C
            IF( U22ABS.LT.SMINI ) THEN
               UR22 = SMINI
               UI22 = ZERO
               INFO = 1
            END IF
            IF( RSWAP( ICMAX ) ) THEN
               BR2 = B( 1, 1 )
               BR1 = B( 2, 1 )
               BI2 = B( 1, 2 )
               BI1 = B( 2, 2 )
            ELSE
               BR1 = B( 1, 1 )
               BR2 = B( 2, 1 )
               BI1 = B( 1, 2 )
               BI2 = B( 2, 2 )
            END IF
            BR2 = BR2 - LR21*BR1 + LI21*BI1
            BI2 = BI2 - LI21*BR1 - LR21*BI1
            BBND = MAX( ( ABS( BR1 )+ABS( BI1 ) )*
     &             ( U22ABS*( ABS( UR11R )+ABS( UI11R ) ) ),
     &             ABS( BR2 )+ABS( BI2 ) )
            IF( BBND.GT.ONE .AND. U22ABS.LT.ONE ) THEN
               IF( BBND.GE.BIGNUM*U22ABS ) THEN
                  SCALE = ONE / BBND
                  BR1 = SCALE*BR1
                  BI1 = SCALE*BI1
                  BR2 = SCALE*BR2
                  BI2 = SCALE*BI2
               END IF
            END IF
C
            CALL DLADIV( BR2, BI2, UR22, UI22, XR2, XI2 )
            XR1 = UR11R*BR1 - UI11R*BI1 - UR12S*XR2 + UI12S*XI2
            XI1 = UI11R*BR1 + UR11R*BI1 - UI12S*XR2 - UR12S*XI2
            IF( ZSWAP( ICMAX ) ) THEN
               X( 1, 1 ) = XR2
               X( 2, 1 ) = XR1
               X( 1, 2 ) = XI2
               X( 2, 2 ) = XI1
            ELSE
               X( 1, 1 ) = XR1
               X( 2, 1 ) = XR2
               X( 1, 2 ) = XI1
               X( 2, 2 ) = XI2
            END IF
            XNORM = MAX( ABS( XR1 )+ABS( XI1 ), ABS( XR2 )+ABS( XI2 ) )
C
C           FURTHER SCALING IF  NORM(A) NORM(X) > OVERFLOW
C
            IF( XNORM.GT.ONE .AND. CMAX.GT.ONE ) THEN
               IF( XNORM.GT.BIGNUM / CMAX ) THEN
                  TEMP = CMAX / BIGNUM
                  X( 1, 1 ) = TEMP*X( 1, 1 )
                  X( 2, 1 ) = TEMP*X( 2, 1 )
                  X( 1, 2 ) = TEMP*X( 1, 2 )
                  X( 2, 2 ) = TEMP*X( 2, 2 )
                  XNORM = TEMP*XNORM
                  SCALE = TEMP*SCALE
               END IF
            END IF
         END IF
      END IF
C
 1000 CONTINUE
C
C     END OF DLALN2
C
      END
