      SUBROUTINE FLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
     &                   ILOZ, IHIZ, Z, LDZ, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 31/01/2005   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK DE MISE A JOUR DES VALEURS PROPRES ET DE LA
C     DECOMPOSITION DE SCHUR DEJA CALCULEES PAR DHSEQR.
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C  PURPOSE
C  =======
C
C  FLAHQR IS AN AUXILIARY ROUTINE CALLED BY DHSEQR TO UPDATE THE
C  EIGENVALUES AND SCHUR DECOMPOSITION ALREADY COMPUTED BY DHSEQR, BY
C  DEALING WITH THE HESSENBERG SUBMATRIX IN ROWS AND COLUMNS ILO TO IHI.
C
C  ARGUMENTS
C  =========
C
C  WANTT   (INPUT) LOGICAL
C          = .TRUE. : THE FULL SCHUR FORM T IS REQUIRED,
C          = .FALSE.: ONLY EIGENVALUES ARE REQUIRED.
C
C  WANTZ   (INPUT) LOGICAL
C          = .TRUE. : THE MATRIX OF SCHUR VECTORS Z IS REQUIRED,
C          = .FALSE.: SCHUR VECTORS ARE NOT REQUIRED.
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX H.  N >= 0.
C
C  ILO     (INPUT) INTEGER
C  IHI     (INPUT) INTEGER
C          IT IS ASSUMED THAT H IS ALREADY UPPER QUASI-TRIANGULAR IN
C          ROWS AND COLUMNS IHI+1:N, AND THAT H(ILO,ILO-1) = 0 (UNLESS
C          ILO = 1). FLAHQR WORKS PRIMARILY WITH THE HESSENBERG
C          SUBMATRIX IN ROWS AND COLUMNS ILO TO IHI, BUT APPLIES
C          TRANSFORMATIONS TO ALL OF H IF WANTT IS .TRUE..
C          1 <= ILO <= MAX(1,IHI), IHI <= N.
C
C  H       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDH,N)
C          ON ENTRY, THE UPPER HESSENBERG MATRIX H.
C          ON EXIT, IF WANTT IS .TRUE., H IS UPPER QUASI-TRIANGULAR IN
C          ROWS AND COLUMNS ILO:IHI, WITH ANY 2-BY-2 DIAGONAL BLOCKS IN
C          STANDARD FORM. IF WANTT IS .FALSE., THE CONTENTS OF H ARE
C          UNSPECIFIED ON EXIT.
C
C  LDH     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY H. LDH >= MAX(1,N).
C
C  WR      (OUTPUT) REAL*8 ARRAY, DIMENSION (N)
C  WI      (OUTPUT) REAL*8 ARRAY, DIMENSION (N)
C          THE REAL AND IMAGINARY PARTS, RESPECTIVELY, OF THE COMPUTED
C          EIGENVALUES ILO TO IHI ARE STORED IN THE CORRESPONDING
C          ELEMENTS OF WR AND WI. IF TWO EIGENVALUES ARE COMPUTED AS A
C          COMPLEX CONJUGATE PAIR, THEY ARE STORED IN CONSECUTIVE
C          ELEMENTS OF WR AND WI, SAY THE I-TH AND (I+1)TH, WITH
C          WI(I) > 0 AND WI(I+1) < 0. IF WANTT IS .TRUE., THE
C          EIGENVALUES ARE STORED IN THE SAME ORDER AS ON THE DIAGONAL
C          OF THE SCHUR FORM RETURNED IN H, WITH WR(I) = H(I,I), AND, IF
C          H(I:I+1,I:I+1) IS A 2-BY-2 DIAGONAL BLOCK,
C          WI(I) = SQRT(H(I+1,I)*H(I,I+1)) AND WI(I+1) = -WI(I).
C
C  ILOZ    (INPUT) INTEGER
C  IHIZ    (INPUT) INTEGER
C          SPECIFY THE ROWS OF Z TO WHICH TRANSFORMATIONS MUST BE
C          APPLIED IF WANTZ IS .TRUE..
C          1 <= ILOZ <= ILO, IHI <= IHIZ <= N.
C
C  Z       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDZ,N)
C          IF WANTZ IS .TRUE., ON ENTRY Z MUST CONTAIN THE CURRENT
C          MATRIX Z OF TRANSFORMATIONS ACCUMULATED BY DHSEQR, AND ON
C          EXIT Z HAS BEEN UPDATED, TRANSFORMATIONS ARE APPLIED ONLY TO
C          THE SUBMATRIX Z(ILOZ:IHIZ,ILO:IHI).
C          IF WANTZ IS .FALSE., Z IS NOT REFERENCED.
C
C  LDZ     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY Z. LDZ >= MAX(1,N).
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          > 0: FLAHQR FAILED TO COMPUTE ALL THE EIGENVALUES ILO TO IHI
C               IN A TOTAL OF 30*(IHI-ILO+1) ITERATIONS, IF INFO = I,
C               ELEMENTS I+1:IHI OF WR AND WI CONTAIN THOSE EIGENVALUES
C               WHICH HAVE BEEN SUCCESSFULLY COMPUTED.
C
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE DLAMCH ET DE DLABAD,
C            UTILISATION DE R8PREM(), R8MIEM() ET ISBAEM(),
C            REMPLACEMENT DE 3 RETURN PAR GOTO 1000,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            ABS, MAX, MIN.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      LOGICAL            WANTT, WANTZ
      INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, N
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   H( LDH, * ), WI( * ), WR( * ), Z( LDZ, * )
C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO
      PARAMETER          ( ZERO = 0.0D+0)
      REAL*8   DAT1, DAT2
      PARAMETER          ( DAT1 = 0.75D+0, DAT2 = -0.4375D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, I1, I2, ITN, ITS, J, K, L, M, NH, NR, NZ
      REAL*8   CS, H00, H10, H11, H12, H21, H22, H33, H33S,
     &                   H43H34, H44, H44S, S, SMLNUM, SN, SUM,
     &                   T1, T2, T3, TST1, ULP, UNFL, V1, V2, V3
C DUE TO CRS512       REAL*8 OVFL
C     ..
C     .. LOCAL ARRAYS ..
      REAL*8   V( 3 ), WORK( 1 )
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER ISBAEM
      REAL*8 DLANHS, R8PREM, R8MIEM
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C DUE TO CRS513
      WORK(1) = ZERO
      INFO = 0
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.EQ.0 )
     &   GOTO 1000
      IF( ILO.EQ.IHI ) THEN
         WR( ILO ) = H( ILO, ILO )
         WI( ILO ) = ZERO
         GOTO 1000
      END IF
C
      NH = IHI - ILO + 1
      NZ = IHIZ - ILOZ + 1
C
C     SET MACHINE-DEPENDENT CONSTANTS FOR THE STOPPING CRITERION.
C     IF NORM(H) <= SQRT(OVFL), OVERFLOW SHOULD NOT OCCUR.
C
      UNFL = R8MIEM()
C DUE TO CRS512      OVFL = ONE / UNFL
      ULP = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = UNFL*( NH / ULP )
C
C     I1 AND I2 ARE THE INDICES OF THE FIRST ROW AND LAST COLUMN OF H
C     TO WHICH TRANSFORMATIONS MUST BE APPLIED. IF EIGENVALUES ONLY ARE
C     BEING COMPUTED, I1 AND I2 ARE SET INSIDE THE MAIN LOOP.
C
      IF( WANTT ) THEN
         I1 = 1
         I2 = N
      END IF
C
C     ITN IS THE TOTAL NUMBER OF QR ITERATIONS ALLOWED.
C
      ITN = 30*NH
C
C     THE MAIN LOOP BEGINS HERE. I IS THE LOOP INDEX AND DECREASES FROM
C     IHI TO ILO IN STEPS OF 1 OR 2. EACH ITERATION OF THE LOOP WORKS
C     WITH THE ACTIVE SUBMATRIX IN ROWS AND COLUMNS L TO I.
C     EIGENVALUES I+1 TO IHI HAVE ALREADY CONVERGED. EITHER L = ILO OR
C     H(L,L-1) IS NEGLIGIBLE SO THAT THE MATRIX SPLITS.
C
      I = IHI
   10 CONTINUE
      L = ILO
      IF( I.LT.ILO )
     &   GO TO 150
C
C     PERFORM QR ITERATIONS ON ROWS AND COLUMNS ILO TO I UNTIL A
C     SUBMATRIX OF ORDER 1 OR 2 SPLITS OFF AT THE BOTTOM BECAUSE A
C     SUBDIAGONAL ELEMENT HAS BECOME NEGLIGIBLE.
C
      DO 130 ITS = 0, ITN
C
C        LOOK FOR A SINGLE SMALL SUBDIAGONAL ELEMENT.
C
         DO 20 K = I, L + 1, -1
            TST1 = ABS( H( K-1, K-1 ) ) + ABS( H( K, K ) )
            IF( TST1.EQ.ZERO )
     &         TST1 = DLANHS( '1', I-L+1, H( L, L ), LDH, WORK )
            IF( ABS( H( K, K-1 ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     &         GO TO 30
   20    CONTINUE
   30    CONTINUE
         L = K
         IF( L.GT.ILO ) THEN
C
C           H(L,L-1) IS NEGLIGIBLE
C
            H( L, L-1 ) = ZERO
         END IF
C
C        EXIT FROM LOOP IF A SUBMATRIX OF ORDER 1 OR 2 HAS SPLIT OFF.
C
         IF( L.GE.I-1 )
     &      GO TO 140
C
C        NOW THE ACTIVE SUBMATRIX IS IN ROWS AND COLUMNS L TO I. IF
C        EIGENVALUES ONLY ARE BEING COMPUTED, ONLY THE ACTIVE SUBMATRIX
C        NEED BE TRANSFORMED.
C
         IF( .NOT.WANTT ) THEN
            I1 = L
            I2 = I
         END IF
C
         IF( ITS.EQ.10 .OR. ITS.EQ.20 ) THEN
C
C           EXCEPTIONAL SHIFT.
C
            S = ABS( H( I, I-1 ) ) + ABS( H( I-1, I-2 ) )
            H44 = DAT1*S
            H33 = H44
            H43H34 = DAT2*S*S
         ELSE
C
C           PREPARE TO USE WILKINSON'S DOUBLE SHIFT
C
            H44 = H( I, I )
            H33 = H( I-1, I-1 )
            H43H34 = H( I, I-1 )*H( I-1, I )
         END IF
C
C        LOOK FOR TWO CONSECUTIVE SMALL SUBDIAGONAL ELEMENTS.
C
         DO 40 M = I - 2, L, -1
C
C           DETERMINE THE EFFECT OF STARTING THE DOUBLE-SHIFT QR
C           ITERATION AT ROW M, AND SEE IF THIS WOULD MAKE H(M,M-1)
C           NEGLIGIBLE.
C
            H11 = H( M, M )
            H22 = H( M+1, M+1 )
            H21 = H( M+1, M )
            H12 = H( M, M+1 )
            H44S = H44 - H11
            H33S = H33 - H11
            V1 = ( H33S*H44S-H43H34 ) / H21 + H12
            V2 = H22 - H11 - H33S - H44S
            V3 = H( M+2, M+1 )
            S = ABS( V1 ) + ABS( V2 ) + ABS( V3 )
            V1 = V1 / S
            V2 = V2 / S
            V3 = V3 / S
            V( 1 ) = V1
            V( 2 ) = V2
            V( 3 ) = V3
            IF( M.EQ.L )
     &         GO TO 50
            H00 = H( M-1, M-1 )
            H10 = H( M, M-1 )
            TST1 = ABS( V1 )*( ABS( H00 )+ABS( H11 )+ABS( H22 ) )
            IF( ABS( H10 )*( ABS( V2 )+ABS( V3 ) ).LE.ULP*TST1 )
     &         GO TO 50
   40    CONTINUE
   50    CONTINUE
C
C        DOUBLE-SHIFT QR STEP
C
         DO 120 K = M, I - 1
C
C           THE FIRST ITERATION OF THIS LOOP DETERMINES A REFLECTION G
C           FROM THE VECTOR V AND APPLIES IT FROM LEFT AND RIGHT TO H,
C           THUS CREATING A NONZERO BULGE BELOW THE SUBDIAGONAL.
C
C           EACH SUBSEQUENT ITERATION DETERMINES A REFLECTION G TO
C           RESTORE THE HESSENBERG FORM IN THE (K-1)TH COLUMN, AND THUS
C           CHASES THE BULGE ONE STEP TOWARD THE BOTTOM OF THE ACTIVE
C           SUBMATRIX. NR IS THE ORDER OF G.
C
            NR = MIN( 3, I-K+1 )
            IF( K.GT.M )
     &         CALL DCOPY( NR, H( K, K-1 ), 1, V, 1 )
            CALL FLARFG( NR, V( 1 ), V( 2 ), 1, T1 )
            IF( K.GT.M ) THEN
               H( K, K-1 ) = V( 1 )
               H( K+1, K-1 ) = ZERO
               IF( K.LT.I-1 )
     &            H( K+2, K-1 ) = ZERO
            ELSE IF( M.GT.L ) THEN
               H( K, K-1 ) = -H( K, K-1 )
            END IF
            V2 = V( 2 )
            T2 = T1*V2
            IF( NR.EQ.3 ) THEN
               V3 = V( 3 )
               T3 = T1*V3
C
C              APPLY G FROM THE LEFT TO TRANSFORM THE ROWS OF THE MATRIX
C              IN COLUMNS K TO I2.
C
               DO 60 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J ) + V3*H( K+2, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
                  H( K+2, J ) = H( K+2, J ) - SUM*T3
   60          CONTINUE
C
C              APPLY G FROM THE RIGHT TO TRANSFORM THE COLUMNS OF THE
C              MATRIX IN ROWS I1 TO MIN(K+3,I).
C
               DO 70 J = I1, MIN( K+3, I )
                  SUM = H( J, K ) + V2*H( J, K+1 ) + V3*H( J, K+2 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
                  H( J, K+2 ) = H( J, K+2 ) - SUM*T3
   70          CONTINUE
C
               IF( WANTZ ) THEN
C
C                 ACCUMULATE TRANSFORMATIONS IN THE MATRIX Z
C
                  DO 80 J = ILOZ, IHIZ
                     SUM = Z( J, K ) + V2*Z( J, K+1 ) + V3*Z( J, K+2 )
                     Z( J, K ) = Z( J, K ) - SUM*T1
                     Z( J, K+1 ) = Z( J, K+1 ) - SUM*T2
                     Z( J, K+2 ) = Z( J, K+2 ) - SUM*T3
   80             CONTINUE
               END IF
            ELSE IF( NR.EQ.2 ) THEN
C
C              APPLY G FROM THE LEFT TO TRANSFORM THE ROWS OF THE MATRIX
C              IN COLUMNS K TO I2.
C
               DO 90 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
   90          CONTINUE
C
C              APPLY G FROM THE RIGHT TO TRANSFORM THE COLUMNS OF THE
C              MATRIX IN ROWS I1 TO MIN(K+3,I).
C
               DO 100 J = I1, I
                  SUM = H( J, K ) + V2*H( J, K+1 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
  100          CONTINUE
C
               IF( WANTZ ) THEN
C
C                 ACCUMULATE TRANSFORMATIONS IN THE MATRIX Z
C
                  DO 110 J = ILOZ, IHIZ
                     SUM = Z( J, K ) + V2*Z( J, K+1 )
                     Z( J, K ) = Z( J, K ) - SUM*T1
                     Z( J, K+1 ) = Z( J, K+1 ) - SUM*T2
  110             CONTINUE
               END IF
            END IF
  120    CONTINUE
C
  130 CONTINUE
C
C     FAILURE TO CONVERGE IN REMAINING NUMBER OF ITERATIONS
C
      INFO = I
      GOTO 1000
C
  140 CONTINUE
C
      IF( L.EQ.I ) THEN
C
C        H(I,I-1) IS NEGLIGIBLE: ONE EIGENVALUE HAS CONVERGED.
C
         WR( I ) = H( I, I )
         WI( I ) = ZERO
      ELSE IF( L.EQ.I-1 ) THEN
C
C        H(I-1,I-2) IS NEGLIGIBLE: A PAIR OF EIGENVALUES HAVE CONVERGED.
C
C        TRANSFORM THE 2-BY-2 SUBMATRIX TO STANDARD SCHUR FORM,
C        AND COMPUTE AND STORE THE EIGENVALUES.
C
         CALL FLANV2( H( I-1, I-1 ), H( I-1, I ), H( I, I-1 ),
     &                H( I, I ), WR( I-1 ), WI( I-1 ), WR( I ), WI( I ),
     &                CS, SN )
C
         IF( WANTT ) THEN
C
C           APPLY THE TRANSFORMATION TO THE REST OF H.
C
            IF( I2.GT.I )
     &         CALL DROT( I2-I, H( I-1, I+1 ), LDH, H( I, I+1 ), LDH,
     &                    CS, SN )
            CALL DROT( I-I1-1, H( I1, I-1 ), 1, H( I1, I ), 1,CS,SN)
         END IF
         IF( WANTZ ) THEN
C
C           APPLY THE TRANSFORMATION TO Z.
C
            CALL DROT( NZ, Z( ILOZ, I-1 ), 1, Z( ILOZ, I ), 1,CS,SN)
         END IF
      END IF
C
C     DECREMENT NUMBER OF REMAINING ITERATIONS, AND RETURN TO START OF
C     THE MAIN LOOP WITH NEW VALUE OF I.
C
      ITN = ITN - ITS
      I = L - 1
      GO TO 10
C
  150 CONTINUE
 1000 CONTINUE
C
C     END OF FLAHQR
C
      END
