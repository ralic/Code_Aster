      SUBROUTINE DLAQRB
     &  (WANTT, N, ILO, IHI, H, LDH, WR, WI, Z, INFO )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C
C     SUBROUTINE ARPACK CALCULANT LES VALEURS PROPRES ET LA DECOMPOSI
C     TION DE SCHUR D'UNE MATRICE DE HESSENBERG.
C-----------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  COMPUTE THE EIGENVALUES AND THE SCHUR DECOMPOSITION OF AN UPPER
C  HESSENBERG SUBMATRIX IN ROWS AND COLUMNS ILO TO IHI.  ONLY THE
C  LAST COMPONENT OF THE SCHUR VECTORS ARE COMPUTED.
C
C  THIS IS MOSTLY A MODIFICATION OF THE LAPACK ROUTINE FLAHQR.
C
C ARGUMENTS
C  WANTT   LOGICAL VARIABLE.  (INPUT)
C          = .TRUE. : THE FULL SCHUR FORM T IS REQUIRED,
C          = .FALSE.: ONLY EIGENVALUES ARE REQUIRED.
C
C  N       INTEGER.  (INPUT)
C          THE ORDER OF THE MATRIX H.  N >= 0.
C
C  ILO     INTEGER.  (INPUT)
C  IHI     INTEGER.  (INPUT)
C          IT IS ASSUMED THAT H IS ALREADY UPPER QUASI-TRIANGULAR IN
C          ROWS AND COLUMNS IHI+1:N, AND THAT H(ILO,ILO-1) = 0 (UNLESS
C          ILO = 1). SLAQRB WORKS PRIMARILY WITH THE HESSENBERG
C          SUBMATRIX IN ROWS AND COLUMNS ILO TO IHI, BUT APPLIES
C          TRANSFORMATIONS TO ALL OF H IF WANTT IS .TRUE..
C          1 <= ILO <= MAX(1,IHI), IHI <= N.
C
C  H       REAL*8 ARRAY, DIMENSION (LDH,N).  (INPUT/OUTPUT)
C          ON ENTRY, THE UPPER HESSENBERG MATRIX H.
C          ON EXIT, IF WANTT IS .TRUE., H IS UPPER QUASI-TRIANGULAR IN
C          ROWS AND COLUMNS ILO:IHI, WITH ANY 2-BY-2 DIAGONAL BLOCKS IN
C          STANDARD FORM. IF WANTT IS .FALSE., THE CONTENTS OF H ARE
C          UNSPECIFIED ON EXIT.
C
C  LDH     INTEGER.  (INPUT)
C          THE LEADING DIMENSION OF THE ARRAY H. LDH >= MAX(1,N).
C
C  WR      REAL*8 ARRAY, DIMENSION (N).  (OUTPUT)
C  WI      REAL*8 ARRAY, DIMENSION (N).  (OUTPUT)
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
C  Z       REAL*8 ARRAY, DIMENSION (N).  (OUTPUT)
C          ON EXIT Z CONTAINS THE LAST COMPONENTS OF THE SCHUR VECTORS.
C
C  INFO    INTEGER.  (OUPUT)
C          = 0: SUCCESSFUL EXIT
C          > 0: SLAQRB FAILED TO COMPUTE ALL THE EIGENVALUES ILO TO IHI
C               IN A TOTAL OF 30*(IHI-ILO+1) ITERATIONS, IF INFO = I,
C               ELEMENTS I+1:IHI OF WR AND WI CONTAIN THOSE EIGENVALUES
C               WHICH HAVE BEEN SUCCESSFULLY COMPUTED.
C
C-----------------------------------------------------------------------
C BEGINLIB
C
C ROUTINES CALLED:
C     FLANHS  LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
C     FLANV2  LAPACK ROUTINE THAT COMPUTES THE SCHUR FACTORIZATION OF
C             2 BY 2 NONSYMMETRIC MATRIX IN STANDARD FORM.
C     FLARFG  LAPACK HOUSEHOLDER REFLECTION CONSTRUCTION ROUTINE.
C     BLCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER.
C     BLSROT    LEVEL 1 BLAS THAT APPLIES A ROTATION TO A 2 BY 2 MATRIX.
C
C     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION.
C     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES.
C     ISBAEM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE BASE.
C
C INTRINSIC FUNCTIONS
C     ABS, MIN, MAX.
C
C AUTHOR
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
C     APPLIED MATHEMATICS
C     RICE UNIVERSITY
C     HOUSTON, TEXAS
C
C REVISION HISTORY:
C     XX/XX/92: VERSION ' 2.4'
C               MODIFIED FROM THE LAPACK ROUTINE FLAHQR SO THAT ONLY THE
C               LAST COMPONENT OF THE SCHUR VECTORS ARE COMPUTED.
C
C FILE: LAQRB.F   SID: 2.2   DATE OF SID: 8/27/96   RELEASE: 2
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE DLAMCH ET DLABAD,
C            UTILISATION DE R8PREM(), R8MIEM() ET ISBAEM(),
C            REMPLACEMENT DE RETURN PAR GOTO 1000,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%

      LOGICAL    WANTT
      INTEGER    IHI, ILO, INFO, LDH, N

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      REAL*8 H( LDH, * ), WI( * ), WR( * ), Z( * )

C     %------------%
C     | PARAMETERS |
C     %------------%

      REAL*8 ZERO, ONE, DAT1, DAT2
      PARAMETER (ZERO = 0.0D+0, ONE = 1.0D+0, DAT1 = 7.5D-1,
     &           DAT2 = -4.375D-1)

C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%

      INTEGER    I, I1, I2, ITN, ITS, J, K, L, M, NH, NR
      REAL*8  CS, H00, H10, H11, H12, H21, H22, H33, H33S,
     &           H43H34, H44, H44S, S, SMLNUM, SN, SUM,
     &           T1, T2, T3, TST1, ULP, UNFL, V1, V2, V3
C DUE TO CRS512      REAL*8 OVFL
      REAL*8 V( 3 ), WORK( 1 )

C     %-----------%
C     | FUNCTIONS |
C     %-----------%

      INTEGER ISBAEM
      REAL*8 R8PREM, R8MIEM, FLANHS

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

C DUE TO CRS513
      WORK(1) = ZERO
      INFO = 0

C     %--------------------------%
C     | QUICK RETURN IF POSSIBLE |
C     %--------------------------%

      IF( N.EQ.0 )
     &   GOTO 1000
      IF( ILO.EQ.IHI ) THEN
         WR( ILO ) = H( ILO, ILO )
         WI( ILO ) = ZERO
         GOTO 1000
      END IF

C     %---------------------------------------------%
C     | INITIALIZE THE VECTOR OF LAST COMPONENTS OF |
C     | THE SCHUR VECTORS FOR ACCUMULATION.         |
C     %---------------------------------------------%

      DO 5 J = 1, N-1
         Z(J) = ZERO
  5   CONTINUE
      Z(N) = ONE

      NH = IHI - ILO + 1

C     %-------------------------------------------------------------%
C     | SET MACHINE-DEPENDENT CONSTANTS FOR THE STOPPING CRITERION. |
C     | IF NORM(H) <= SQRT(OVFL), OVERFLOW SHOULD NOT OCCUR.        |
C     %-------------------------------------------------------------%


      UNFL = R8MIEM()
C DUE TO CRS512      OVFL = ONE / UNFL
      ULP = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = UNFL*( NH / ULP )

C     %---------------------------------------------------------------%
C     | I1 AND I2 ARE THE INDICES OF THE FIRST ROW AND LAST COLUMN    |
C     | OF H TO WHICH TRANSFORMATIONS MUST BE APPLIED. IF EIGENVALUES |
C     | ONLY ARE COMPUTED, I1 AND I2 ARE SET INSIDE THE MAIN LOOP.    |
C     | ZERO OUT H(J+2,J) = ZERO FOR J=1:N IF WANTT = .TRUE.          |
C     | ELSE H(J+2,J) FOR J=ILO:IHI-ILO-1 IF WANTT = .FALSE.          |
C     %---------------------------------------------------------------%

      IF( WANTT ) THEN
         I1 = 1
         I2 = N
         DO 8 I=1,I2-2
            H(I1+I+1,I) = ZERO
 8       CONTINUE
      ELSE
         DO 9 I=1, IHI-ILO-1
            H(ILO+I+1,ILO+I-1) = ZERO
 9       CONTINUE
      END IF

C     %---------------------------------------------------%
C     | ITN IS THE TOTAL NUMBER OF QR ITERATIONS ALLOWED. |
C     %---------------------------------------------------%

      ITN = 30*NH

C     ------------------------------------------------------------------
C     THE MAIN LOOP BEGINS HERE. I IS THE LOOP INDEX AND DECREASES FROM
C     IHI TO ILO IN STEPS OF 1 OR 2. EACH ITERATION OF THE LOOP WORKS
C     WITH THE ACTIVE SUBMATRIX IN ROWS AND COLUMNS L TO I.
C     EIGENVALUES I+1 TO IHI HAVE ALREADY CONVERGED. EITHER L = ILO OR
C     H(L,L-1) IS NEGLIGIBLE SO THAT THE MATRIX SPLITS.
C     ------------------------------------------------------------------

      I = IHI
   10 CONTINUE
      L = ILO
      IF( I.LT.ILO )
     &   GO TO 150

C     %--------------------------------------------------------------%
C     | PERFORM QR ITERATIONS ON ROWS AND COLUMNS ILO TO I UNTIL A   |
C     | SUBMATRIX OF ORDER 1 OR 2 SPLITS OFF AT THE BOTTOM BECAUSE A |
C     | SUBDIAGONAL ELEMENT HAS BECOME NEGLIGIBLE.                   |
C     %--------------------------------------------------------------%

      DO 130 ITS = 0, ITN

C        %----------------------------------------------%
C        | LOOK FOR A SINGLE SMALL SUBDIAGONAL ELEMENT. |
C        %----------------------------------------------%

         DO 20 K = I, L + 1, -1
            TST1 = ABS( H( K-1, K-1 ) ) + ABS( H( K, K ) )
            IF( TST1.EQ.ZERO )
     &         TST1 = FLANHS( '1', I-L+1, H( L, L ), LDH, WORK )
            IF( ABS( H( K, K-1 ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     &         GO TO 30
   20    CONTINUE
   30    CONTINUE
         L = K
         IF( L.GT.ILO ) THEN

C           %------------------------%
C           | H(L,L-1) IS NEGLIGIBLE |
C           %------------------------%

            H( L, L-1 ) = ZERO
         END IF

C        %-------------------------------------------------------------%
C        | EXIT FROM LOOP IF A SUBMATRIX OF ORDER 1 OR 2 HAS SPLIT OFF |
C        %-------------------------------------------------------------%

         IF( L.GE.I-1 )
     &      GO TO 140

C        %---------------------------------------------------------%
C        | NOW THE ACTIVE SUBMATRIX IS IN ROWS AND COLUMNS L TO I. |
C        | IF EIGENVALUES ONLY ARE BEING COMPUTED, ONLY THE ACTIVE |
C        | SUBMATRIX NEED BE TRANSFORMED.                          |
C        %---------------------------------------------------------%

         IF( .NOT.WANTT ) THEN
            I1 = L
            I2 = I
         END IF

         IF( ITS.EQ.10 .OR. ITS.EQ.20 ) THEN

C           %-------------------%
C           | EXCEPTIONAL SHIFT |
C           %-------------------%

            S = ABS( H( I, I-1 ) ) + ABS( H( I-1, I-2 ) )
            H44 = DAT1*S
            H33 = H44
            H43H34 = DAT2*S*S

         ELSE

C           %-----------------------------------------%
C           | PREPARE TO USE WILKINSON'S DOUBLE SHIFT |
C           %-----------------------------------------%

            H44 = H( I, I )
            H33 = H( I-1, I-1 )
            H43H34 = H( I, I-1 )*H( I-1, I )
         END IF

C        %-----------------------------------------------------%
C        | LOOK FOR TWO CONSECUTIVE SMALL SUBDIAGONAL ELEMENTS |
C        %-----------------------------------------------------%

         DO 40 M = I - 2, L, -1

C           %---------------------------------------------------------%
C           | DETERMINE THE EFFECT OF STARTING THE DOUBLE-SHIFT QR    |
C           | ITERATION AT ROW M, AND SEE IF THIS WOULD MAKE H(M,M-1) |
C           | NEGLIGIBLE.                                             |
C           %---------------------------------------------------------%

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

C        %----------------------%
C        | DOUBLE-SHIFT QR STEP |
C        %----------------------%

         DO 120 K = M, I - 1

C           ------------------------------------------------------------
C           THE FIRST ITERATION OF THIS LOOP DETERMINES A REFLECTION G
C           FROM THE VECTOR V AND APPLIES IT FROM LEFT AND RIGHT TO H,
C           THUS CREATING A NONZERO BULGE BELOW THE SUBDIAGONAL.
C
C           EACH SUBSEQUENT ITERATION DETERMINES A REFLECTION G TO
C           RESTORE THE HESSENBERG FORM IN THE (K-1)TH COLUMN, AND THUS
C           CHASES THE BULGE ONE STEP TOWARD THE BOTTOM OF THE ACTIVE
C           SUBMATRIX. NR IS THE ORDER OF G.
C           ------------------------------------------------------------

            NR = MIN( 3, I-K+1 )
            IF( K.GT.M )
     &         CALL BLCOPY( NR, H( K, K-1 ), 1, V, 1 )
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

C              %------------------------------------------------%
C              | APPLY G FROM THE LEFT TO TRANSFORM THE ROWS OF |
C              | THE MATRIX IN COLUMNS K TO I2.                 |
C              %------------------------------------------------%

               DO 60 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J ) + V3*H( K+2, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
                  H( K+2, J ) = H( K+2, J ) - SUM*T3
   60          CONTINUE

C              %----------------------------------------------------%
C              | APPLY G FROM THE RIGHT TO TRANSFORM THE COLUMNS OF |
C              | THE MATRIX IN ROWS I1 TO MIN(K+3,I).               |
C              %----------------------------------------------------%

               DO 70 J = I1, MIN( K+3, I )
                  SUM = H( J, K ) + V2*H( J, K+1 ) + V3*H( J, K+2 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
                  H( J, K+2 ) = H( J, K+2 ) - SUM*T3
   70          CONTINUE

C              %----------------------------------%
C              | ACCUMULATE TRANSFORMATIONS FOR Z |
C              %----------------------------------%

               SUM      = Z( K ) + V2*Z( K+1 ) + V3*Z( K+2 )
               Z( K )   = Z( K ) - SUM*T1
               Z( K+1 ) = Z( K+1 ) - SUM*T2
               Z( K+2 ) = Z( K+2 ) - SUM*T3

            ELSE IF( NR.EQ.2 ) THEN

C              %------------------------------------------------%
C              | APPLY G FROM THE LEFT TO TRANSFORM THE ROWS OF |
C              | THE MATRIX IN COLUMNS K TO I2.                 |
C              %------------------------------------------------%

               DO 90 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
   90          CONTINUE

C              %----------------------------------------------------%
C              | APPLY G FROM THE RIGHT TO TRANSFORM THE COLUMNS OF |
C              | THE MATRIX IN ROWS I1 TO MIN(K+3,I).               |
C              %----------------------------------------------------%

               DO 100 J = I1, I
                  SUM = H( J, K ) + V2*H( J, K+1 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
  100          CONTINUE

C              %----------------------------------%
C              | ACCUMULATE TRANSFORMATIONS FOR Z |
C              %----------------------------------%

               SUM      = Z( K ) + V2*Z( K+1 )
               Z( K )   = Z( K ) - SUM*T1
               Z( K+1 ) = Z( K+1 ) - SUM*T2
            END IF
  120    CONTINUE

  130 CONTINUE

C     %-------------------------------------------------------%
C     | FAILURE TO CONVERGE IN REMAINING NUMBER OF ITERATIONS |
C     %-------------------------------------------------------%

      INFO = I
      GOTO 1000

  140 CONTINUE

      IF( L.EQ.I ) THEN

C        %------------------------------------------------------%
C        | H(I,I-1) IS NEGLIGIBLE: ONE EIGENVALUE HAS CONVERGED |
C        %------------------------------------------------------%

         WR( I ) = H( I, I )
         WI( I ) = ZERO

      ELSE IF( L.EQ.I-1 ) THEN

C        %--------------------------------------------------------%
C        | H(I-1,I-2) IS NEGLIGIBLE,                              |
C        | A PAIR OF EIGENVALUES HAVE CONVERGED.                  |
C        |                                                        |
C        | TRANSFORM THE 2-BY-2 SUBMATRIX TO STANDARD SCHUR FORM, |
C        | AND COMPUTE AND STORE THE EIGENVALUES.                 |
C        %--------------------------------------------------------%

         CALL FLANV2( H( I-1, I-1 ), H( I-1, I ), H( I, I-1 ),
     &                H( I, I ), WR( I-1 ), WI( I-1 ), WR( I ), WI( I ),
     &                CS, SN )

         IF( WANTT ) THEN

C           %-----------------------------------------------------%
C           | APPLY THE TRANSFORMATION TO THE REST OF H AND TO Z, |
C           | AS REQUIRED.                                        |
C           %-----------------------------------------------------%

            IF( I2.GT.I )
     &         CALL BLSROT( I2-I, H( I-1, I+1 ), LDH, H( I, I+1 ), LDH,
     &                    CS, SN )
            CALL BLSROT( I-I1-1, H( I1, I-1 ), 1, H( I1, I ), 1,CS,SN)
            SUM      = CS*Z( I-1 ) + SN*Z( I )
            Z( I )   = CS*Z( I )   - SN*Z( I-1 )
            Z( I-1 ) = SUM
         END IF
      END IF

C     %---------------------------------------------------------%
C     | DECREMENT NUMBER OF REMAINING ITERATIONS, AND RETURN TO |
C     | START OF THE MAIN LOOP WITH NEW VALUE OF I.             |
C     %---------------------------------------------------------%

      ITN = ITN - ITS
      I = L - 1
      GO TO 10

  150 CONTINUE
 1000 CONTINUE

C     %---------------%
C     | END OF DLAQRB |
C     %---------------%

      END
