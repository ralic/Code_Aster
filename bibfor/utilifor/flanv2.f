      SUBROUTINE FLANV2( A, B, C, D, RT1R, RT1I, RT2R, RT2I, CS, SN )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 06/11/2006   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK CALCULANT LA FACTORISATION DE SCHUR D'UNE
C     MATRICE 2X2.
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C  PURPOSE
C  =======
C
C  FLANV2 COMPUTES THE SCHUR FACTORIZATION OF A REAL 2-BY-2 NONSYMMETRIC
C  MATRIX IN STANDARD FORM:
C
C       ( A  B ) = ( CS -SN ) ( AA  BB ) ( CS  SN )
C       ( C  D )   ( SN  CS ) ( CC  DD ) (-SN  CS )
C
C  WHERE EITHER
C  1) CC = 0 SO THAT AA AND DD ARE REAL EIGENVALUES OF THE MATRIX, OR
C  2) AA = DD AND BB*CC < 0, SO THAT AA + OR - SQRT(BB*CC) ARE COMPLEX
C  CONJUGATE EIGENVALUES.
C
C  ARGUMENTS
C  =========
C
C  A       (INPUT/OUTPUT) REAL*8
C  B       (INPUT/OUTPUT) REAL*8
C  C       (INPUT/OUTPUT) REAL*8
C  D       (INPUT/OUTPUT) REAL*8
C          ON ENTRY, THE ELEMENTS OF THE INPUT MATRIX.
C          ON EXIT, THEY ARE OVERWRITTEN BY THE ELEMENTS OF THE
C          STANDARDISED SCHUR FORM.
C
C  RT1R    (OUTPUT) REAL*8
C  RT1I    (OUTPUT) REAL*8
C  RT2R    (OUTPUT) REAL*8
C  RT2I    (OUTPUT) REAL*8
C          THE REAL AND IMAGINARY PARTS OF THE EIGENVALUES. IF THE
C          EIGENVALUES ARE BOTH REAL, ABS(RT1R) >= ABS(RT2R), IF THE
C          EIGENVALUES ARE A COMPLEX CONJUGATE PAIR, RT1I > 0.
C
C  CS      (OUTPUT) REAL*8
C  SN      (OUTPUT) REAL*8
C          PARAMETERS OF THE ROTATION MATRIX.
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            ABS, SIGN, SQRT.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      REAL*8   A, B, C, CS, D, RT1I, RT1R, RT2I, RT2R, SN

C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      REAL*8   AA, BB, CC, CS1, DD, P, SAB, SAC, SIGMA, SN1,
     &                   TAU, TEMP
C     ..
C     .. EXTERNAL FUNCTIONS ..
      REAL*8   DLAPY2
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      CALL MATFPE(-1)
C
C     INITIALIZE CS AND SN
C
      CS = ONE
      SN = ZERO
C
      IF( C.EQ.ZERO ) THEN
         GO TO 10
C
      ELSE IF( B.EQ.ZERO ) THEN
C
C        SWAP ROWS AND COLUMNS
C
         CS = ZERO
         SN = ONE
         TEMP = D
         D = A
         A = TEMP
         B = -C
         C = ZERO
         GO TO 10
      ELSE IF( (A-D).EQ.ZERO .AND. SIGN( ONE, B ).NE.
     &   SIGN( ONE, C ) ) THEN
         GO TO 10
      ELSE
C
C        MAKE DIAGONAL ELEMENTS EQUAL
C
         TEMP = A - D
         P = HALF*TEMP
         SIGMA = B + C
         TAU = DLAPY2( SIGMA, TEMP )
         CS1 = SQRT( HALF*( ONE+ABS( SIGMA ) / TAU ) )
         SN1 = -( P / ( TAU*CS1 ) )*SIGN( ONE, SIGMA )
C
C        COMPUTE ( AA  BB ) = ( A  B ) ( CS1 -SN1 )
C                ( CC  DD )   ( C  D ) ( SN1  CS1 )
C
         AA = A*CS1 + B*SN1
         BB = -A*SN1 + B*CS1
         CC = C*CS1 + D*SN1
         DD = -C*SN1 + D*CS1
C
C        COMPUTE ( A  B ) = ( CS1  SN1 ) ( AA  BB )
C                ( C  D )   (-SN1  CS1 ) ( CC  DD )
C
         A = AA*CS1 + CC*SN1
         B = BB*CS1 + DD*SN1
         C = -AA*SN1 + CC*CS1
         D = -BB*SN1 + DD*CS1
C
C        ACCUMULATE TRANSFORMATION
C
         TEMP = CS*CS1 - SN*SN1
         SN = CS*SN1 + SN*CS1
         CS = TEMP
C
         TEMP = HALF*( A+D )
         A = TEMP
         D = TEMP
C
         IF( C.NE.ZERO ) THEN
            IF( B.NE.ZERO ) THEN
               IF( SIGN( ONE, B ).EQ.SIGN( ONE, C ) ) THEN
C
C                 REAL EIGENVALUES: REDUCE TO UPPER TRIANGULAR FORM
C
                  SAB = SQRT( ABS( B ) )
                  SAC = SQRT( ABS( C ) )
                  P = SIGN( SAB*SAC, C )
                  TAU = ONE / SQRT( ABS( B+C ) )
                  A = TEMP + P
                  D = TEMP - P
                  B = B - C
                  C = ZERO
                  CS1 = SAB*TAU
                  SN1 = SAC*TAU
                  TEMP = CS*CS1 - SN*SN1
                  SN = CS*SN1 + SN*CS1
                  CS = TEMP
               END IF
            ELSE
               B = -C
               C = ZERO
               TEMP = CS
               CS = -SN
               SN = TEMP
            END IF
         END IF
      END IF
C
   10 CONTINUE
C
C     STORE EIGENVALUES IN (RT1R,RT1I) AND (RT2R,RT2I).
C
      RT1R = A
      RT2R = D
      IF( C.EQ.ZERO ) THEN
         RT1I = ZERO
         RT2I = ZERO
      ELSE
         RT1I = SQRT( ABS( B ) )*SQRT( ABS( C ) )
         RT2I = -RT1I
      END IF
C
      CALL MATFPE(1)
C
C     END OF FLANV2
C
      END
