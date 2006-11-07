      SUBROUTINE DNAPPS
     &   ( N, KEV, NP, SHIFTR, SHIFTI, V, LDV, H, LDH, RESID, Q, LDQ,
     &     WORKL, WORKD)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 06/11/2006   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C
C     SUBROUTINE ARPACK PREPARANT LE RESTART VIA UN QR IMPLICITE POUR
C     ELIMINER LES NP MODES PROPRES INDESIRABLES.
C-----------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  GIVEN THE ARNOLDI FACTORIZATION
C
C     A*V_(K) - V_(K)*H_(K) = R_(K+P)*E_(K+P)T,
C
C  APPLY NP IMPLICIT SHIFTS RESULTING IN
C
C     A*(V_(K)*Q) - (V_(K)*Q)*(QT* H_(K)*Q) = R_(K+P)*E_(K+P)T * Q
C
C  WHERE Q IS AN ORTHOGONAL MATRIX WHICH IS THE PRODUCT OF ROTATIONS
C  AND REFLECTIONS RESULTING FROM THE NP BULGE CHAGE SWEEPS.
C  THE UPDATED ARNOLDI FACTORIZATION BECOMES:
C
C     A*VNEW_(K) - VNEW_(K)*HNEW_(K) = RNEW_(K)*E_(K)T.
C
C ARGUMENTS
C  N       INTEGER.  (INPUT)
C          PROBLEM SIZE, I.E. SIZE OF MATRIX A.
C
C  KEV     INTEGER.  (INPUT/OUTPUT)
C          KEV+NP IS THE SIZE OF THE INPUT MATRIX H.
C          KEV IS THE SIZE OF THE UPDATED MATRIX HNEW.  KEV IS ONLY
C          UPDATED ON OUPUT WHEN FEWER THAN NP SHIFTS ARE APPLIED IN
C          ORDER TO KEEP THE CONJUGATE PAIR TOGETHER.
C
C  NP      INTEGER.  (INPUT)
C          NUMBER OF IMPLICIT SHIFTS TO BE APPLIED.
C
C  SHIFTR, REAL*8 ARRAY OF LENGTH NP.  (INPUT)
C  SHIFTI  REAL AND IMAGINARY PART OF THE SHIFTS TO BE APPLIED.
C          UPON, ENTRY TO DNAPPS, THE SHIFTS MUST BE SORTED SO THAT
C          THE CONJUGATE PAIRS ARE IN CONSECUTIVE LOCATIONS.
C
C  V       REAL*8 N BY (KEV+NP) ARRAY.  (INPUT/OUTPUT)
C          ON INPUT, V CONTAINS THE CURRENT KEV+NP ARNOLDI VECTORS.
C          ON OUTPUT, V CONTAINS THE UPDATED KEV ARNOLDI VECTORS
C          IN THE FIRST KEV COLUMNS OF V.
C
C  LDV     INTEGER.  (INPUT)
C          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  H       REAL*8 (KEV+NP) BY (KEV+NP) ARRAY.  (INPUT/OUTPUT)
C          ON INPUT, H CONTAINS THE CURRENT KEV+NP BY KEV+NP UPPER
C          HESSENBER MATRIX OF THE ARNOLDI FACTORIZATION.
C          ON OUTPUT, H CONTAINS THE UPDATED KEV BY KEV UPPER
C          HESSENBERG MATRIX IN THE KEV LEADING SUBMATRIX.
C
C  LDH     INTEGER.  (INPUT)
C          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  RESID   REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C          ON INPUT, RESID CONTAINS THE THE RESIDUAL VECTOR R_(K+P).
C          ON OUTPUT, RESID IS THE UPDATE RESIDUAL VECTOR RNEW_(K)
C          IN THE FIRST KEV LOCATIONS.
C
C  Q       REAL*8 KEV+NP BY KEV+NP WORK ARRAY.  (WORKSPACE)
C          WORK ARRAY USED TO ACCUMULATE THE ROTATIONS AND REFLECTIONS
C          DURING THE BULGE CHASE SWEEP.
C
C  LDQ     INTEGER.  (INPUT)
C          LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  WORKL   REAL*8 WORK ARRAY OF LENGTH (KEV+NP).  (WORKSPACE)
C          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
C          THE FRONT END.
C
C  WORKD   REAL*8 WORK ARRAY OF LENGTH 2*N.  (WORKSPACE)
C          DISTRIBUTED ARRAY USED IN THE APPLICATION OF THE ACCUMULATED
C          ORTHOGONAL MATRIX Q.
C
C ENDDOC
C-----------------------------------------------------------------------
C BEGINLIB
C
C REFERENCES:
C  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
C     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
C     PP 357-385.
C
C ROUTINES CALLED:
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES.
C     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     DLACPY  LAPACK MATRIX COPY ROUTINE.
C     DLANHS  LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
C     DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C     DLARF   LAPACK ROUTINE THAT APPLIES HOUSEHOLDER REFLECTION TO
C             A MATRIX.
C     FLARFG  LAPACK HOUSEHOLDER REFLECTION CONSTRUCTION ROUTINE.
C     FLARTG  LAPACK GIVENS ROTATION CONSTRUCTION ROUTINE.
C     DLASET  LAPACK MATRIX INITIALIZATION ROUTINE.
C     DGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
C     DAXPY   LEVEL 1 BLAS THAT COMPUTES A VECTOR TRIAD.
C     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     DSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
C
C     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION.
C     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES.
C     ISBAEM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE BASE.
C
C INTRINSIC FUNCTIONS
C     ABS, MAX, MIN
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
C     XX/XX/92: VERSION ' 2.1'
C
C FILE: NAPPS.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
C
C REMARKS
C  1. IN THIS VERSION, EACH SHIFT IS APPLIED TO ALL THE SUBLOCKS OF
C     THE HESSENBERG MATRIX H AND NOT JUST TO THE SUBMATRIX THAT IT
C     COMES FROM. DEFLATION AS IN LAPACK ROUTINE FLAHQR (QR ALGORITHM
C     FOR UPPER HESSENBERG MATRICES ) IS USED.
C     THE SUBDIAGONALS OF H ARE ENFORCED TO BE NON-NEGATIVE.
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE SECOND, DLABAD ET DLAMCH,
C            COMMON TIMING REMPLACE PAR COMMON INFOR,
C            UTILISATION DE R8PREM(), R8MIEM() ET ISBAEM(),
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     %-----------------------------%
C     | INCLUDE FILES FOR DEBUGGING |
C     %-----------------------------%

      INTEGER LOGFIL, NDIGIT, MGETV0,
     &  MNAUPD, MNAUP2, MNAITR, MNEIGH, MNAPPS, MNGETS, MNEUPD
      COMMON /DEBUG/
     &  LOGFIL, NDIGIT, MGETV0,
     &  MNAUPD, MNAUP2, MNAITR, MNEIGH, MNAPPS, MNGETS, MNEUPD

C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%

      INTEGER KEV, LDH, LDQ, LDV, N, NP

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      REAL*8 H(LDH,KEV+NP), RESID(N), SHIFTI(NP), SHIFTR(NP),
     &  V(LDV,KEV+NP), Q(LDQ,KEV+NP), WORKD(2*N), WORKL(KEV+NP)

C     %------------%
C     | PARAMETERS |
C     %------------%

      REAL*8 ONE, ZERO, DEUX
      PARAMETER (ONE = 1.0D+0, ZERO = 0.0D+0, DEUX = 2.0D+0)

C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%

      INTEGER I, IEND, IR, ISTART, J, JJ, KPLUSP, MSGLVL, NR
      LOGICAL CCONJ, FIRST
      REAL*8 C, F, G, H11, H12, H21, H22, H32, R, S, SIGMAI,
     &  SIGMAR, SMLNUM, ULP, UNFL, U(3), T, TAU, TST1
C DUE TO CRS512      REAL*8 OVFL
C DUE TO CRS512      SAVE FIRST, OVFL, SMLNUM, ULP, UNFL
      SAVE FIRST, SMLNUM, ULP, UNFL

C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%

      INTEGER ISBAEM
      REAL*8 DLANHS, DLAPY2, R8PREM, R8MIEM

C     %----------------%
C     | DATA STATMENTS |
C     %----------------%

      DATA FIRST / .TRUE. /

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
      CALL MATFPE(-1)

      IF (FIRST) THEN

C        %-----------------------------------------------%
C        | SET MACHINE-DEPENDENT CONSTANTS FOR THE       |
C        | STOPPING CRITERION. IF NORM(H) <= SQRT(OVFL), |
C        | OVERFLOW SHOULD NOT OCCUR.                    |
C        | REFERENCE: LAPACK SUBROUTINE FLAHQR           |
C        %-----------------------------------------------%

         UNFL = R8MIEM()
C DUE RO CRS512         OVFL = ONE / UNFL
         ULP = R8PREM() *0.5D0 * ISBAEM()
         SMLNUM = UNFL*( N / ULP )
         FIRST = .FALSE.
      END IF

C     %-------------------------------%
C     | INITIALIZE TIMING STATISTICS  |
C     | & MESSAGE LEVEL FOR DEBUGGING |
C     %-------------------------------%

      MSGLVL = MNAPPS
      KPLUSP = KEV + NP

C     %--------------------------------------------%
C     | INITIALIZE Q TO THE IDENTITY TO ACCUMULATE |
C     | THE ROTATIONS AND REFLECTIONS              |
C     %--------------------------------------------%

C DUE TO CRP_102 CALL DLASET ('ALL', KPLUSP, KPLUSP, ZERO,
C ONE, Q, LDQ)
      CALL DLASET ('A', KPLUSP, KPLUSP, ZERO, ONE, Q, LDQ)

C     %----------------------------------------------%
C     | QUICK RETURN IF THERE ARE NO SHIFTS TO APPLY |
C     %----------------------------------------------%

      IF (NP .EQ. 0) GO TO 9000

C     %----------------------------------------------%
C     | CHASE THE BULGE WITH THE APPLICATION OF EACH |
C     | IMPLICIT SHIFT. EACH SHIFT IS APPLIED TO THE |
C     | WHOLE MATRIX INCLUDING EACH BLOCK.           |
C     %----------------------------------------------%

      CCONJ = .FALSE.
      DO 110 JJ = 1, NP
         SIGMAR = SHIFTR(JJ)
         SIGMAI = SHIFTI(JJ)

         IF (MSGLVL .GT. 2 ) THEN
             CALL IVOUT (LOGFIL, 1, JJ, NDIGIT,
     &               '_NAPPS: SHIFT NUMBER.')
             CALL DVOUT (LOGFIL, 1, SIGMAR, NDIGIT,
     &               '_NAPPS: THE REAL PART OF THE SHIFT ')
             CALL DVOUT (LOGFIL, 1, SIGMAI, NDIGIT,
     &               '_NAPPS: THE IMAGINARY PART OF THE SHIFT ')
         ENDIF

C        %-------------------------------------------------%
C        | THE FOLLOWING SET OF CONDITIONALS IS NECESSARY  |
C        | IN ORDER THAT COMPLEX CONJUGATE PAIRS OF SHIFTS |
C        | ARE APPLIED TOGETHER OR NOT AT ALL.             |
C        %-------------------------------------------------%

         IF ( CCONJ ) THEN

C           %-----------------------------------------%
C           | CCONJ = .TRUE. MEANS THE PREVIOUS SHIFT |
C           | HAD NON-ZERO IMAGINARY PART.            |
C           %-----------------------------------------%

            CCONJ = .FALSE.
            GO TO 110
         ELSE IF ( JJ .LT. NP .AND. ABS( SIGMAI ) .GT. ZERO ) THEN

C           %------------------------------------%
C           | START OF A COMPLEX CONJUGATE PAIR. |
C           %------------------------------------%

            CCONJ = .TRUE.
         ELSE IF ( JJ .EQ. NP .AND. ABS( SIGMAI ) .GT. ZERO ) THEN

C           %----------------------------------------------%
C           | THE LAST SHIFT HAS A NONZERO IMAGINARY PART. |
C           | DON'T APPLY IT, THUS THE ORDER OF THE        |
C           | COMPRESSED H IS ORDER KEV+1 SINCE ONLY NP-1  |
C           | WERE APPLIED.                                |
C           %----------------------------------------------%

            KEV = KEV + 1
            GO TO 110
         END IF
         ISTART = 1
   20    CONTINUE

C        %--------------------------------------------------%
C        | IF SIGMAI = 0 THEN                               |
C        |    APPLY THE JJ-TH SHIFT ...                     |
C        | ELSE                                             |
C        |    APPLY THE JJ-TH AND (JJ+1)-TH TOGETHER ...    |
C        |    (NOTE THAT JJ < NP AT THIS POINT IN THE CODE) |
C        | END                                              |
C        | TO THE CURRENT BLOCK OF H. THE NEXT DO LOOP      |
C        | DETERMINES THE CURRENT BLOCK ,                   |
C        %--------------------------------------------------%

         DO 30 I = ISTART, KPLUSP-1

C           %----------------------------------------%
C           | CHECK FOR SPLITTING AND DEFLATION. USE |
C           | A STANDARD TEST AS IN THE QR ALGORITHM |
C           | REFERENCE: LAPACK SUBROUTINE FLAHQR    |
C           %----------------------------------------%

            TST1 = ABS( H( I, I ) ) + ABS( H( I+1, I+1 ) )
            IF( TST1.EQ.ZERO )
     &         TST1 = DLANHS( '1', KPLUSP-JJ+1, H, LDH, WORKL )
            IF( ABS( H( I+1,I ) ).LE.MAX( ULP*TST1, SMLNUM ) ) THEN
               IF (MSGLVL .GT. 0) THEN
                   CALL IVOUT (LOGFIL, 1, I, NDIGIT,
     &                 '_NAPPS: MATRIX SPLITTING AT ROW/COLUMN NO.')
                   CALL IVOUT (LOGFIL, 1, JJ, NDIGIT,
     &                 '_NAPPS: MATRIX SPLITTING WITH SHIFT NUMBER.')
                   CALL DVOUT (LOGFIL, 1, H(I+1,I), NDIGIT,
     &                 '_NAPPS: OFF DIAGONAL ELEMENT.')
               ENDIF
               IEND = I
               H(I+1,I) = ZERO
               GO TO 40
            ENDIF
   30    CONTINUE
         IEND = KPLUSP
   40    CONTINUE
C
         IF (MSGLVL .GT. 2) THEN
             CALL IVOUT (LOGFIL, 1, ISTART, NDIGIT,
     &                   '_NAPPS: START OF CURRENT BLOCK ')
             CALL IVOUT (LOGFIL, 1, IEND, NDIGIT,
     &                   '_NAPPS: END OF CURRENT BLOCK ')
         ENDIF

C        %------------------------------------------------%
C        | NO REASON TO APPLY A SHIFT TO BLOCK OF ORDER 1 |
C        %------------------------------------------------%

         IF ( ISTART .EQ. IEND ) GO TO 100

C        %------------------------------------------------------%
C        | IF ISTART + 1 = IEND THEN NO REASON TO APPLY A       |
C        | COMPLEX CONJUGATE PAIR OF SHIFTS ON A 2 BY 2 MATRIX. |
C        %------------------------------------------------------%

         IF ( ISTART + 1 .EQ. IEND .AND. ABS( SIGMAI ) .GT. ZERO )
     &      GO TO 100

         H11 = H(ISTART,ISTART)
         H21 = H(ISTART+1,ISTART)
         IF ( ABS( SIGMAI ) .LE. ZERO ) THEN

C           %---------------------------------------------%
C           | REAL-VALUED SHIFT ==> APPLY SINGLE SHIFT QR |
C           %---------------------------------------------%

            F = H11 - SIGMAR
            G = H21

            DO 80 I = ISTART, IEND-1

C              %-----------------------------------------------------%
C              | CONTRUCT THE PLANE ROTATION G TO ZERO OUT THE BULGE |
C              %-----------------------------------------------------%

               CALL FLARTG (F, G, C, S, R)
               IF (I .GT. ISTART) THEN

C                 %-------------------------------------------%
C                 | THE FOLLOWING ENSURES THAT H(1:IEND-1,1), |
C                 | THE FIRST IEND-2 OFF DIAGONAL OF ELEMENTS |
C                 | H, REMAIN NON NEGATIVE.                   |
C                 %-------------------------------------------%

                  IF (R .LT. ZERO) THEN
                     R = -R
                     C = -C
                     S = -S
                  END IF
                  H(I,I-1) = R
                  H(I+1,I-1) = ZERO
               END IF

C              %---------------------------------------------%
C              | APPLY ROTATION TO THE LEFT OF H,  H <- G'*H |
C              %---------------------------------------------%

               DO 50 J = I, KPLUSP
                  T        =  C*H(I,J) + S*H(I+1,J)
                  H(I+1,J) = -S*H(I,J) + C*H(I+1,J)
                  H(I,J)   = T
   50          CONTINUE

C              %---------------------------------------------%
C              | APPLY ROTATION TO THE RIGHT OF H,  H <- H*G |
C              %---------------------------------------------%

               DO 60 J = 1, MIN(I+2,IEND)
                  T        =  C*H(J,I) + S*H(J,I+1)
                  H(J,I+1) = -S*H(J,I) + C*H(J,I+1)
                  H(J,I)   = T
   60          CONTINUE

C              %----------------------------------------------------%
C              | ACCUMULATE THE ROTATION IN THE MATRIX Q,  Q <- Q*G |
C              %----------------------------------------------------%

               DO 70 J = 1, MIN( I+JJ, KPLUSP )
                  T        =   C*Q(J,I) + S*Q(J,I+1)
                  Q(J,I+1) = - S*Q(J,I) + C*Q(J,I+1)
                  Q(J,I)   = T
   70          CONTINUE

C              %---------------------------%
C              | PREPARE FOR NEXT ROTATION |
C              %---------------------------%

               IF (I .LT. IEND-1) THEN
                  F = H(I+1,I)
                  G = H(I+2,I)
               END IF
   80       CONTINUE

C           %-----------------------------------%
C           | FINISHED APPLYING THE REAL SHIFT. |
C           %-----------------------------------%

         ELSE

C           %----------------------------------------------------%
C           | COMPLEX CONJUGATE SHIFTS ==> APPLY DOUBLE SHIFT QR |
C           %----------------------------------------------------%

            H12 = H(ISTART,ISTART+1)
            H22 = H(ISTART+1,ISTART+1)
            H32 = H(ISTART+2,ISTART+1)

C           %---------------------------------------------------------%
C           | COMPUTE 1ST COLUMN OF (H - SHIFT*I)*(H - CONJ(SHIFT)*I) |
C           %---------------------------------------------------------%

            S    = DEUX*SIGMAR
            T = DLAPY2 ( SIGMAR, SIGMAI )
            U(1) = ( H11 * (H11 - S) + T * T ) / H21 + H12
            U(2) = H11 + H22 - S
            U(3) = H32

            DO 90 I = ISTART, IEND-1

               NR = MIN ( 3, IEND-I+1 )

C              %-----------------------------------------------------%
C              | CONSTRUCT HOUSEHOLDER REFLECTOR G TO ZERO OUT U(1). |
C              | G IS OF THE FORM I - TAU*( 1 U )' * ( 1 U' ).       |
C              %-----------------------------------------------------%

               CALL FLARFG ( NR, U(1), U(2), 1, TAU )

               IF (I .GT. ISTART) THEN
                  H(I,I-1)   = U(1)
                  H(I+1,I-1) = ZERO
                  IF (I .LT. IEND-1) H(I+2,I-1) = ZERO
               END IF
               U(1) = ONE

C              %--------------------------------------%
C              | APPLY THE REFLECTOR TO THE LEFT OF H |
C              %--------------------------------------%
C DUE TO CRP_102 CALL DLARF ('LEFT', NR, KPLUSP-I+1, U, 1, TAU,
               CALL DLARF ('L', NR, KPLUSP-I+1, U, 1, TAU,
     &                     H(I,I), LDH, WORKL)

C              %---------------------------------------%
C              | APPLY THE REFLECTOR TO THE RIGHT OF H |
C              %---------------------------------------%

               IR = MIN ( I+3, IEND )
C DUE TO CRP_102 CALL DLARF ('RIGHT', IR, NR, U, 1, TAU,
               CALL DLARF ('R', IR, NR, U, 1, TAU,
     &                     H(1,I), LDH, WORKL)

C              %-----------------------------------------------------%
C              | ACCUMULATE THE REFLECTOR IN THE MATRIX Q,  Q <- Q*G |
C              %-----------------------------------------------------%

C DUE TO CRP_102 CALL DLARF ('RIGHT', KPLUSP, NR, U, 1, TAU,
               CALL DLARF ('R', KPLUSP, NR, U, 1, TAU,
     &                     Q(1,I), LDQ, WORKL)

C              %----------------------------%
C              | PREPARE FOR NEXT REFLECTOR |
C              %----------------------------%

               IF (I .LT. IEND-1) THEN
                  U(1) = H(I+1,I)
                  U(2) = H(I+2,I)
                  IF (I .LT. IEND-2) U(3) = H(I+3,I)
               END IF

   90       CONTINUE

C           %--------------------------------------------%
C           | FINISHED APPLYING A COMPLEX PAIR OF SHIFTS |
C           | TO THE CURRENT BLOCK                       |
C           %--------------------------------------------%

         END IF

  100    CONTINUE

C        %---------------------------------------------------------%
C        | APPLY THE SAME SHIFT TO THE NEXT BLOCK IF THERE IS ANY. |
C        %---------------------------------------------------------%

         ISTART = IEND + 1
         IF (IEND .LT. KPLUSP) GO TO 20

C        %---------------------------------------------%
C        | LOOP BACK TO THE TOP TO GET THE NEXT SHIFT. |
C        %---------------------------------------------%

  110 CONTINUE

C     %--------------------------------------------------%
C     | PERFORM A SIMILARITY TRANSFORMATION THAT MAKES   |
C     | SURE THAT H WILL HAVE NON NEGATIVE SUB DIAGONALS |
C     %--------------------------------------------------%

      DO 120 J=1,KEV
         IF ( H(J+1,J) .LT. ZERO ) THEN
              CALL DSCAL( KPLUSP-J+1, -ONE, H(J+1,J), LDH )
              CALL DSCAL( MIN(J+2, KPLUSP), -ONE, H(1,J+1), 1 )
              CALL DSCAL( MIN(J+NP+1,KPLUSP), -ONE, Q(1,J+1), 1 )
         END IF
 120  CONTINUE

      DO 130 I = 1, KEV

C        %--------------------------------------------%
C        | FINAL CHECK FOR SPLITTING AND DEFLATION.   |
C        | USE A STANDARD TEST AS IN THE QR ALGORITHM |
C        | REFERENCE: LAPACK SUBROUTINE FLAHQR        |
C        %--------------------------------------------%

         TST1 = ABS( H( I, I ) ) + ABS( H( I+1, I+1 ) )
         IF( TST1.EQ.ZERO )
     &       TST1 = DLANHS( '1', KEV, H, LDH, WORKL )
         IF( H( I+1,I ) .LE. MAX( ULP*TST1, SMLNUM ) )
     &       H(I+1,I) = ZERO
 130  CONTINUE

C     %-------------------------------------------------%
C     | COMPUTE THE (KEV+1)-ST COLUMN OF (V*Q) AND      |
C     | TEMPORARILY STORE THE RESULT IN WORKD(N+1:2*N). |
C     | THIS IS NEEDED IN THE RESIDUAL UPDATE SINCE WE  |
C     | CANNOT GUARANTEE THAT THE CORRESPONDING ENTRY   |
C     | OF H WOULD BE ZERO AS IN EXACT ARITHMETIC.      |
C     %-------------------------------------------------%

      IF (H(KEV+1,KEV) .GT. ZERO)
     & CALL DGEMV('N', N, KPLUSP, ONE, V, LDV, Q(1,KEV+1), 1, ZERO,
     &              WORKD(N+1), 1)

C     %----------------------------------------------------------%
C     | COMPUTE COLUMN 1 TO KEV OF (V*Q) IN BACKWARD ORDER       |
C     | TAKING ADVANTAGE OF THE UPPER HESSENBERG STRUCTURE OF Q. |
C     %----------------------------------------------------------%

      DO 140 I = 1, KEV
         CALL DGEMV ('N', N, KPLUSP-I+1, ONE, V, LDV,
     &               Q(1,KEV-I+1), 1, ZERO, WORKD, 1)
         CALL DCOPY (N, WORKD, 1, V(1,KPLUSP-I+1), 1)
  140 CONTINUE

C     %-------------------------------------------------%
C     |  MOVE V(:,KPLUSP-KEV+1:KPLUSP) INTO V(:,1:KEV). |
C     %-------------------------------------------------%

      CALL DLACPY ('A', N, KEV, V(1,KPLUSP-KEV+1), LDV, V, LDV)

C     %--------------------------------------------------------------%
C     | COPY THE (KEV+1)-ST COLUMN OF (V*Q) IN THE APPROPRIATE PLACE |
C     %--------------------------------------------------------------%

      IF (H(KEV+1,KEV) .GT. ZERO)
     &   CALL DCOPY (N, WORKD(N+1), 1, V(1,KEV+1), 1)

C     %-------------------------------------%
C     | UPDATE THE RESIDUAL VECTOR:         |
C     |    R <- SIGMAK*R + BETAK*V(:,KEV+1) |
C     | WHERE                               |
C     |    SIGMAK = (E_(KPLUSP)'*Q)*E_(KEV) |
C     |    BETAK = E_(KEV+1)'*H*E_(KEV)     |
C     %-------------------------------------%

      CALL DSCAL (N, Q(KPLUSP,KEV), RESID, 1)
      IF (H(KEV+1,KEV) .GT. ZERO)
     &   CALL DAXPY (N, H(KEV+1,KEV), V(1,KEV+1), 1, RESID, 1)

      IF (MSGLVL .GT. 1) THEN
         CALL DVOUT (LOGFIL, 1, Q(KPLUSP,KEV), NDIGIT,
     &        '_NAPPS: SIGMAK = (E_(KEV+P)T*Q)*E_(KEV)')
         CALL DVOUT (LOGFIL, 1, H(KEV+1,KEV), NDIGIT,
     &        '_NAPPS: BETAK = E_(KEV+1)T*H*E_(KEV)')
         CALL IVOUT (LOGFIL, 1, KEV, NDIGIT,
     &               '_NAPPS: ORDER OF THE FINAL HESSENBERG MATRIX ')
         IF (MSGLVL .GT. 2) THEN
            CALL DMOUT (LOGFIL, KEV, KEV, H, LDH, NDIGIT,
     &      '_NAPPS: UPDATED HESSENBERG MATRIX H FOR NEXT ITERATION')
         ENDIF
      END IF
C
 9000 CONTINUE
C
      CALL MATFPE(1)

C     %---------------%
C     | END OF DNAPPS |
C     %---------------%

      END
