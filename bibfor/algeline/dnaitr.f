      SUBROUTINE DNAITR
     &   (IDO, BMAT, N, K, NP, RESID, RNORM, V, LDV, H, LDH,
     &    IPNTR, WORKD, INFO, ALPHA)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C
C     SUBROUTINE ARPACK OPERANT NP ETAPE D'ARNOLDI A PARTIR D'UNE
C     FACTORISATION D'ORDRE K.
C-----------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  REVERSE COMMUNICATION INTERFACE FOR APPLYING NP ADDITIONAL STEPS TO
C  A K STEP NONSYMMETRIC ARNOLDI FACTORIZATION.
C
C  INPUT:  OP*V_(K)  -  V_(K)*H = R_(K)*E_(K)T
C
C          WITH (V_(K)T)*B*V_(K) = I, (V_(K)T)*B*R_(K) = 0.
C
C  OUTPUT: OP*V_(K+P)  -  V_(K+P)*H = R_(K+P)*E_(K+P)T
C
C          WITH (V_(K+P)T)*B*V_(K+P) = I, (V_(K+P)T)*B*R_(K+P) = 0.
C
C  WHERE OP AND B ARE AS IN DNAUPD.  THE B-NORM OF R_(K+P) IS ALSO
C  COMPUTED AND RETURNED.
C
C ARGUMENTS
C  IDO     INTEGER.  (INPUT/OUTPUT)
C          REVERSE COMMUNICATION FLAG.
C          -------------------------------------------------------------
C          IDO =  0: FIRST CALL TO THE REVERSE COMMUNICATION INTERFACE
C          IDO = -1: COMPUTE  Y = OP * X  WHERE
C                    IPNTR(1) IS THE POINTER INTO WORK FOR X,
C                    IPNTR(2) IS THE POINTER INTO WORK FOR Y.
C                    THIS IS FOR THE RESTART PHASE TO FORCE THE NEW
C                    STARTING VECTOR INTO THE RANGE OF OP.
C          IDO =  1: COMPUTE  Y = OP * X  WHERE
C                    IPNTR(1) IS THE POINTER INTO WORK FOR X,
C                    IPNTR(2) IS THE POINTER INTO WORK FOR Y,
C                    IPNTR(3) IS THE POINTER INTO WORK FOR B * X.
C          IDO =  2: COMPUTE  Y = B * X  WHERE
C                    IPNTR(1) IS THE POINTER INTO WORK FOR X,
C                    IPNTR(2) IS THE POINTER INTO WORK FOR Y.
C          IDO = 99: DONE
C          -------------------------------------------------------------
C          WHEN THE ROUTINE IS USED IN THE "SHIFT-AND-INVERT" MODE, THE
C          VECTOR B * Q IS ALREADY AVAILABLE AND DO NOT NEED TO BE
C          RECOMPUTE IN FORMING OP * Q.
C
C  BMAT    CHARACTER*1.  (INPUT)
C          BMAT SPECIFIES THE TYPE OF THE MATRIX B THAT DEFINES THE
C          SEMI-INNER PRODUCT FOR THE OPERATOR OP.  SEE DNAUPD.
C          B = 'I' -> STANDARD EIGENVALUE PROBLEM A*X = LAMBDA*X
C          B = 'G' -> GENERALIZED EIGENVALUE PROBLEM A*X = LAMBDA*M**X
C
C  N       INTEGER.  (INPUT)
C          DIMENSION OF THE EIGENPROBLEM.
C
C  K       INTEGER.  (INPUT)
C          CURRENT SIZE OF V AND H.
C
C  NP      INTEGER.  (INPUT)
C          NUMBER OF ADDITIONAL ARNOLDI STEPS TO TAKE.
C
C  RESID   REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C          ON INPUT:  RESID CONTAINS THE RESIDUAL VECTOR R_(K).
C          ON OUTPUT: RESID CONTAINS THE RESIDUAL VECTOR R_(K+P).
C
C  RNORM   REAL*8 SCALAR.  (INPUT/OUTPUT)
C          B-NORM OF THE STARTING RESIDUAL ON INPUT.
C          B-NORM OF THE UPDATED RESIDUAL R_(K+P) ON OUTPUT.
C
C  V       REAL*8 N BY K+NP ARRAY.  (INPUT/OUTPUT)
C          ON INPUT:  V CONTAINS THE ARNOLDI VECTORS IN THE FIRST K
C          COLUMNS.
C          ON OUTPUT: V CONTAINS THE NEW NP ARNOLDI VECTORS IN THE NEXT
C          NP COLUMNS.  THE FIRST K COLUMNS ARE UNCHANGED.
C
C  LDV     INTEGER.  (INPUT)
C          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  H       REAL*8 (K+NP) BY (K+NP) ARRAY.  (INPUT/OUTPUT)
C          H IS USED TO STORE THE GENERATED UPPER HESSENBERG MATRIX.
C
C  LDH     INTEGER.  (INPUT)
C          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  IPNTR   INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
C          POINTER TO MARK THE STARTING LOCATIONS IN THE WORK FOR
C          VECTORS USED BY THE ARNOLDI ITERATION.
C          -------------------------------------------------------------
C          IPNTR(1): POINTER TO THE CURRENT OPERAND VECTOR X.
C          IPNTR(2): POINTER TO THE CURRENT RESULT VECTOR Y.
C          IPNTR(3): POINTER TO THE VECTOR B * X WHEN USED IN THE
C                    SHIFT-AND-INVERT MODE.  X IS THE CURRENT OPERAND.
C          -------------------------------------------------------------
C
C  WORKD   REAL*8 WORK ARRAY OF LENGTH 3*N.  (REVERSE COMMUNICATION)
C          DISTRIBUTED ARRAY TO BE USED IN THE BASIC ARNOLDI ITERATION
C          FOR REVERSE COMMUNICATION.  THE CALLING PROGRAM SHOULD NOT
C          USE WORKD AS TEMPORARY WORKSPACE DURING THE ITERATION
C          ON INPUT, WORKD(1:N) = B*RESID AND IS USED TO SAVE SOME
C          COMPUTATION AT THE FIRST STEP.
C
C  INFO    INTEGER.  (OUTPUT)
C          = 0: NORMAL EXIT.
C          > 0: SIZE OF THE SPANNING INVARIANT SUBSPACE OF OP FOUND.
C
C  ALPHA   REAL  (INPUT/ NEW PARAMETER INTRODUCED FOR ASTER)
C          ORTHONORMALISATION PARAMETER FOR KAHAN-PARLETT ALGORITHM
C
C ENDDOC
C-----------------------------------------------------------------------
C BEGINLIB
C
C REFERENCES:
C  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
C     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
C     PP 357-385.
C  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
C     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
C     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
C
C ROUTINES CALLED:
C     DGETV0  ARPACK ROUTINE TO GENERATE THE INITIAL VECTOR.
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     FLASCL  LAPACK ROUTINE FOR CAREFUL SCALING OF A MATRIX.
C     FLANHS  LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
C     BLGEMV  LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
C     BLAXPY  LEVEL 1 BLAS THAT COMPUTES A VECTOR TRIAD.
C     BLSCAL  LEVEL 1 BLAS THAT SCALES A VECTOR.
C     BLCOPY  LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     BLSDOT  LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT OF VECTORS.
C     BLNRM2  LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
C
C     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION
C     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES
C     ISBAEM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE BASE.
C
C INTRINSIC FUNCTIONS
C     ABS, SQRT
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
C
C FILE: NAITR.F   SID: 2.4   DATE OF SID: 8/27/96   RELEASE: 2
C
C REMARKS
C  THE ALGORITHM IMPLEMENTED IS:
C
C  RESTART = .FALSE.
C  GIVEN V_(K) = (V_(1), ..., V_(K)), R_(K),
C  R_(K) CONTAINS THE INITIAL RESIDUAL VECTOR EVEN FOR K = 0,
C  ALSO ASSUME THAT RNORM = || B*R_(K) || AND B*R_(K) ARE ALREADY
C  COMPUTED BY THE CALLING PROGRAM.
C
C  BETAJ = RNORM , P_(K+1) = B*R_(K) ,
C  FOR  J = K+1, ..., K+NP  DO
C     1) IF ( BETAJ < TOL ) STOP OR RESTART DEPENDING ON J.
C        ( AT PRESENT TOL IS ZERO )
C        IF ( RESTART ) GENERATE A NEW STARTING VECTOR.
C     2) V_(J) = R(J-1)/BETAJ,  V_(J) = (V_(J-1), V_(J)),
C        P_(J) = P_(J)/BETAJ
C     3) R_(J) = OP*V_(J) WHERE OP IS DEFINED AS IN DNAUPD
C        FOR SHIFT-INVERT MODE P_(J) = B*V_(J) IS ALREADY AVAILABLE.
C        WNORM = || OP*V_(J) ||
C     4) COMPUTE THE J-TH STEP RESIDUAL VECTOR.
C        W_(J) =  V_(J)T * B * OP * V_(J)
C        R_(J) =  OP*V_(J) - V_(J) * W_(J)
C        H(:,J) = W_(J),
C        H(J,J-1) = RNORM
C        RNORM = || R_(J) ||
C        IF (RNORM > ALPHA*WNORM) ACCEPT STEP AND GO BACK TO 1)
C     5) RE-ORTHOGONALIZATION STEP:
C        S = V_(J)'*B*R_(J)
C        R_(J) = R_(J) - V_(J)*S,  RNORM1 = || R_(J) ||
C        ALPHAJ = ALPHAJ + S_(J),
C     6) ITERATIVE REFINEMENT STEP:
C        IF (RNORM1 > ALPHA*RNORM) THEN
C           RNORM = RNORM1
C           ACCEPT STEP AND GO BACK TO 1)
C        ELSE
C           RNORM = RNORM1
C           IF THIS IS THE FIRST TIME IN STEP 6), GO TO 5)
C           ELSE R_(J) LIES IN THE SPAN OF V_(J) NUMERICALLY.
C              SET R_(J) = 0 AND RNORM = 0, GO TO 1)
C        ENDIF
C  END DO
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
      INTEGER NOPX, NBX, NRORTH, NITREF, NRSTRT
      COMMON /INFOR/
     &  NOPX, NBX, NRORTH, NITREF, NRSTRT

C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%

      CHARACTER*1 BMAT
      INTEGER IDO, INFO, K, LDH, LDV, N, NP
      REAL*8 RNORM, ALPHA

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      INTEGER IPNTR(3)
      REAL*8 H(LDH,K+NP), RESID(N), V(LDV,K+NP), WORKD(3*N)

C     %------------%
C     | PARAMETERS |
C     %------------%

      REAL*8 ONE, ZERO
      PARAMETER (ONE = 1.0D+0, ZERO = 0.0D+0)

C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%

      LOGICAL FIRST, ORTH1, ORTH2, RSTART, STEP3, STEP4
      INTEGER IERR, I, INFOL, IPJ, IRJ, IVJ, ITER, ITRY, J, MSGLVL,
     &  JJ
      REAL*8 BETAJ, TEMP1, RNORM1, SMLNUM, TST1, ULP, UNFL,
     &  WNORM
C DUE TO CRS512      REAL*8 OVFL
C DUE TO CRS512      SAVE FIRST, ORTH1, ORTH2, RSTART, STEP3, STEP4,
C     &  IERR, IPJ, IRJ, IVJ, ITER, ITRY, J, MSGLVL, OVFL,
C     &  BETAJ, RNORM1, SMLNUM, ULP, UNFL, WNORM
      SAVE FIRST, ORTH1, ORTH2, RSTART, STEP3, STEP4,
     &  IERR, IPJ, IRJ, IVJ, ITER, ITRY, J, MSGLVL,
     &  BETAJ, RNORM1, SMLNUM, ULP, UNFL, WNORM

C     %-----------------------%
C     | LOCAL ARRAY ARGUMENTS |
C     %-----------------------%

      REAL*8 XTEMP(2)

C     %-----------%
C     | FUNCTIONS |
C     %-----------%

      INTEGER ISBAEM
      REAL*8 BLSDOT, BLNRM2, FLANHS, R8PREM, R8MIEM

C     %-----------------%
C     | DATA STATEMENTS |
C     %-----------------%

      DATA  FIRST / .TRUE. /

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

      IF (FIRST) THEN

C        %-----------------------------------------%
C        | SET MACHINE-DEPENDENT CONSTANTS FOR THE |
C        | THE SPLITTING AND DEFLATION CRITERION.  |
C        | IF NORM(H) <= SQRT(OVFL),               |
C        | OVERFLOW SHOULD NOT OCCUR.              |
C        | REFERENCE: LAPACK SUBROUTINE FLAHQR     |
C        %-----------------------------------------%

         UNFL = R8MIEM()
C DUE TO CRS512         OVFL = ONE / UNFL
         ULP = R8PREM() * 0.5D0 * ISBAEM()
         SMLNUM = UNFL*( N / ULP )
         FIRST = .FALSE.
      END IF

      IF (IDO .EQ. 0) THEN

C        %-------------------------------%
C        | INITIALIZE TIMING STATISTICS  |
C        | & MESSAGE LEVEL FOR DEBUGGING |
C        %-------------------------------%

         MSGLVL = MNAITR

C        %------------------------------%
C        | INITIAL CALL TO THIS ROUTINE |
C        %------------------------------%

         INFO   = 0
         STEP3  = .FALSE.
         STEP4  = .FALSE.
         RSTART = .FALSE.
         ORTH1  = .FALSE.
         ORTH2  = .FALSE.
         J      = K + 1
         IPJ    = 1
         IRJ    = IPJ   + N
         IVJ    = IRJ   + N
      END IF

C     %-------------------------------------------------%
C     | WHEN IN REVERSE COMMUNICATION MODE ONE OF:      |
C     | STEP3, STEP4, ORTH1, ORTH2, RSTART              |
C     | WILL BE .TRUE. WHEN ....                        |
C     | STEP3: RETURN FROM COMPUTING OP*V_(J).          |
C     | STEP4: RETURN FROM COMPUTING B-NORM OF OP*V_(J) |
C     | ORTH1: RETURN FROM COMPUTING B-NORM OF R_(J+1)  |
C     | ORTH2: RETURN FROM COMPUTING B-NORM OF          |
C     |        CORRECTION TO THE RESIDUAL VECTOR.       |
C     | RSTART: RETURN FROM OP COMPUTATIONS NEEDED BY   |
C     |         DGETV0.                                 |
C     %-------------------------------------------------%

      IF (STEP3)  GO TO 50
      IF (STEP4)  GO TO 60
      IF (ORTH1)  GO TO 70
      IF (ORTH2)  GO TO 90
      IF (RSTART) GO TO 30

C     %-----------------------------%
C     | ELSE THIS IS THE FIRST STEP |
C     %-----------------------------%

C     %--------------------------------------------------------------%
C     |                                                              |
C     |        A R N O L D I     I T E R A T I O N     L O O P       |
C     |                                                              |
C     | NOTE:  B*R_(J-1) IS ALREADY IN WORKD(1:N)=WORKD(IPJ:IPJ+N-1) |
C     %--------------------------------------------------------------%

 1000 CONTINUE

         IF (MSGLVL .GT. 1) THEN
             CALL IVOUT (LOGFIL, 1, J, NDIGIT,
     &                  '_NAITR: GENERATING ARNOLDI VECTOR NUMBER')
             CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT,
     &                  '_NAITR: B-NORM OF THE CURRENT RESIDUAL IS')
         ENDIF

C        %---------------------------------------------------%
C        | STEP 1: CHECK IF THE B NORM OF J-TH RESIDUAL      |
C        | VECTOR IS ZERO. EQUIVALENT TO DETERMING WHETHER   |
C        | AN EXACT J-STEP ARNOLDI FACTORIZATION IS PRESENT. |
C        %---------------------------------------------------%

         BETAJ = RNORM
         IF (RNORM .GT. ZERO) GO TO 40

C           %---------------------------------------------------%
C           | INVARIANT SUBSPACE FOUND, GENERATE A NEW STARTING |
C           | VECTOR WHICH IS ORTHOGONAL TO THE CURRENT ARNOLDI |
C           | BASIS AND CONTINUE THE ITERATION.                 |
C           %---------------------------------------------------%

            IF (MSGLVL .GT. 0) THEN
                CALL IVOUT (LOGFIL, 1, J, NDIGIT,
     &                     '_NAITR: ****** RESTART AT STEP ******')
            ENDIF

C           %---------------------------------------------%
C           | ITRY IS THE LOOP VARIABLE THAT CONTROLS THE |
C           | MAXIMUM AMOUNT OF TIMES THAT A RESTART IS   |
C           | ATTEMPTED. NRSTRT IS USED BY STAT.H         |
C           %---------------------------------------------%

            BETAJ  = ZERO
            NRSTRT = NRSTRT + 1
            ITRY   = 1
   20       CONTINUE
            RSTART = .TRUE.
            IDO    = 0
   30       CONTINUE

C           %--------------------------------------%
C           | IF IN REVERSE COMMUNICATION MODE AND |
C           | RSTART = .TRUE. FLOW RETURNS HERE.   |
C           %--------------------------------------%

            CALL DGETV0 (IDO, BMAT, ITRY, .FALSE., N, J, V, LDV,
     &                   RESID, RNORM, IPNTR, WORKD, IERR, ALPHA)
            IF (IDO .NE. 99) GO TO 9000
            IF (IERR .LT. 0) THEN
               ITRY = ITRY + 1
               IF (ITRY .LE. 3) GO TO 20

C              %------------------------------------------------%
C              | GIVE UP AFTER SEVERAL RESTART ATTEMPTS.        |
C              | SET INFO TO THE SIZE OF THE INVARIANT SUBSPACE |
C              | WHICH SPANS OP AND EXIT.                       |
C              %------------------------------------------------%

               INFO = J - 1
               IDO = 99
               GO TO 9000
            END IF

   40    CONTINUE

C        %---------------------------------------------------------%
C        | STEP 2:  V_(J) = R_(J-1)/RNORM AND P_(J) = P_(J)/RNORM  |
C        | NOTE THAT P_(J) = B*R_(J-1). IN ORDER TO AVOID OVERFLOW |
C        | WHEN RECIPROCATING A SMALL RNORM, TEST AGAINST LOWER    |
C        | MACHINE BOUND.                                          |
C        %---------------------------------------------------------%

         CALL BLCOPY (N, RESID, 1, V(1,J), 1)
         IF (RNORM .GE. UNFL) THEN
             TEMP1 = ONE / RNORM
             CALL BLSCAL (N, TEMP1, V(1,J), 1)
             CALL BLSCAL (N, TEMP1, WORKD(IPJ), 1)
         ELSE

C            %-----------------------------------------%
C            | TO SCALE BOTH V_(J) AND P_(J) CAREFULLY |
C            | USE LAPACK ROUTINE SLASCL               |
C            %-----------------------------------------%

C DUE TO CRP_102 CALL FLASCL ('GENERAL', I, I, RNORM, ONE, N, 1,
             CALL FLASCL ('G', I, I, RNORM, ONE, N, 1,
     &                    V(1,J), N, INFOL)
C DUE TO CRP_102 CALL FLASCL ('GENERAL', I, I, RNORM, ONE, N, 1,
             CALL FLASCL ('G', I, I, RNORM, ONE, N, 1,
     &                    WORKD(IPJ), N, INFOL)
         END IF

C        %------------------------------------------------------%
C        | STEP 3:  R_(J) = OP*V_(J), NOTE THAT P_(J) = B*V_(J) |
C        | NOTE THAT THIS IS NOT QUITE YET R_(J). SEE STEP 4    |
C        %------------------------------------------------------%

         STEP3 = .TRUE.
         NOPX  = NOPX + 1
         CALL BLCOPY (N, V(1,J), 1, WORKD(IVJ), 1)
         IPNTR(1) = IVJ
         IPNTR(2) = IRJ
         IPNTR(3) = IPJ
         IDO = 1

C        %-----------------------------------%
C        | EXIT IN ORDER TO COMPUTE OP*V_(J) |
C        %-----------------------------------%

         GO TO 9000
   50    CONTINUE

C        %----------------------------------%
C        | BACK FROM REVERSE COMMUNICATION, |
C        | WORKD(IRJ:IRJ+N-1) := OP*V_(J)   |
C        | IF STEP3 = .TRUE.                |
C        %----------------------------------%

         STEP3 = .FALSE.

C        %------------------------------------------%
C        | PUT ANOTHER COPY OF OP*V_(J) INTO RESID. |
C        %------------------------------------------%

         CALL BLCOPY (N, WORKD(IRJ), 1, RESID, 1)

C        %---------------------------------------%
C        | STEP 4:  FINISH EXTENDING THE ARNOLDI |
C        |          FACTORIZATION TO LENGTH J.   |
C        %---------------------------------------%

         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            STEP4 = .TRUE.
            IPNTR(1) = IRJ
            IPNTR(2) = IPJ
            IDO = 2

C           %-------------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*OP*V_(J) |
C           %-------------------------------------%

            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL BLCOPY (N, RESID, 1, WORKD(IPJ), 1)
         END IF
   60    CONTINUE

C        %----------------------------------%
C        | BACK FROM REVERSE COMMUNICATION, |
C        | WORKD(IPJ:IPJ+N-1) := B*OP*V_(J) |
C        | IF STEP4 = .TRUE.                |
C        %----------------------------------%

         STEP4 = .FALSE.

C        %-------------------------------------%
C        | THE FOLLOWING IS NEEDED FOR STEP 5. |
C        | COMPUTE THE B-NORM OF OP*V_(J).     |
C        %-------------------------------------%

         IF (BMAT .EQ. 'G') THEN
             WNORM = BLSDOT (N, RESID, 1, WORKD(IPJ), 1)
             WNORM = SQRT(ABS(WNORM))
         ELSE IF (BMAT .EQ. 'I') THEN
            WNORM = BLNRM2(N, RESID, 1)
         END IF

C        %-----------------------------------------%
C        | COMPUTE THE J-TH RESIDUAL CORRESPONDING |
C        | TO THE J STEP FACTORIZATION.            |
C        | USE CLASSICAL GRAM SCHMIDT AND COMPUTE: |
C        | W_(J) <-  V_(J)T * B * OP * V_(J)      |
C        | R_(J) <-  OP*V_(J) - V_(J) * W_(J)      |
C        %-----------------------------------------%

C        %------------------------------------------%
C        | COMPUTE THE J FOURIER COEFFICIENTS W_(J) |
C        | WORKD(IPJ:IPJ+N-1) CONTAINS B*OP*V_(J).  |
C        %------------------------------------------%

         CALL BLGEMV ('T', N, J, ONE, V, LDV, WORKD(IPJ), 1,
     &               ZERO, H(1,J), 1)

C        %--------------------------------------%
C        | ORTHOGONALIZE R_(J) AGAINST V_(J).   |
C        | RESID CONTAINS OP*V_(J). SEE STEP 3. |
C        %--------------------------------------%

         CALL BLGEMV ('N', N, J, -ONE, V, LDV, H(1,J), 1,
     &               ONE, RESID, 1)

         IF (J .GT. 1) H(J,J-1) = BETAJ
         ORTH1 = .TRUE.
         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            CALL BLCOPY (N, RESID, 1, WORKD(IRJ), 1)
            IPNTR(1) = IRJ
            IPNTR(2) = IPJ
            IDO = 2

C           %----------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*R_(J) |
C           %----------------------------------%

            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL BLCOPY (N, RESID, 1, WORKD(IPJ), 1)
         END IF
   70    CONTINUE

C        %---------------------------------------------------%
C        | BACK FROM REVERSE COMMUNICATION IF ORTH1 = .TRUE. |
C        | WORKD(IPJ:IPJ+N-1) := B*R_(J).                    |
C        %---------------------------------------------------%

         ORTH1 = .FALSE.

C        %------------------------------%
C        | COMPUTE THE B-NORM OF R_(J). |
C        %------------------------------%

         IF (BMAT .EQ. 'G') THEN
            RNORM = BLSDOT (N, RESID, 1, WORKD(IPJ), 1)
            RNORM = SQRT(ABS(RNORM))
         ELSE IF (BMAT .EQ. 'I') THEN
            RNORM = BLNRM2(N, RESID, 1)
         END IF

C        %-----------------------------------------------------------%
C        | STEP 5: RE-ORTHOGONALIZATION / ITERATIVE REFINEMENT PHASE |
C        | MAXIMUM NITER_ITREF TRIES.                                |
C        |                                                           |
C        |          S      = V_(J)T * B * R_(J)                      |
C        |          R_(J)  = R_(J) - V_(J)*S                         |
C        |          ALPHAJ = ALPHAJ + S_(J)                          |
C        |                                                           |
C        | THE STOPPING CRITERIA USED FOR ITERATIVE REFINEMENT IS    |
C        | DISCUSSED IN PARLETT'S BOOK SEP, PAGE 107 AND IN GRAGG &  |
C        | REICHEL ACM TOMS PAPER, ALGORITHM 686, DEC. 1990.         |
C        | DETERMINE IF WE NEED TO CORRECT THE RESIDUAL. THE GOAL IS |
C        | TO ENFORCE ||V(:,1:J)T * R_(J)|| .LE. EPS * || R_(J) ||   |
C        | THE FOLLOWING TEST DETERMINES WHETHER THE SINE OF THE     |
C        | ANGLE BETWEEN  OP*X AND THE COMPUTED RESIDUAL IS LESS     |
C        | THAN OR EQUAL TO 0.717.                                   |
C        %-----------------------------------------------------------%

         IF (RNORM .GT. ALPHA*WNORM) GO TO 100
         ITER  = 0
         NRORTH = NRORTH + 1

C        %---------------------------------------------------%
C        | ENTER THE ITERATIVE REFINEMENT PHASE. IF FURTHER  |
C        | REFINEMENT IS NECESSARY, LOOP BACK HERE. THE LOOP |
C        | VARIABLE IS ITER. PERFORM A STEP OF CLASSICAL     |
C        | GRAM-SCHMIDT USING ALL THE ARNOLDI VECTORS V_(J)  |
C        %---------------------------------------------------%

   80    CONTINUE
C
         IF (MSGLVL .GT. 2) THEN
             XTEMP(1) = WNORM
             XTEMP(2) = RNORM
             CALL DVOUT (LOGFIL, 2, XTEMP, NDIGIT,
     &           '_NAITR: RE-ORTHONALIZATION, WNORM AND RNORM ARE')
             CALL DVOUT (LOGFIL, J, H(1,J), NDIGIT,
     &                  '_NAITR: J-TH COLUMN OF H')
         ENDIF

C        %----------------------------------------------------%
C        | COMPUTE V_(J)T * B * R_(J)                         |
C        | WORKD(IRJ:IRJ+J-1) = V(:,1:J)'*WORKD(IPJ:IPJ+N-1). |
C        %----------------------------------------------------%

         CALL BLGEMV ('T', N, J, ONE, V, LDV, WORKD(IPJ), 1,
     &               ZERO, WORKD(IRJ), 1)

C        %---------------------------------------------%
C        | COMPUTE THE CORRECTION TO THE RESIDUAL:     |
C        | R_(J) = R_(J) - V_(J) * WORKD(IRJ:IRJ+J-1). |
C        | THE CORRECTION TO H IS V(:,1:J)*H(1:J,1:J)  |
C        | + V(:,1:J)*WORKD(IRJ:IRJ+J-1)*E'_J.         |
C        %---------------------------------------------%

         CALL BLGEMV ('N', N, J, -ONE, V, LDV, WORKD(IRJ), 1,
     &               ONE, RESID, 1)
         CALL BLAXPY (J, ONE, WORKD(IRJ), 1, H(1,J), 1)

         ORTH2 = .TRUE.
         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            CALL BLCOPY (N, RESID, 1, WORKD(IRJ), 1)
            IPNTR(1) = IRJ
            IPNTR(2) = IPJ
            IDO = 2

C           %-----------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*R_(J). |
C           | R_(J) IS THE CORRECTED RESIDUAL.  |
C           %-----------------------------------%

            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL BLCOPY (N, RESID, 1, WORKD(IPJ), 1)
         END IF
   90    CONTINUE

C        %---------------------------------------------------%
C        | BACK FROM REVERSE COMMUNICATION IF ORTH2 = .TRUE. |
C        %---------------------------------------------------%

C        %-----------------------------------------------------%
C        | COMPUTE THE B-NORM OF THE CORRECTED RESIDUAL R_(J). |
C        %-----------------------------------------------------%
C
         IF (BMAT .EQ. 'G') THEN
             RNORM1 = BLSDOT (N, RESID, 1, WORKD(IPJ), 1)
             RNORM1 = SQRT(ABS(RNORM1))
         ELSE IF (BMAT .EQ. 'I') THEN
             RNORM1 = BLNRM2(N, RESID, 1)
         END IF
C
         IF (MSGLVL .GT. 0 .AND. ITER .GT. 0) THEN
             CALL IVOUT (LOGFIL, 1, J, NDIGIT,
     &           '_NAITR: ITERATIVE REFINEMENT FOR ARNOLDI RESIDUAL')
             IF (MSGLVL .GT. 2) THEN
                XTEMP(1) = RNORM
                XTEMP(2) = RNORM1
                CALL DVOUT (LOGFIL, 2, XTEMP, NDIGIT,
     &           '_NAITR: ITERATIVE REFINEMENT , RNORM AND RNORM1 ARE')
             ENDIF
         ENDIF

C        %-----------------------------------------%
C        | DETERMINE IF WE NEED TO PERFORM ANOTHER |
C        | STEP OF RE-ORTHOGONALIZATION.           |
C        %-----------------------------------------%

         IF (RNORM1 .GT. ALPHA*RNORM) THEN

C           %---------------------------------------%
C           | NO NEED FOR FURTHER REFINEMENT.       |
C           | THE COSINE OF THE ANGLE BETWEEN THE   |
C           | CORRECTED RESIDUAL VECTOR AND THE OLD |
C           | RESIDUAL VECTOR IS GREATER THAN 0.717 |
C           | IN OTHER WORDS THE CORRECTED RESIDUAL |
C           | AND THE OLD RESIDUAL VECTOR SHARE AN  |
C           | ANGLE OF LESS THAN ARCCOS(0.717)      |
C           %---------------------------------------%

            RNORM = RNORM1

         ELSE

C           %-------------------------------------------%
C           | ANOTHER STEP OF ITERATIVE REFINEMENT STEP |
C           | IS REQUIRED. NITREF IS USED BY STAT.H     |
C           %-------------------------------------------%

            NITREF = NITREF + 1
            RNORM  = RNORM1
            ITER   = ITER + 1
            IF (ITER .LE. 1) GO TO 80

C           %-------------------------------------------------%
C           | OTHERWISE RESID IS NUMERICALLY IN THE SPAN OF V |
C           %-------------------------------------------------%

            DO 95 JJ = 1, N
               RESID(JJ) = ZERO
  95        CONTINUE
            RNORM = ZERO
         END IF

C        %----------------------------------------------%
C        | BRANCH HERE DIRECTLY IF ITERATIVE REFINEMENT |
C        | WASN'T NECESSARY OR AFTER AT MOST NITER_REF  |
C        | STEPS OF ITERATIVE REFINEMENT.               |
C        %----------------------------------------------%

  100    CONTINUE
C
         RSTART = .FALSE.
         ORTH2  = .FALSE.

C        %------------------------------------%
C        | STEP 6: UPDATE  J = J+1,  CONTINUE |
C        %------------------------------------%
         J = J + 1
         IF (J .GT. K+NP) THEN
            IDO = 99
            DO 110 I = MAX(1,K), K+NP-1

C              %--------------------------------------------%
C              | CHECK FOR SPLITTING AND DEFLATION.         |
C              | USE A STANDARD TEST AS IN THE QR ALGORITHM |
C              | REFERENCE: LAPACK SUBROUTINE FLAHQR        |
C              %--------------------------------------------%

               TST1 = ABS( H( I, I ) ) + ABS( H( I+1, I+1 ) )
               IF( TST1.EQ.ZERO )
     &              TST1 = FLANHS( '1', K+NP, H, LDH, WORKD(N+1) )
               IF( ABS( H( I+1,I ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     &              H(I+1,I) = ZERO
 110        CONTINUE

            IF (MSGLVL .GT. 2) THEN
                CALL DMOUT (LOGFIL, K+NP, K+NP, H, LDH, NDIGIT,
     &          '_NAITR: FINAL UPPER HESSENBERG MATRIX H OF ORDER K+NP')
            ENDIF

            GO TO 9000
         ENDIF

C        %--------------------------------------------------------%
C        | LOOP BACK TO EXTEND THE FACTORIZATION BY ANOTHER STEP. |
C        %--------------------------------------------------------%

      GO TO 1000

C     %---------------------------------------------------------------%
C     |                                                               |
C     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
C     |                                                               |
C     %---------------------------------------------------------------%

 9000 CONTINUE

C     %---------------%
C     | END OF DNAITR |
C     %---------------%

      END
