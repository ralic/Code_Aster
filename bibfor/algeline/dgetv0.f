      SUBROUTINE DGETV0
     &   ( IDO, BMAT, ITRY, INITV, N, J, V, LDV, RESID, RNORM,
     &     IPNTR, WORKD, IERR, ALPHA)
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE ARPACK GENERANT UN VECTEUR INITIAL DANS IM(OP).
C-----------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  GENERATE A RANDOM INITIAL RESIDUAL VECTOR FOR THE ARNOLDI PROCESS.
C  FORCE THE RESIDUAL VECTOR TO BE IN THE RANGE OF THE OPERATOR OP.
C
C ARGUMENTS
C  IDO     INTEGER.  (INPUT/OUTPUT)
C          REVERSE COMMUNICATION FLAG.  IDO MUST BE ZERO ON THE FIRST
C          CALL TO DGETV0.
C          ------------------------------------------------------------
C          IDO =  0: FIRST CALL TO THE REVERSE COMMUNICATION INTERFACE
C          IDO = -1: COMPUTE  Y = OP * X  WHERE
C                    IPNTR(1) IS THE POINTER INTO WORKD FOR X,
C                    IPNTR(2) IS THE POINTER INTO WORKD FOR Y.
C                    THIS IS FOR THE INITIALIZATION PHASE TO FORCE THE
C                    STARTING VECTOR INTO THE RANGE OF OP.
C          IDO =  2: COMPUTE  Y = B * X  WHERE
C                    IPNTR(1) IS THE POINTER INTO WORKD FOR X,
C                    IPNTR(2) IS THE POINTER INTO WORKD FOR Y.
C          IDO = 99: DONE
C          ------------------------------------------------------------
C
C  BMAT    CHARACTER*1.  (INPUT)
C          BMAT SPECIFIES THE TYPE OF THE MATRIX B IN THE (GENERALIZED)
C          EIGENVALUE PROBLEM A*X = LAMBDA*B*X.
C          B = 'I' -> STANDARD EIGENVALUE PROBLEM A*X = LAMBDA*X
C          B = 'G' -> GENERALIZED EIGENVALUE PROBLEM A*X = LAMBDA*B*X
C
C  ITRY    INTEGER.  (INPUT)
C          ITRY COUNTS THE NUMBER OF TIMES THAT DGETV0 IS CALLED.
C          IT SHOULD BE SET TO 1 ON THE INITIAL CALL TO DGETV0.
C
C  INITV   LOGICAL VARIABLE.  (INPUT)
C          .TRUE.  => THE INITIAL RESIDUAL VECTOR IS GIVEN IN RESID.
C          .FALSE. => GENERATE A RANDOM INITIAL RESIDUAL VECTOR.
C
C  N       INTEGER.  (INPUT)
C          DIMENSION OF THE PROBLEM.
C
C  J     INTEGER.  (INPUT)
C        INDEX OF THE RESIDUAL VECTOR TO BE GENERATED, WITH RESPECT TO
C        THE ARNOLDI PROCESS.  J > 1 IN CASE OF A "RESTART".
C
C  V     REAL*8 N BY J ARRAY.  (INPUT)
C        THE FIRST J-1 COLUMNS OF V CONTAIN THE CURRENT ARNOLDI BASIS
C        IF THIS IS A "RESTART".
C
C  LDV     INTEGER.  (INPUT)
C          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  RESID   REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C          INITIAL RESIDUAL VECTOR TO BE GENERATED.  IF RESID IS
C          PROVIDED, FORCE RESID INTO THE RANGE OF THE OPERATOR OP.
C
C  RNORM   REAL*8 SCALAR.  (OUTPUT)
C          B-NORM OF THE GENERATED RESIDUAL.
C
C  IPNTR   INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
C
C  WORKD   REAL*8 WORK ARRAY OF LENGTH 2*N.  (REVERSE COMMUNICATION).
C          ON EXIT, WORK(1:N) = B*RESID TO BE USED IN SSAITR.
C
C  IERR  INTEGER.  (OUTPUT)
C        =  0: NORMAL EXIT.
C        = -1: CANNOT GENERATE A NONTRIVIAL RESTARTED RESIDUAL VECTOR
C                IN THE RANGE OF THE OPERATOR OP.
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
C     DVOUT   ARPACK UTILITY ROUTINE FOR VECTOR OUTPUT.
C     FLARNV  LAPACK ROUTINE FOR GENERATING A RANDOM VECTOR.
C     BLGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
C     BLCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER.
C     BLSDOT    LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT .
C     BLNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
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
C FILE: GETV0.F   SID: 2.6   DATE OF SID: 8/27/96   RELEASE: 2
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE SECOND,
C            RAJOUT DE ALPHA,
C            COMMON TIMING REMPLACE PAR COMMON INFOR,
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
      LOGICAL INITV
      INTEGER IDO, IERR, ITRY, J, LDV, N
      REAL*8 RNORM, ALPHA

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      INTEGER IPNTR(3)
      REAL*8 RESID(N), V(LDV,J), WORKD(2*N)

C     %------------%
C     | PARAMETERS |
C     %------------%

      REAL*8 ONE, ZERO
      PARAMETER (ONE = 1.0D+0, ZERO = 0.0D+0)

C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%

      LOGICAL FIRST, INITS, ORTH
      INTEGER IDIST, ISEED(4), ITER, MSGLVL, JJ
      REAL*8 RNORM0
      SAVE FIRST, ISEED, INITS, ITER, MSGLVL, ORTH, RNORM0

C     %-----------%
C     | FUNCTIONS |
C     %-----------%

      REAL*8 BLSDOT, BLNRM2

C     %-----------------%
C     | DATA STATEMENTS |
C     %-----------------%

      DATA       INITS /.TRUE./

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

C     %-----------------------------------%
C     | INITIALIZE THE SEED OF THE LAPACK |
C     | RANDOM NUMBER GENERATOR           |
C     %-----------------------------------%

      IF (INITS) THEN
          ISEED(1) = 1
          ISEED(2) = 3
          ISEED(3) = 5
          ISEED(4) = 7
          INITS = .FALSE.
      END IF

      IF (IDO .EQ.  0) THEN

C        %-------------------------------%
C        | INITIALIZE TIMING STATISTICS  |
C        | & MESSAGE LEVEL FOR DEBUGGING |
C        %-------------------------------%
C
         MSGLVL = MGETV0
         IERR   = 0
         ITER   = 0
         FIRST  = .FALSE.
         ORTH   = .FALSE.

C        %-----------------------------------------------------%
C        | POSSIBLY GENERATE A RANDOM STARTING VECTOR IN RESID |
C        | USE A LAPACK RANDOM NUMBER GENERATOR USED BY THE    |
C        | MATRIX GENERATION ROUTINES.                         |
C        |    IDIST = 1: UNIFORM (0,1)  DISTRIBUTION,          |
C        |    IDIST = 2: UNIFORM (-1,1) DISTRIBUTION,          |
C        |    IDIST = 3: NORMAL  (0,1)  DISTRIBUTION,          |
C        %-----------------------------------------------------%

         IF (.NOT.INITV) THEN
            IDIST = 2
            CALL FLARNV (IDIST, ISEED, N, RESID)
         END IF

C        %----------------------------------------------------------%
C        | FORCE THE STARTING VECTOR INTO THE RANGE OF OP TO HANDLE |
C        | THE GENERALIZED PROBLEM WHEN B IS POSSIBLY (SINGULAR).   |
C        %----------------------------------------------------------%

         IF (BMAT .EQ. 'G') THEN
            NOPX = NOPX + 1
            IPNTR(1) = 1
            IPNTR(2) = N + 1
            CALL BLCOPY (N, RESID, 1, WORKD, 1)
            IDO = -1
            GO TO 9000
         END IF
      END IF

C     %-----------------------------------------%
C     | BACK FROM COMPUTING OP*(INITIAL-VECTOR) |
C     %-----------------------------------------%
C
      IF (FIRST) GO TO 20

C     %-----------------------------------------------%
C     | BACK FROM COMPUTING B*(ORTHOGONALIZED-VECTOR) |
C     %-----------------------------------------------%

      IF (ORTH)  GO TO 40

C     %------------------------------------------------------%
C     | STARTING VECTOR IS NOW IN THE RANGE OF OP, R = OP*R, |
C     | COMPUTE B-NORM OF STARTING VECTOR.                   |
C     %------------------------------------------------------%

      FIRST = .TRUE.
      IF (BMAT .EQ. 'G') THEN
         NBX = NBX + 1
         CALL BLCOPY (N, WORKD(N+1), 1, RESID, 1)
         IPNTR(1) = N + 1
         IPNTR(2) = 1
         IDO = 2
         GO TO 9000
      ELSE IF (BMAT .EQ. 'I') THEN
         CALL BLCOPY (N, RESID, 1, WORKD, 1)
      END IF

   20 CONTINUE

      FIRST = .FALSE.
      IF (BMAT .EQ. 'G') THEN
          RNORM0 = BLSDOT (N, RESID, 1, WORKD, 1)
          RNORM0 = SQRT(ABS(RNORM0))
      ELSE IF (BMAT .EQ. 'I') THEN
           RNORM0 = BLNRM2(N, RESID, 1)
      END IF
      RNORM  = RNORM0

C     %---------------------------------------------%
C     | EXIT IF THIS IS THE VERY FIRST ARNOLDI STEP |
C     %---------------------------------------------%

      IF (J .EQ. 1) GO TO 50

C     %----------------------------------------------------------------
C     | OTHERWISE NEED TO B-ORTHOGONALIZE THE STARTING VECTOR AGAINST |
C     | THE CURRENT ARNOLDI BASIS USING GRAM-SCHMIDT WITH ITER. REF.  |
C     | THIS IS THE CASE WHERE AN INVARIANT SUBSPACE IS ENCOUNTERED   |
C     | IN THE MIDDLE OF THE ARNOLDI FACTORIZATION.                   |
C     |                                                               |
C     |       S = VT(T)*B*R,   R = R - V*S,                           |
C     |                                                               |
C     | STOPPING CRITERIA USED FOR ITER. REF. IS DISCUSSED IN         |
C     | PARLETT'S BOOK, PAGE 107 AND IN GRAGG & REICHEL TOMS PAPER.   |
C     %---------------------------------------------------------------%

      ORTH = .TRUE.
   30 CONTINUE

      CALL BLGEMV ('T', N, J-1, ONE, V, LDV, WORKD, 1,
     &            ZERO, WORKD(N+1), 1)
      CALL BLGEMV ('N', N, J-1, -ONE, V, LDV, WORKD(N+1), 1,
     &            ONE, RESID, 1)

C     %----------------------------------------------------------%
C     | COMPUTE THE B-NORM OF THE ORTHOGONALIZED STARTING VECTOR |
C     %----------------------------------------------------------%

      IF (BMAT .EQ. 'G') THEN
         NBX = NBX + 1
         CALL BLCOPY (N, RESID, 1, WORKD(N+1), 1)
         IPNTR(1) = N + 1
         IPNTR(2) = 1
         IDO = 2
         GO TO 9000
      ELSE IF (BMAT .EQ. 'I') THEN
         CALL BLCOPY (N, RESID, 1, WORKD, 1)
      END IF

   40 CONTINUE

      IF (BMAT .EQ. 'G') THEN
         RNORM = BLSDOT (N, RESID, 1, WORKD, 1)
         RNORM = SQRT(ABS(RNORM))
      ELSE IF (BMAT .EQ. 'I') THEN
         RNORM = BLNRM2(N, RESID, 1)
      END IF

C     %--------------------------------------%
C     | CHECK FOR FURTHER ORTHOGONALIZATION. |
C     %--------------------------------------%

      IF (MSGLVL .GT. 2) THEN
          CALL DVOUT (LOGFIL, 1, RNORM0, NDIGIT,
     &                '_GETV0: RE-ORTHONALIZATION , RNORM0 IS')
          CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT,
     &                '_GETV0: RE-ORTHONALIZATION , RNORM IS')
      ENDIF
      IF (RNORM .GT. ALPHA*RNORM0) GO TO 50
      ITER = ITER + 1
      IF (ITER .LE. 5) THEN

C        %-----------------------------------%
C        | PERFORM ITERATIVE REFINEMENT STEP |
C        %-----------------------------------%

         RNORM0 = RNORM
         GO TO 30
      ELSE

C        %------------------------------------%
C        | ITERATIVE REFINEMENT STEP "FAILED" |
C        %------------------------------------%

         DO 45 JJ = 1, N
            RESID(JJ) = ZERO
   45    CONTINUE
         RNORM = ZERO
         IERR = -1
      END IF
C
   50 CONTINUE
C
      IF (MSGLVL .GT. 0) THEN
          CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT,
     &        '_GETV0: B-NORM OF INITIAL / RESTARTED STARTING VECTOR')
      END IF
      IF (MSGLVL .GT. 2) THEN
          CALL DVOUT (LOGFIL, N, RESID, NDIGIT,
     &        '_GETV0: INITIAL / RESTARTED STARTING VECTOR')
      ENDIF
      IDO = 99

 9000 CONTINUE

C     %---------------%
C     | END OF DGETV0 |
C     %---------------%

      END
