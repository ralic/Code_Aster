      SUBROUTINE DNAUP2
     &   ( IDO, BMAT, N, WHICH, NEV, NP, TOL, RESID, MODE, IUPD,
     &     ISHIFT, MXITER, V, LDV, H, LDH, RITZR, RITZI, BOUNDS,
     &     Q, LDQ, WORKL, IPNTR, WORKD, INFO, NEQACT, ALPHA)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C
C     SUBROUTINE ARPACK CALCULANT LES VALEURS PROPRES DE (OP) VIA
C     IRAM.
C---------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  INTERMEDIATE LEVEL INTERFACE CALLED BY DNAUPD.
C
C ARGUMENTS
C
C  IDO, BMAT, N, WHICH, NEV, TOL, RESID: SAME AS DEFINED IN DNAUPD.
C  MODE, ISHIFT, MXITER: SEE THE DEFINITION OF IPARAM IN DNAUPD.
C
C  NP    INTEGER.  (INPUT/OUTPUT)
C        CONTAINS THE NUMBER OF IMPLICIT SHIFTS TO APPLY DURING
C        EACH ARNOLDI ITERATION.
C        IF ISHIFT=1, NP IS ADJUSTED DYNAMICALLY AT EACH ITERATION
C        TO ACCELERATE CONVERGENCE AND PREVENT STAGNATION.
C        THIS IS ALSO ROUGHLY EQUAL TO THE NUMBER OF MATRIX-VECTOR
C        PRODUCTS (INVOLVING THE OPERATOR OP) PER ARNOLDI ITERATION.
C        THE LOGIC FOR ADJUSTING IS CONTAINED WITHIN THE CURRENT
C        SUBROUTINE.
C        IF ISHIFT=0, NP IS THE NUMBER OF SHIFTS THE USER NEEDS
C        TO PROVIDE VIA REVERSE COMUNICATION. 0 < NP < NCV-NEV.
C        NP MAY BE LESS THAN NCV-NEV FOR TWO REASONS. THE FIRST, IS
C        TO KEEP COMPLEX CONJUGATE PAIRS OF "WANTED" RITZ VALUES
C        TOGETHER. THE SECOND, IS THAT A LEADING BLOCK OF THE CURRENT
C        UPPER HESSENBERG MATRIX HAS SPLIT OFF AND CONTAINS "UNWANTED"
C        RITZ VALUES.
C        UPON TERMINATION OF THE IRA ITERATION, NP CONTAINS NUMBER
C        OF "CONVERGED" WANTED RITZ VALUES.
C
C  IUPD  INTEGER.  (INPUT)
C        IUPD .EQ. 0: USE EXPLICIT RESTART INSTEAD IMPLICIT UPDATE.
C        IUPD .NE. 0: USE IMPLICIT UPDATE.
C
C  V     DOUBLE PRECISION N BY (NEV+NP) ARRAY.  (INPUT/OUTPUT)
C        THE ARNOLDI BASIS VECTORS ARE RETURNED IN THE FIRST NEV
C        COLUMNS OF V.
C
C  LDV   INTEGER.  (INPUT)
C        LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
C        PROGRAM.
C
C  H     DOUBLE PRECISION (NEV+NP) BY (NEV+NP) ARRAY.  (OUTPUT)
C        H IS USED TO STORE THE GENERATED UPPER HESSENBERG MATRIX
C
C  LDH   INTEGER.  (INPUT)
C        LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
C        PROGRAM.
C
C  RITZR, DOUBLE PRECISION ARRAYS OF LENGTH NEV+NP.  (OUTPUT)
C  RITZI  RITZR(1:NEV) (RESP. RITZI(1:NEV)) CONTAINS THE REAL (RESP.
C         IMAGINARY) PART OF THE COMPUTED RITZ VALUES OF OP.
C
C  BOUNDS DOUBLE PRECISION ARRAY OF LENGTH NEV+NP.  (OUTPUT)
C         BOUNDS(1:NEV) CONTAIN THE ERROR BOUNDS CORRESPONDING TO
C         THE COMPUTED RITZ VALUES.
C
C  Q     DOUBLE PRECISION (NEV+NP) BY (NEV+NP) ARRAY.  (WORKSPACE)
C        PRIVATE (REPLICATED) WORK ARRAY USED TO ACCUMULATE THE
C        ROTATION IN THE SHIFT APPLICATION STEP.
C
C  LDQ   INTEGER.  (INPUT)
C        LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
C        PROGRAM.
C
C  WORKL DOUBLE PRECISION WORK ARRAY OF LENGTH AT LEAST
C        (NEV+NP)**2 + 3*(NEV+NP).  (INPUT/WORKSPACE)
C        PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
C        THE FRONT END.  IT IS USED IN SHIFTS CALCULATION, SHIFTS
C        APPLICATION AND CONVERGENCE CHECKING.
C
C        ON EXIT, THE LAST 3*(NEV+NP) LOCATIONS OF WORKL CONTAIN
C        THE RITZ VALUES (REAL,IMAGINARY) AND ASSOCIATED RITZ
C        ESTIMATES OF THE CURRENT HESSENBERG MATRIX.  THEY ARE
C        LISTED IN THE SAME ORDER AS RETURNED FROM DNEIGH.
C
C        IF ISHIFT .EQ. O AND IDO .EQ. 3, THE FIRST 2*NP LOCATIONS
C        OF WORKL ARE USED IN REVERSE COMMUNICATION TO HOLD THE USER
C        SUPPLIED SHIFTS.
C
C  IPNTR INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
C        POINTER TO MARK THE STARTING LOCATIONS IN THE WORKD FOR
C        VECTORS USED BY THE ARNOLDI ITERATION.
C        ----------------------------------------------------------
C        IPNTR(1): POINTER TO THE CURRENT OPERAND VECTOR X.
C        IPNTR(2): POINTER TO THE CURRENT RESULT VECTOR Y.
C        IPNTR(3): POINTER TO THE VECTOR B * X WHEN USED IN THE
C                  SHIFT-AND-INVERT MODE.  X IS THE CURRENT OPERAND.
C        ----------------------------------------------------------
C
C  WORKD DOUBLE PRECISION WORK ARRAY OF LENGTH 3*N.  (WORKSPACE)
C        DISTRIBUTED ARRAY TO BE USED IN THE BASIC ARNOLDI ITERATION
C        FOR REVERSE COMMUNICATION.  THE USER SHOULD NOT USE WORKD
C        AS TEMPORARY WORKSPACE DURING THE ITERATION !!!!!!!!!!
C        SEE DATA DISTRIBUTION NOTE IN DNAUPD.
C
C  INFO  INTEGER.  (INPUT/OUTPUT)
C        IF INFO .EQ. 0, A RANDOMLY INITIAL RESIDUAL VECTOR IS USED.
C        IF INFO .NE. 0, RESID CONTAINS THE INITIAL RESIDUAL VECTOR,
C                        POSSIBLY FROM A PREVIOUS RUN.
C        ERROR FLAG ON OUTPUT.
C        =     0: NORMAL RETURN.
C        =     1: MAXIMUM NUMBER OF ITERATIONS TAKEN.
C                 ALL POSSIBLE EIGENVALUES OF OP HAS BEEN FOUND.
C                 NP RETURNS THE NUMBER OF CONVERGED RITZ VALUES.
C        =     2: NO SHIFTS COULD BE APPLIED.
C        =    -8: ERROR RETURN FROM LAPACK EIGENVALUE CALCULATION,
C                 THIS SHOULD NEVER HAPPEN.
C        =    -9: STARTING VECTOR IS ZERO.
C        = -9999: COULD NOT BUILD AN ARNOLDI FACTORIZATION.
C                 SIZE THAT WAS BUILT IN RETURNED IN NP.
C
C  NEQACT INTEGER  (INPUT/ NEW PARAMETER INTRODUCED BY ASTER)
C         NUMBER OF PHYSICAL DEGREE OF FREEDOM
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
C     DGETV0  ARPACK INITIAL VECTOR GENERATION ROUTINE.
C     DNAITR  ARPACK ARNOLDI FACTORIZATION ROUTINE.
C     DNAPPS  ARPACK APPLICATION OF IMPLICIT SHIFTS ROUTINE.
C     DNCONV  ARPACK CONVERGENCE OF RITZ VALUES ROUTINE.
C     DNEIGH  ARPACK COMPUTE RITZ VALUES AND ERROR BOUNDS ROUTINE.
C     DNGETS  ARPACK REORDER RITZ VALUES AND ERROR BOUNDS ROUTINE.
C     DSORTC  ARPACK SORTING ROUTINE.
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     FLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C     BLCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     BLSDOT    LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT.
C     BLNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
C     BLSWAP   LEVEL 1 BLAS THAT SWAPS TWO VECTORS.
C
C     R8PREM  ASTER UTILITY ROUTINE THAT GIVES MACHINE PRECISION
C
C INTRINSIC FUNCTIONS
C
C     MIN, MAX, ABS, SQRT
C
C AUTHOR
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
C     APPLIED MATHEMATICS
C     RICE UNIVERSITY
C     HOUSTON, TEXAS
C
C FILE: NAUP2.F   SID: 2.4   DATE OF SID: 7/30/96   RELEASE: 2
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE SECOND ET DLAMCH,
C            COMMON TIMING REMPLACE PAR COMMON INFOR,
C            RAJOUT DU PARAMETRE NEQACT,
C            NOUVEAU MSG POUR ESPACE INVARIANT,
C            UTILISATION DE R8PREM(),
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
      CHARACTER*2 WHICH
      INTEGER IDO, INFO, ISHIFT, IUPD, MODE, LDH, LDQ, LDV, MXITER,
     &  N, NEV, NP, NEQACT
      REAL*8 TOL, ALPHA

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      INTEGER IPNTR(13)
      REAL*8 BOUNDS(NEV+NP), H(LDH,NEV+NP), Q(LDQ,NEV+NP), RESID(N),
     &  RITZI(NEV+NP), RITZR(NEV+NP), V(LDV,NEV+NP),
     &  WORKD(3*N), WORKL( (NEV+NP)*(NEV+NP+3) )

C     %------------%
C     | PARAMETERS |
C     %------------%

      REAL*8 ZERO
      PARAMETER (ZERO = 0.0D+0)

C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%

      CHARACTER*2 WPRIME
      LOGICAL CNORM, GETV0, INITV, UPDATE, USHIFT
      INTEGER IERR, ITER, J, KPLUSP, MSGLVL, NCONV, NEVBEF, NEV0,
     &  NP0, NPTEMP, NUMCNV
      REAL*8 RNORM, TEMP, EPS23

C     %-----------------------%
C     | LOCAL ARRAY ARGUMENTS |
C     %-----------------------%

      INTEGER KP(4)
      SAVE

C     %-----------%
C     | FUNCTIONS |
C     %-----------%

      REAL*8 BLSDOT, BLNRM2, FLAPY2, R8PREM

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

      IF (IDO .EQ. 0) THEN

         MSGLVL = MNAUP2

C        %-------------------------------------%
C        | GET THE MACHINE DEPENDENT CONSTANT. |
C        %-------------------------------------%

         EPS23 = R8PREM()*0.5D0
         EPS23 = EPS23**(2.0D+0 / 3.0D+0)

         NEV0   = NEV
         NP0    = NP

C        %-------------------------------------%
C        | KPLUSP IS THE BOUND ON THE LARGEST  |
C        |        LANCZOS FACTORIZATION BUILT. |
C        | NCONV IS THE CURRENT NUMBER OF      |
C        |        "CONVERGED" EIGENVLUES.      |
C        | ITER IS THE COUNTER ON THE CURRENT  |
C        |      ITERATION STEP.                |
C        %-------------------------------------%

         KPLUSP = NEV + NP
         NCONV  = 0
         ITER   = 0

C        %---------------------------------------%
C        | SET FLAGS FOR COMPUTING THE FIRST NEV |
C        | STEPS OF THE ARNOLDI FACTORIZATION.   |
C        %---------------------------------------%

         GETV0    = .TRUE.
         UPDATE   = .FALSE.
         USHIFT   = .FALSE.
         CNORM    = .FALSE.

         IF (INFO .NE. 0) THEN

C           %--------------------------------------------%
C           | USER PROVIDES THE INITIAL RESIDUAL VECTOR. |
C           %--------------------------------------------%

            INITV = .TRUE.
            INFO  = 0
         ELSE
            INITV = .FALSE.
         END IF
      END IF

C     %---------------------------------------------%
C     | GET A POSSIBLY RANDOM STARTING VECTOR AND   |
C     | FORCE IT INTO THE RANGE OF THE OPERATOR OP. |
C     %---------------------------------------------%

   10 CONTINUE

      IF (GETV0) THEN
         CALL DGETV0 (IDO, BMAT, 1, INITV, N, 1, V, LDV, RESID, RNORM,
     &                IPNTR, WORKD, INFO, ALPHA)
         IF (IDO .NE. 99) GO TO 9000
         IF (RNORM .EQ. ZERO) THEN

C           %-----------------------------------------%
C           | THE INITIAL VECTOR IS ZERO. ERROR EXIT. |
C           %-----------------------------------------%

            INFO = -9
            GO TO 1100
         END IF
         GETV0 = .FALSE.
         IDO  = 0
      END IF

C     %-----------------------------------%
C     | BACK FROM REVERSE COMMUNICATION : |
C     | CONTINUE WITH UPDATE STEP         |
C     %-----------------------------------%

      IF (UPDATE) GO TO 20

C     %-------------------------------------------%
C     | BACK FROM COMPUTING USER SPECIFIED SHIFTS |
C     %-------------------------------------------%

      IF (USHIFT) GO TO 50

C     %-------------------------------------%
C     | BACK FROM COMPUTING RESIDUAL NORM   |
C     | AT THE END OF THE CURRENT ITERATION |
C     %-------------------------------------%

      IF (CNORM)  GO TO 100

C     %----------------------------------------------------------%
C     | COMPUTE THE FIRST NEV STEPS OF THE ARNOLDI FACTORIZATION |
C     %----------------------------------------------------------%

      CALL DNAITR (IDO, BMAT, N, 0, NEV, MODE, RESID, RNORM, V, LDV,
     &             H, LDH, IPNTR, WORKD, INFO, ALPHA)

C     %---------------------------------------------------%
C     | IDO .NE. 99 IMPLIES USE OF REVERSE COMMUNICATION  |
C     | TO COMPUTE OPERATIONS INVOLVING OP AND POSSIBLY B |
C     %---------------------------------------------------%

      IF (IDO .NE. 99) GO TO 9000
      IF (INFO .GT. 0) THEN
         NP   = INFO
         MXITER = ITER
         INFO = -9999
         GO TO 1200
      END IF

C     %--------------------------------------------------------------%
C     |                                                              |
C     |           M A I N  ARNOLDI  I T E R A T I O N  L O O P       |
C     |           EACH ITERATION IMPLICITLY RESTARTS THE ARNOLDI     |
C     |           FACTORIZATION IN PLACE.                            |
C     |                                                              |
C     %--------------------------------------------------------------%

 1000 CONTINUE

         ITER = ITER + 1
         IF (MSGLVL .GT. 0) THEN
             CALL IVOUT (LOGFIL, 1, ITER, NDIGIT,
     &           '_NAUP2: **** START OF MAJOR ITERATION NUMBER ****')
         END IF

C        %-----------------------------------------------------------%
C        | COMPUTE NP ADDITIONAL STEPS OF THE ARNOLDI FACTORIZATION. |
C        | ADJUST NP SINCE NEV MIGHT HAVE BEEN UPDATED BY LAST CALL  |
C        | TO THE SHIFT APPLICATION ROUTINE DNAPPS.                  |
C        %-----------------------------------------------------------%

         NP  = KPLUSP - NEV
         IF (MSGLVL .GT. 1) THEN
             CALL IVOUT (LOGFIL, 1, NEV, NDIGIT,
     &       '_NAUP2: THE LENGTH OF THE CURRENT ARNOLDI FACTORIZATION')
             CALL IVOUT (LOGFIL, 1, NP, NDIGIT,
     &       '_NAUP2: EXTEND THE ARNOLDI FACTORIZATION BY')
         ENDIF

C        %-----------------------------------------------------------%
C        | COMPUTE NP ADDITIONAL STEPS OF THE ARNOLDI FACTORIZATION. |
C        %-----------------------------------------------------------%

         IDO = 0
   20    CONTINUE
         UPDATE = .TRUE.
         CALL DNAITR (IDO, BMAT, N, NEV, NP, MODE, RESID, RNORM, V, LDV,
     &                H, LDH, IPNTR, WORKD, INFO, ALPHA)

C        %---------------------------------------------------%
C        | IDO .NE. 99 IMPLIES USE OF REVERSE COMMUNICATION  |
C        | TO COMPUTE OPERATIONS INVOLVING OP AND POSSIBLY B |
C        %---------------------------------------------------%

         IF (IDO .NE. 99) GO TO 9000
         IF (INFO .GT. 0) THEN
           NP = INFO
           MXITER = ITER
           IF (INFO.GE.NEQACT) THEN
             IF (MSGLVL.GT.0) THEN
               WRITE(LOGFIL,*)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)'& ESPACE INVARIANT DE TAILLE &'
               WRITE(LOGFIL,*)'& NEQACT = ',NEQACT
               WRITE(LOGFIL,*)'& SHUNTAGE PARTIEL DE DNAUP2 &'
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)
             ENDIF
           ELSE
             INFO = -9999
             GO TO 1200
           ENDIF
         END IF
         UPDATE = .FALSE.

         IF (MSGLVL .GT. 1) THEN
             CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT,
     &           '_NAUP2: CORRESPONDING B-NORM OF THE RESIDUAL')
         ENDIF

C        %--------------------------------------------------------%
C        | COMPUTE THE EIGENVALUES AND CORRESPONDING ERROR BOUNDS |
C        | OF THE CURRENT UPPER HESSENBERG MATRIX.                |
C        %--------------------------------------------------------%

         CALL DNEIGH (RNORM, KPLUSP, H, LDH, RITZR, RITZI, BOUNDS,
     &                Q, LDQ, WORKL, IERR)

         IF (IERR .NE. 0) THEN
            INFO = -8
            GO TO 1200
         END IF

C        %----------------------------------------------------%
C        | MAKE A COPY OF EIGENVALUES AND CORRESPONDING ERROR |
C        | BOUNDS OBTAINED FROM DNEIGH.                       |
C        %----------------------------------------------------%

         CALL BLCOPY(KPLUSP, RITZR, 1, WORKL(KPLUSP**2+1), 1)
         CALL BLCOPY(KPLUSP, RITZI, 1, WORKL(KPLUSP**2+KPLUSP+1), 1)
         CALL BLCOPY(KPLUSP, BOUNDS, 1, WORKL(KPLUSP**2+2*KPLUSP+1), 1)

C        %---------------------------------------------------%
C        | SELECT THE WANTED RITZ VALUES AND THEIR BOUNDS    |
C        | TO BE USED IN THE CONVERGENCE TEST.               |
C        | THE WANTED PART OF THE SPECTRUM AND CORRESPONDING |
C        | ERROR BOUNDS ARE IN THE LAST NEV LOC. OF RITZR,   |
C        | RITZI AND BOUNDS RESPECTIVELY. THE VARIABLES NEV  |
C        | AND NP MAY BE UPDATED IF THE NEV-TH WANTED RITZ   |
C        | VALUE HAS A NON ZERO IMAGINARY PART. IN THIS CASE |
C        | NEV IS INCREASED BY ONE AND NP DECREASED BY ONE.  |
C        | NOTE: THE LAST TWO ARGUMENTS OF DNGETS ARE NO     |
C        | LONGER USED AS OF VERSION 2.1.                    |
C        %---------------------------------------------------%

         NEV = NEV0
         NP = NP0
         NUMCNV = NEV
         CALL DNGETS (ISHIFT, WHICH, NEV, NP, RITZR, RITZI,
     &                BOUNDS, WORKL, WORKL(NP+1))
         IF (NEV .EQ. NEV0+1) NUMCNV = NEV0+1

C        %-------------------%
C        | CONVERGENCE TEST. |
C        %-------------------%

         CALL BLCOPY (NEV, BOUNDS(NP+1), 1, WORKL(2*NP+1), 1)
         CALL DNCONV (NEV, RITZR(NP+1), RITZI(NP+1), WORKL(2*NP+1),
     &        TOL, NCONV)

         IF (MSGLVL .GT. 2) THEN
             KP(1) = NEV
             KP(2) = NP
             KP(3) = NUMCNV
             KP(4) = NCONV
             CALL IVOUT (LOGFIL, 4, KP, NDIGIT,
     &                  '_NAUP2: NEV, NP, NUMCNV, NCONV ARE')
             CALL DVOUT (LOGFIL, KPLUSP, RITZR, NDIGIT,
     &           '_NAUP2: REAL PART OF THE EIGENVALUES OF H')
             CALL DVOUT (LOGFIL, KPLUSP, RITZI, NDIGIT,
     &           '_NAUP2: IMAGINARY PART OF THE EIGENVALUES OF H')
             CALL DVOUT (LOGFIL, KPLUSP, BOUNDS, NDIGIT,
     &          '_NAUP2: RITZ ESTIMATES OF THE CURRENT NCV RITZ VALUES')
         ENDIF

C        %---------------------------------------------------------%
C        | COUNT THE NUMBER OF UNWANTED RITZ VALUES THAT HAVE ZERO |
C        | RITZ ESTIMATES. IF ANY RITZ ESTIMATES ARE EQUAL TO ZERO |
C        | THEN A LEADING BLOCK OF H OF ORDER EQUAL TO AT LEAST    |
C        | THE NUMBER OF RITZ VALUES WITH ZERO RITZ ESTIMATES HAS  |
C        | SPLIT OFF. NONE OF THESE RITZ VALUES MAY BE REMOVED BY  |
C        | SHIFTING. DECREASE NP THE NUMBER OF SHIFTS TO APPLY. IF |
C        | NO SHIFTS MAY BE APPLIED, THEN PREPARE TO EXIT          |
C        %---------------------------------------------------------%

         NPTEMP = NP
         DO 30 J=1, NPTEMP
            IF (BOUNDS(J) .EQ. ZERO) THEN
               NP = NP - 1
               NEV = NEV + 1
            END IF
 30      CONTINUE
C
         IF ( (NCONV .GE. NUMCNV) .OR.
     &        (ITER .GT. MXITER) .OR.
     &        (NP .EQ. 0) ) THEN
C
            IF (MSGLVL .GT. 4) THEN
                CALL DVOUT(LOGFIL, KPLUSP, WORKL(KPLUSP**2+1), NDIGIT,
     &             '_NAUP2: REAL PART OF THE EIG COMPUTED BY _NEIGH:')
                CALL DVOUT(LOGFIL, KPLUSP, WORKL(KPLUSP**2+KPLUSP+1),
     &                     NDIGIT,
     &             '_NAUP2: IMAG PART OF THE EIG COMPUTED BY _NEIGH:')
                CALL DVOUT(LOGFIL, KPLUSP, WORKL(KPLUSP**2+KPLUSP*2+1),
     &                     NDIGIT,
     &             '_NAUP2: RITZ EISTMATES COMPUTED BY _NEIGH:')
            ENDIF

C           %------------------------------------------------%
C           | PREPARE TO EXIT. PUT THE CONVERGED RITZ VALUES |
C           | AND CORRESPONDING BOUNDS IN RITZ(1:NCONV) AND  |
C           | BOUNDS(1:NCONV) RESPECTIVELY. THEN SORT. BE    |
C           | CAREFUL WHEN NCONV > NP                        |
C           %------------------------------------------------%

C           %------------------------------------------%
C           |  USE H( 3,1 ) AS STORAGE TO COMMUNICATE  |
C           |  RNORM TO _NEUPD IF NEEDED               |
C           %------------------------------------------%

            H(3,1) = RNORM

C           %----------------------------------------------%
C           | TO BE CONSISTENT WITH DNGETS, WE FIRST DO A  |
C           | PRE-PROCESSING SORT IN ORDER TO KEEP COMPLEX |
C           | CONJUGATE PAIRS TOGETHER.  THIS IS SIMILAR   |
C           | TO THE PRE-PROCESSING SORT USED IN DNGETS    |
C           | EXCEPT THAT THE SORT IS DONE IN THE OPPOSITE |
C           | ORDER.                                       |
C           %----------------------------------------------%

            IF (WHICH .EQ. 'LM') WPRIME = 'SR'
            IF (WHICH .EQ. 'SM') WPRIME = 'LR'
            IF (WHICH .EQ. 'LR') WPRIME = 'SM'
            IF (WHICH .EQ. 'SR') WPRIME = 'LM'
            IF (WHICH .EQ. 'LI') WPRIME = 'SM'
            IF (WHICH .EQ. 'SI') WPRIME = 'LM'

            CALL DSORTC (WPRIME, .TRUE., KPLUSP, RITZR, RITZI, BOUNDS)

C           %----------------------------------------------%
C           | NOW SORT RITZ VALUES SO THAT CONVERGED RITZ  |
C           | VALUES APPEAR WITHIN THE FIRST NEV LOCATIONS |
C           | OF RITZR, RITZI AND BOUNDS, AND THE MOST     |
C           | DESIRED ONE APPEARS AT THE FRONT.            |
C           %----------------------------------------------%

            IF (WHICH .EQ. 'LM') WPRIME = 'SM'
            IF (WHICH .EQ. 'SM') WPRIME = 'LM'
            IF (WHICH .EQ. 'LR') WPRIME = 'SR'
            IF (WHICH .EQ. 'SR') WPRIME = 'LR'
            IF (WHICH .EQ. 'LI') WPRIME = 'SI'
            IF (WHICH .EQ. 'SI') WPRIME = 'LI'

            CALL DSORTC(WPRIME, .TRUE., KPLUSP, RITZR, RITZI, BOUNDS)

C           %--------------------------------------------------%
C           | SCALE THE RITZ ESTIMATE OF EACH RITZ VALUE       |
C           | BY 1 / MAX(EPS23,MAGNITUDE OF THE RITZ VALUE).   |
C           %--------------------------------------------------%

            DO 35 J = 1, NEV0
                TEMP = MAX(EPS23,FLAPY2(RITZR(J),
     &                                   RITZI(J)))
                BOUNDS(J) = BOUNDS(J)/TEMP
 35         CONTINUE

C           %----------------------------------------------------%
C           | SORT THE RITZ VALUES ACCORDING TO THE SCALED RITZ  |
C           | ESITMATES.  THIS WILL PUSH ALL THE CONVERGED ONES  |
C           | TOWARDS THE FRONT OF RITZR, RITZI, BOUNDS          |
C           | (IN THE CASE WHEN NCONV < NEV.)                    |
C           %----------------------------------------------------%

            WPRIME = 'LR'
            CALL DSORTC(WPRIME, .TRUE., NEV0, BOUNDS, RITZR, RITZI)

C           %----------------------------------------------%
C           | SCALE THE RITZ ESTIMATE BACK TO ITS ORIGINAL |
C           | VALUE.                                       |
C           %----------------------------------------------%

            DO 40 J = 1, NEV0
                TEMP = MAX(EPS23, FLAPY2(RITZR(J),
     &                                   RITZI(J)))
                BOUNDS(J) = BOUNDS(J)*TEMP
 40         CONTINUE

C           %------------------------------------------------%
C           | SORT THE CONVERGED RITZ VALUES AGAIN SO THAT   |
C           | THE "THRESHOLD" VALUE APPEARS AT THE FRONT OF  |
C           | RITZR, RITZI AND BOUND.                        |
C           %------------------------------------------------%

            CALL DSORTC(WHICH, .TRUE., NCONV, RITZR, RITZI, BOUNDS)

            IF (MSGLVL .GT. 1) THEN
                CALL DVOUT (LOGFIL, KPLUSP, RITZR, NDIGIT,
     &            '_NAUP2: SORTED REAL PART OF THE EIGENVALUES')
                CALL DVOUT (LOGFIL, KPLUSP, RITZI, NDIGIT,
     &            '_NAUP2: SORTED IMAGINARY PART OF THE EIGENVALUES')
                CALL DVOUT (LOGFIL, KPLUSP, BOUNDS, NDIGIT,
     &            '_NAUP2: SORTED RITZ ESTIMATES.')
            ENDIF
C
C           %------------------------------------%
C           | MAX ITERATIONS HAVE BEEN EXCEEDED. |
C           %------------------------------------%
C
            IF (ITER .GT. MXITER .AND. NCONV .LT. NUMCNV) INFO = 1
C
C           %---------------------%
C           | NO SHIFTS TO APPLY. |
C           %---------------------%
C
            IF (NP .EQ. 0 .AND. NCONV .LT. NUMCNV) INFO = 2
C
            NP = NCONV
            GO TO 1100

         ELSE IF ( (NCONV .LT. NUMCNV) .AND. (ISHIFT .EQ. 1) ) THEN

C           %-------------------------------------------------%
C           | DO NOT HAVE ALL THE REQUESTED EIGENVALUES YET.  |
C           | TO PREVENT POSSIBLE STAGNATION, ADJUST THE SIZE |
C           | OF NEV.                                         |
C           %-------------------------------------------------%

            NEVBEF = NEV
            NEV = NEV + MIN(NCONV, NP/2)
            IF (NEV .EQ. 1 .AND. KPLUSP .GE. 6) THEN
               NEV = KPLUSP / 2
            ELSE IF (NEV .EQ. 1 .AND. KPLUSP .GT. 3) THEN
               NEV = 2
            END IF
            NP = KPLUSP - NEV

C           %---------------------------------------%
C           | IF THE SIZE OF NEV WAS JUST INCREASED |
C           | RESORT THE EIGENVALUES.               |
C           %---------------------------------------%

            IF (NEVBEF .LT. NEV)
     &         CALL DNGETS (ISHIFT, WHICH, NEV, NP, RITZR, RITZI,
     &              BOUNDS, WORKL, WORKL(NP+1))

         END IF

         IF (MSGLVL .GT. 0) THEN
             CALL IVOUT (LOGFIL, 1, NCONV, NDIGIT,
     &           '_NAUP2: NO. OF "CONVERGED" RITZ VALUES AT THIS ITER.')
             IF (MSGLVL .GT. 1) THEN
               KP(1) = NEV
               KP(2) = NP
               CALL IVOUT (LOGFIL, 2, KP, NDIGIT,
     &              '_NAUP2: NEV AND NP ARE')
               CALL DVOUT (LOGFIL, NEV, RITZR(NP+1), NDIGIT,
     &              '_NAUP2: "WANTED" RITZ VALUES -- REAL PART')
               CALL DVOUT (LOGFIL, NEV, RITZI(NP+1), NDIGIT,
     &              '_NAUP2: "WANTED" RITZ VALUES -- IMAG PART')
               CALL DVOUT (LOGFIL, NEV, BOUNDS(NP+1), NDIGIT,
     &              '_NAUP2: RITZ ESTIMATES OF THE "WANTED" VALUES ')
             ENDIF
         ENDIF

         IF (ISHIFT .EQ. 0) THEN

C           %-------------------------------------------------------%
C           | USER SPECIFIED SHIFTS: REVERSE COMMINUCATION TO       |
C           | COMPUTE THE SHIFTS. THEY ARE RETURNED IN THE FIRST    |
C           | 2*NP LOCATIONS OF WORKL.                              |
C           %-------------------------------------------------------%

            USHIFT = .TRUE.
            IDO = 3
            GO TO 9000
         END IF

   50    CONTINUE

C        %------------------------------------%
C        | BACK FROM REVERSE COMMUNICATION,   |
C        | USER SPECIFIED SHIFTS ARE RETURNED |
C        | IN WORKL(1:2*NP)                   |
C        %------------------------------------%

         USHIFT = .FALSE.
         IF ( ISHIFT .EQ. 0 ) THEN

C            %----------------------------------%
C            | MOVE THE NP SHIFTS FROM WORKL TO |
C            | RITZR, RITZI TO FREE UP WORKL    |
C            | FOR NON-EXACT SHIFT CASE.        |
C            %----------------------------------%

             CALL BLCOPY (NP, WORKL,       1, RITZR, 1)
             CALL BLCOPY (NP, WORKL(NP+1), 1, RITZI, 1)
         END IF

         IF (MSGLVL .GT. 2) THEN
             CALL IVOUT (LOGFIL, 1, NP, NDIGIT,
     &                  '_NAUP2: THE NUMBER OF SHIFTS TO APPLY ')
             CALL DVOUT (LOGFIL, NP, RITZR, NDIGIT,
     &                  '_NAUP2: REAL PART OF THE SHIFTS')
             CALL DVOUT (LOGFIL, NP, RITZI, NDIGIT,
     &                  '_NAUP2: IMAGINARY PART OF THE SHIFTS')
             IF ( ISHIFT .EQ. 1 )
     &          CALL DVOUT (LOGFIL, NP, BOUNDS, NDIGIT,
     &                  '_NAUP2: RITZ ESTIMATES OF THE SHIFTS')
         ENDIF

C        %---------------------------------------------------------%
C        | APPLY THE NP IMPLICIT SHIFTS BY QR BULGE CHASING.       |
C        | EACH SHIFT IS APPLIED TO THE WHOLE UPPER HESSENBERG     |
C        | MATRIX H.                                               |
C        | THE FIRST 2*N LOCATIONS OF WORKD ARE USED AS WORKSPACE. |
C        %---------------------------------------------------------%

         CALL DNAPPS (N, NEV, NP, RITZR, RITZI, V, LDV,
     &                H, LDH, RESID, Q, LDQ, WORKL, WORKD)

C        %---------------------------------------------%
C        | COMPUTE THE B-NORM OF THE UPDATED RESIDUAL. |
C        | KEEP B*RESID IN WORKD(1:N) TO BE USED IN    |
C        | THE FIRST STEP OF THE NEXT CALL TO DNAITR.  |
C        %---------------------------------------------%

         CNORM = .TRUE.
         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            CALL BLCOPY (N, RESID, 1, WORKD(N+1), 1)
            IPNTR(1) = N + 1
            IPNTR(2) = 1
            IDO = 2
C
C           %----------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*RESID |
C           %----------------------------------%

            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL BLCOPY (N, RESID, 1, WORKD, 1)
         END IF

  100    CONTINUE

C        %----------------------------------%
C        | BACK FROM REVERSE COMMUNICATION, |
C        | WORKD(1:N) := B*RESID            |
C        %----------------------------------%

         IF (BMAT .EQ. 'G') THEN
            RNORM = BLSDOT (N, RESID, 1, WORKD, 1)
            RNORM = SQRT(ABS(RNORM))
         ELSE IF (BMAT .EQ. 'I') THEN
            RNORM = BLNRM2(N, RESID, 1)
         END IF
         CNORM = .FALSE.

         IF (MSGLVL .GT. 2) THEN
             CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT,
     &       '_NAUP2: B-NORM OF RESIDUAL FOR COMPRESSED FACTORIZATION')
             CALL DMOUT (LOGFIL, NEV, NEV, H, LDH, NDIGIT,
     &        '_NAUP2: COMPRESSED UPPER HESSENBERG MATRIX H')
         ENDIF

      GO TO 1000

C     %---------------------------------------------------------------%
C     |                                                               |
C     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
C     |                                                               |
C     %---------------------------------------------------------------%

 1100 CONTINUE

      MXITER = ITER
      NEV = NUMCNV

 1200 CONTINUE
      IDO = 99


 9000 CONTINUE

C     %---------------%
C     | END OF DNAUP2 |
C     %---------------%

      END
