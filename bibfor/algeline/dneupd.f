      SUBROUTINE DNEUPD
     &  (RVEC, HOWMNY, SELECT, DR, DI, Z, LDZ, SIGMAR, SIGMAI, WORKEV,
     &   BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR,
     &   WORKD, WORKL, LWORKL, INFO)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C
C     SUBROUTINE ARPACK CALCULANT LES MODES PROPRES DU PROBLEME
C     INITIAL.
C---------------------------------------------------------------------
C BEGINDOC
C
C NAME: DNEUPD
C
C DESCRIPTION:
C
C  THIS SUBROUTINE RETURNS THE CONVERGED APPROXIMATIONS TO EIGENVALUES
C  OF A*Z = LAMBDA*B*Z AND (OPTIONALLY):
C
C      (1) THE CORRESPONDING APPROXIMATE EIGENVECTORS,
C
C      (2) AN ORTHONORMAL BASIS FOR THE ASSOCIATED APPROXIMATE
C          INVARIANT SUBSPACE,
C
C      (3) BOTH.
C
C  THERE IS NEGLIGIBLE ADDITIONAL COST TO OBTAIN EIGENVECTORS. AN
C  ORTHONORMAL BASIS IS ALWAYS COMPUTED.
C  THERE IS AN ADDITIONAL STORAGE COST OF N*NEV IF BOTH ARE REQUESTED
C  (IN THIS CASE A SEPARATE ARRAY Z MUST BE SUPPLIED).
C
C  THE APPROXIMATE EIGENVALUES AND EIGENVECTORS OF  A*Z = LAMBDA*B*Z
C  ARE DERIVED FROM APPROXIMATE EIGENVALUES AND EIGENVECTORS OF
C  OF THE LINEAR OPERATOR OP PRESCRIBED BY THE MODE SELECTION IN THE
C  CALL TO DNAUPD. DNAUPD MUST BE CALLED BEFORE THIS ROUTINE IS CALLED.
C  THESE APPROXIMATE EIGENVALUES AND VECTORS ARE COMMONLY CALLED RITZ
C  VALUES AND RITZ VECTORS RESPECTIVELY.  THEY ARE REFERRED TO AS SUCH
C  IN THE COMMENTS THAT FOLLOW. THE COMPUTED ORTHONORMAL BASIS FOR THE
C  INVARIANT SUBSPACE CORRESPONDING TO THESE RITZ VALUES IS REFERRED
C  TO AS A SCHUR BASIS.
C
C  SEE DOCUMENTATION IN THE HEADER OF THE SUBROUTINE DNAUPD FOR
C  DEFINITION OF OP AS WELL AS OTHER TERMS AND THE RELATION OF COMPUTED
C  RITZ VALUES AND RITZ VECTORS OF OP WITH RESPECT TO THE GIVEN PROBLEM
C  A*Z = LAMBDA*B*Z.  FOR A BRIEF DESCRIPTION, SEE DEFINITIONS OF
C  IPARAM(7), MODE AND WHICH IN THE DOCUMENTATION OF DNAUPD.
C
C ARGUMENTS:
C  RVEC  LOGICAL  (INPUT)
C    SPECIFIES WHETHER A BASIS FOR THE INVARIANT SUBSPACE CORRESPONDING
C    TO THE CONVERGED RITZ VALUE APPROXIMATIONS FOR THE EIGENPROBLEM
C    A*Z = LAMBDA*B*Z IS COMPUTED.
C
C             RVEC = .FALSE. COMPUTE RITZ VALUES ONLY.
C
C             RVEC = .TRUE.  COMPUTE THE RITZ VECTORS OR SCHUR VECTORS.
C                            SEE REMARKS BELOW.
C
C  HOWMNY  CHARACTER*1  (INPUT)
C     SPECIFIES THE FORM OF THE BASIS FOR THE INVARIANT SUBSPACE
C     CORRESPONDING TO THE CONVERGED RITZ VALUES THAT IS TO BE COMPUTED.
C
C          = 'A': COMPUTE NEV RITZ VECTORS,
C          = 'P': COMPUTE NEV SCHUR VECTORS,
C          = 'S': COMPUTE SOME OF THE RITZ VECTORS, SPECIFIED
C                 BY THE LOGICAL ARRAY SELECT.
C
C  SELECT  LOGICAL ARRAY OF DIMENSION NCV.  (INPUT)
C          IF HOWMNY = 'S', SELECT SPECIFIES THE RITZ VECTORS TO BE
C          COMPUTED. TO SELECT THE RITZ VECTOR CORRESPONDING TO A
C          RITZ VALUE (DR(J), DI(J)), SELECT(J) MUST BE SET TO .TRUE..
C          IF HOWMNY = 'A' OR 'P', SELECT IS USED AS INTERNAL WORKSPACE.
C
C  DR   REAL*8 ARRAY OF DIMENSION NEV+1.  (OUTPUT)
C     IF IPARAM(7) = 1,2 OR 3 AND SIGMAI=0.0  THEN ON EXIT: DR CONTAINS
C     THE REAL PART OF THE RITZ  APPROXIMATIONS TO THE EIGENVALUES OF
C     A*Z = LAMBDA*B*Z.
C     IF IPARAM(7) = 3, 4 AND SIGMAI IS NOT EQUAL TO ZERO, THEN ON EXIT:
C     DR CONTAINS THE REAL PART OF THE RITZ VALUES OF OP COMPUTED BY
C     DNAUPD. A FURTHER COMPUTATION MUST BE PERFORMED BY THE USER
C     TO TRANSFORM THE RITZ VALUES COMPUTED FOR OP BY DNAUPD TO THOSE
C     OF THE ORIGINAL SYSTEM A*Z = LAMBDA*B*Z. SEE REMARK 3 BELOW.
C
C  DI REAL*8 ARRAY OF DIMENSION NEV+1.  (OUTPUT)
C     ON EXIT, DI CONTAINS THE IMAGINARY PART OF THE RITZ VALUE
C     APPROXIMATIONS TO THE EIGENVALUES OF A*Z = LAMBDA*B*Z ASSOCIATED
C     WITH DR.
C
C     NOTE: WHEN RITZ VALUES ARE COMPLEX, THEY WILL COME IN COMPLEX
C           CONJUGATE PAIRS.  IF EIGENVECTORS ARE REQUESTED, THE
C           CORRESPONDING RITZ VECTORS WILL ALSO COME IN CONJUGATE
C           PAIRS AND THE REAL AND IMAGINARY PARTS OF THESE ARE
C           REPRESENTED IN TWO CONSECUTIVE COLUMNS OF THE ARRAY Z
C           (SEE BELOW).
C
C  Z REAL*8 N BY NEV+1 ARRAY IF RVEC = .TRUE. AND HOWMNY = 'A'. (OUTPUT)
C    ON EXIT, IF RVEC = .TRUE. AND HOWMNY = 'A', THEN THE COLUMNS OF
C    Z REPRESENT APPROXIMATE EIGENVECTORS (RITZ VECTORS) CORRESPONDING
C    TO THE NCONV=IPARAM(5) RITZ VALUES FOR EIGENSYSTEM
C    A*Z = LAMBDA*B*Z.
C
C    THE COMPLEX RITZ VECTOR ASSOCIATED WITH THE RITZ VALUE
C    WITH POSITIVE IMAGINARY PART IS STORED IN TWO CONSECUTIVE
C    COLUMNS.  THE FIRST COLUMN HOLDS THE REAL PART OF THE RITZ
C    VECTOR AND THE SECOND COLUMN HOLDS THE IMAGINARY PART.  THE
C    RITZ VECTOR ASSOCIATED WITH THE RITZ VALUE WITH NEGATIVE
C    IMAGINARY PART IS SIMPLY THE COMPLEX CONJUGATE OF THE RITZ VECTOR
C    ASSOCIATED WITH THE POSITIVE IMAGINARY PART.
C
C    IF  RVEC = .FALSE. OR HOWMNY = 'P', THEN Z IS NOT REFERENCED.
C
C    NOTE: IF IF RVEC = .TRUE. AND A SCHUR BASIS IS NOT REQUIRED,
C    THE ARRAY Z MAY BE SET EQUAL TO FIRST NEV+1 COLUMNS OF THE ARNOLDI
C    BASIS ARRAY V COMPUTED BY DNAUPD.  IN THIS CASE THE ARNOLDI BASIS
C    WILL BE DESTROYED AND OVERWRITTEN WITH THE EIGENVECTOR BASIS.
C
C  LDZ INTEGER.  (INPUT)
C     THE LEADING DIMENSION OF THE ARRAY Z.  IF RITZ VECTORS ARE
C     DESIRED, THEN  LDZ >= MAX( 1, N ).  IN ANY CASE,  LDZ >= 1.
C
C  SIGMAR  REAL*8  (INPUT)
C        IF IPARAM(7) = 3 OR 4, REPRESENTS THE REAL PART OF THE SHIFT.
C        NOT REFERENCED IF IPARAM(7) = 1 OR 2.
C
C  SIGMAI  REAL*8  (INPUT)
C    IF IPARAM(7) = 3 OR 4, REPRESENTS THE IMAGINARY PART OF THE SHIFT.
C    NOT REFERENCED IF IPARAM(7) = 1 OR 2. SEE REMARK 3 BELOW.
C
C  WORKEV  REAL*8 WORK ARRAY OF DIMENSION 3*NCV.  (WORKSPACE)
C
C  **** THE REMAINING ARGUMENTS MUST BE THE SAME AS FOR THE   ****
C  **** CALL TO DNAUPD THAT WAS JUST COMPLETED.               ****
C
C  NOTE: THE REMAINING ARGUMENTS
C
C       BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR,
C       WORKD, WORKL, LWORKL, INFO
C
C       MUST BE PASSED DIRECTLY TO DNEUPD FOLLOWING THE LAST CALL
C       TO DNAUPD.  THESE ARGUMENTS MUST NOT BE MODIFIED BETWEEN
C       THE THE LAST CALL TO DNAUPD AND THE CALL TO DNEUPD.
C
C  THREE PARAMETERS (V, WORKL, INFO) ARE ALSO OUTPUT PARAMETERS:
C
C  V  REAL*8 N BY NCV ARRAY.  (INPUT/OUTPUT)
C
C     UPON INPUT: THE NCV COLUMNS OF V CONTAIN THE ARNOLDI BASIS
C                 VECTORS FOR OP AS CONSTRUCTED BY DNAUPD .
C
C     UPON OUTPUT: IF RVEC = .TRUE. THE FIRST NCONV=IPARAM(5) COLUMNS
C                  CONTAIN APPROXIMATE SCHUR VECTORS THAT SPAN THE
C                  DESIRED INVARIANT SUBSPACE.  SEE REMARK 2 BELOW.
C
C     NOTE: IF THE ARRAY Z HAS BEEN SET EQUAL TO FIRST NEV+1 COLUMNS
C     OF THE ARRAY V AND RVEC=.TRUE. AND HOWMNY= 'A', THEN THE
C     ARNOLDI BASIS HELD BY V HAS BEEN OVERWRITTEN BY THE DESIRED
C     RITZ VECTORS.  IF A SEPARATE ARRAY Z HAS BEEN PASSED THEN
C     THE FIRST NCONV=IPARAM(5) COLUMNS OF V WILL CONTAIN APPROXIMATE
C     SCHUR VECTORS THAT SPAN THE DESIRED INVARIANT SUBSPACE.
C
C  WORKL   REAL*8 WORK ARRAY OF LENGTH LWORKL.  (OUTPUT/WORKSPACE)
C     WORKL(1:NCV*NCV+3*NCV) CONTAINS INFORMATION OBTAINED IN
C     DNAUPD.  THEY ARE NOT CHANGED BY DNEUPD.
C     WORKL(NCV*NCV+3*NCV+1:3*NCV*NCV+6*NCV) HOLDS THE
C     REAL AND IMAGINARY PART OF THE UNTRANSFORMED RITZ VALUES,
C     THE UPPER QUASI-TRIANGULAR MATRIX FOR H, AND THE
C     ASSOCIATED MATRIX REPRESENTATION OF THE INVARIANT SUBSPACE FOR H.
C
C     NOTE: IPNTR(9:13) CONTAINS THE POINTER INTO WORKL FOR ADDRESSES
C     OF THE ABOVE INFORMATION COMPUTED BY DNEUPD.
C     -------------------------------------------------------------
C     IPNTR(9):  POINTER TO THE REAL PART OF THE NCV RITZ VALUES OF THE
C                ORIGINAL SYSTEM.
C     IPNTR(10): POINTER TO THE IMAGINARY PART OF THE NCV RITZ VALUES OF
C                THE ORIGINAL SYSTEM.
C     IPNTR(11): POINTER TO THE NCV CORRESPONDING ERROR BOUNDS.
C     IPNTR(12): POINTER TO THE NCV BY NCV UPPER QUASI-TRIANGULAR
C                SCHUR MATRIX FOR H.
C     IPNTR(13): POINTER TO THE NCV BY NCV MATRIX OF EIGENVECTORS
C                OF THE UPPER HESSENBERG MATRIX H. ONLY REFERENCED BY
C                DNEUPD IF RVEC = .TRUE. SEE REMARK 2 BELOW.
C      -------------------------------------------------------------
C
C  INFO  INTEGER.  (OUTPUT)
C        ERROR FLAG ON OUTPUT.
C
C        =  0: NORMAL EXIT.
C
C        =  1: THE SCHUR FORM COMPUTED BY LAPACK ROUTINE FLAHQR
C          COULD NOT BE REORDERED BY LAPACK ROUTINE FTRSEN.
C          RE-ENTER SUBROUTINE DNEUPD WITH IPARAM(5)=NCV AND
C          INCREASE THE SIZE OF THE ARRAYS DR AND DI TO HAVE
C          DIMENSION AT LEAST DIMENSION NCV AND ALLOCATE AT LEAST NCV
C          COLUMNS FOR Z. NOTE: NOT NECESSARY IF Z AND V SHARE
C          THE SAME SPACE. PLEASE NOTIFY THE AUTHORS IF THIS ERROR
C          OCCURS.
C
C        = -1: N MUST BE POSITIVE.
C        = -2: NEV MUST BE POSITIVE.
C        = -3: NCV-NEV >= 2 AND LESS THAN OR EQUAL TO N.
C        = -5: WHICH MUST BE ONE OF 'LM', 'SM', 'LR', 'SR', 'LI', 'SI'
C        = -6: BMAT MUST BE ONE OF 'I' OR 'G'.
C        = -7: LENGTH OF PRIVATE WORK WORKL ARRAY IS NOT SUFFICIENT.
C        = -8: ERROR RETURN FROM CALCULATION OF A REAL SCHUR FORM.
C              INFORMATIONAL ERROR FROM LAPACK ROUTINE FLAHQR.
C        = -9: ERROR RETURN FROM CALCULATION OF EIGENVECTORS.
C              INFORMATIONAL ERROR FROM LAPACK ROUTINE FTREVC.
C        = -10: IPARAM(7) MUST BE 1,2,3,4.
C        = -11: IPARAM(7) = 1 AND BMAT = 'G' ARE INCOMPATIBLE.
C        = -12: HOWMNY = 'S' NOT YET IMPLEMENTED
C        = -13: HOWMNY MUST BE ONE OF 'A' OR 'P' IF RVEC = .TRUE.
C        = -14: DNAUPD DID NOT FIND ANY EIGENVALUES TO SUFFICIENT
C               ACCURACY.
C
C BEGINLIB
C-----------------------------------------------------------------------
C REFERENCES:
C  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
C     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
C     PP 357-385.
C  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
C     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
C     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
C  3. B.N. PARLETT & Y. SAAD, "COMPLEX SHIFT AND INVERT STRATEGIES FOR
C     REAL MATRICES", LINEAR ALGEBRA AND ITS APPLICATIONS, VOL 88/89,
C     PP 575-595, (1987).
C
C ROUTINES CALLED:
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     FGEQR2  LAPACK ROUTINE THAT COMPUTES THE QR FACTORIZATION OF
C             A MATRIX.
C     FLACPY  LAPACK MATRIX COPY ROUTINE.
C     FLAHQR  LAPACK ROUTINE TO COMPUTE THE REAL SCHUR FORM OF AN
C             UPPER HESSENBERG MATRIX.
C     FLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C     FLASET  LAPACK MATRIX INITIALIZATION ROUTINE.
C     FORM2R  LAPACK ROUTINE THAT APPLIES AN ORTHOGONAL MATRIX IN
C             FACTORED FORM.
C     FTREVC  LAPACK ROUTINE TO COMPUTE THE EIGENVECTORS OF A MATRIX
C             IN UPPER QUASI-TRIANGULAR FORM.
C     FTRSEN  LAPACK ROUTINE THAT RE-ORDERS THE SCHUR FORM.
C     DTRMM   LEVEL 3 BLAS MATRIX TIMES AN UPPER TRIANGULAR MATRIX.
C     DGER    LEVEL 2 BLAS RANK ONE UPDATE TO A MATRIX.
C     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     DNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
C     DSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
C
C     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION.
C     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES.
C
C INTRINSIC FUNCTIONS
C     ABS, MAX, MIN
C
C REMARKS
C
C  1. CURRENTLY ONLY HOWMNY = 'A' AND 'P' ARE IMPLEMENTED.
C
C     LET X' DENOTE THE TRANSPOSE OF X.
C
C  2. SCHUR VECTORS ARE AN ORTHOGONAL REPRESENTATION FOR THE BASIS OF
C   RITZ VECTORS. THUS, THEIR NUMERICAL PROPERTIES ARE OFTEN SUPERIOR.
C   IF RVEC = .TRUE. THEN THE RELATIONSHIP
C             A * V(:,1:IPARAM(5)) = V(:,1:IPARAM(5)) * T, AND
C   V(:,1:IPARAM(5))' * V(:,1:IPARAM(5)) = I ARE APPROXIMATELY
C   SATISFIED.
C   HERE T IS THE LEADING SUBMATRIX OF ORDER IPARAM(5) OF THE REAL
C   UPPER QUASI-TRIANGULAR MATRIX STORED WORKL(IPNTR(12)). THAT IS,
C   T IS BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS,
C   EACH 2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
C   OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.CORRESPONDING TO EACH 2-BY-2
C   DIAGONAL BLOCK IS A COMPLEX CONJUGATE PAIR OF RITZ VALUES. THE REAL
C   RITZ VALUES ARE STORED ON THE DIAGONAL OF T.
C
C  3. IF IPARAM(7) = 3 OR 4 AND SIGMAI IS NOT EQUAL ZERO, THEN THE USER
C   MUST FORM THE IPARAM(5) RAYLEIGH QUOTIENTS IN ORDER TO TRANSFORM
C   RITZ VALUES COMPUTED BY DNAUPD FOR OP TO THOSE OF A*Z = LAMBDA*B*Z.
C   SET RVEC = .TRUE. AND HOWMNY = 'A', AND
C   COMPUTE
C         Z(:,I)' * A * Z(:,I) IF DI(I) = 0.
C   IF DI(I) IS NOT EQUAL TO ZERO AND DI(I+1) = - D(I),
C   THEN THE DESIRED REAL AND IMAGINARY PARTS OF THE RITZ VALUE ARE
C       Z(:,I)' * A * Z(:,I) +  Z(:,I+1)' * A * Z(:,I+1),
C       Z(:,I)' * A * Z(:,I+1) -  Z(:,I+1)' * A * Z(:,I), RESPECTIVELY.
C     ANOTHER POSSIBILITY IS TO SET RVEC = .TRUE. AND HOWMNY = 'P' AND
C     COMPUTE V(:,1:IPARAM(5))' * A * V(:,1:IPARAM(5)) AND THEN AN UPPER
C     QUASI-TRIANGULAR MATRIX OF ORDER IPARAM(5) IS COMPUTED. SEE REMARK
C     2 ABOVE.
C
C AUTHORS
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     CHAO YANG                    HOUSTON, TEXAS
C     DEPT. OF COMPUTATIONAL &
C     APPLIED MATHEMATICS
C     RICE UNIVERSITY
C     HOUSTON, TEXAS
C
C FILE: NEUPD.F   SID: 2.5   DATE OF SID: 7/31/96   RELEASE: 2
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE DLAMCH,
C            COMMON TIMING REMPLACE PAR COMMON INFORMATION,
C            UTILISATION DE R8PREM() ET R8MIEM(),
C            RAJOUT DU REEL EPS,
C            SHUNTAGE MESSAGE ERREUR -3 ET NOUVEAU MESSAGE,
C            RAJOUT 4 MESSAGES DIVISION PAR ZERO,
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

      CHARACTER*1 BMAT, HOWMNY
      CHARACTER*2 WHICH
      LOGICAL RVEC
      INTEGER INFO, LDZ, LDV, LWORKL, N, NCV, NEV
      REAL*8 SIGMAR, SIGMAI, TOL

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      INTEGER IPARAM(11), IPNTR(14)
      LOGICAL SELECT(NCV)
      REAL*8 DR(NEV+1), DI(NEV+1), RESID(N), V(LDV,NCV), Z(LDZ,*),
     &  WORKD(3*N), WORKL(LWORKL), WORKEV(3*NCV)

C     %------------%
C     | PARAMETERS |
C     %------------%

      REAL*8 ONE, ZERO
      PARAMETER (ONE = 1.0D+0, ZERO = 0.0D+0)

C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%

      CHARACTER*6 TYPE
      INTEGER BOUNDS, IERR, IH, IHBDS, IHEIGR, IHEIGI, ICONJ, NCONV,
     &  INVSUB, IUPTRI, IWORK(1), J, K, KTRORD,
     &  LDH, LDQ, MODE, MSGLVL, OUTNCV, RITZR, RITZI, IRR, IRI, IBD
C DUE TO CRS512 INTEGER IWEV, WRR, WRI
      LOGICAL REORD
      REAL*8 CONDS, RNORM, SEP, TEMP, THRES, VL(1,1), TEMP1, EPS23, EPS

C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%

      REAL*8 FLAPY2, DNRM2, R8MIEM, R8PREM

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

C     %------------------------%
C     | SET DEFAULT PARAMETERS |
C     %------------------------%

      MSGLVL = MNEUPD
      EPS = R8MIEM()**(2.0D+0 / 3.0D+0)
      MODE = IPARAM(7)
      NCONV = IPARAM(5)
      INFO = 0

C     %---------------------------------%
C     | GET MACHINE DEPENDENT CONSTANT. |
C     %---------------------------------%

      EPS23 = R8PREM()*0.5D0
      EPS23 = EPS23**(2.0D+0 / 3.0D+0)

C     %--------------%
C     | QUICK RETURN |
C     %--------------%

      IERR = 0

      IF (NCONV .LE. 0) THEN
         IERR = -14
      ELSE IF (N .LE. 0) THEN
         IERR = -1
      ELSE IF (NEV .LE. 0) THEN
         IERR = -2
      ELSE IF (NCV .LE. NEV+1 .OR.  NCV .GT. N) THEN
        IF (MSGLVL.GT.0) THEN
             WRITE(LOGFIL,*)
             WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
             WRITE(LOGFIL,*)'& FLAG ERREUR -3 DEBRANCHE DANS DNEUPD &'
             WRITE(LOGFIL,*)'& NBVECT < NBFREQ + 2 OU NBVECT > NBEQ &'
             WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
             WRITE(LOGFIL,*)
        ENDIF
      ELSE IF (WHICH .NE. 'LM' .AND.
     &        WHICH .NE. 'SM' .AND.
     &        WHICH .NE. 'LR' .AND.
     &        WHICH .NE. 'SR' .AND.
     &        WHICH .NE. 'LI' .AND.
     &        WHICH .NE. 'SI') THEN
         IERR = -5
      ELSE IF (BMAT .NE. 'I' .AND. BMAT .NE. 'G') THEN
         IERR = -6
      ELSE IF (LWORKL .LT. 3*NCV**2 + 6*NCV) THEN
         IERR = -7
      ELSE IF ( (HOWMNY .NE. 'A' .AND.
     &           HOWMNY .NE. 'P' .AND.
     &           HOWMNY .NE. 'S') .AND. RVEC ) THEN
         IERR = -13
      ELSE IF (HOWMNY .EQ. 'S' ) THEN
         IERR = -12
      END IF

      IF (MODE .EQ. 1 .OR. MODE .EQ. 2) THEN
         TYPE = 'REGULR'
      ELSE IF (MODE .EQ. 3 .AND. SIGMAI .EQ. ZERO) THEN
         TYPE = 'SHIFTI'
      ELSE IF (MODE .EQ. 3 ) THEN
         TYPE = 'REALPT'
      ELSE IF (MODE .EQ. 4 ) THEN
         TYPE = 'IMAGPT'
      ELSE
         IERR = -10
      END IF
      IF (MODE .EQ. 1 .AND. BMAT .EQ. 'G')    IERR = -11

C     %------------%
C     | ERROR EXIT |
C     %------------%

      IF (IERR .NE. 0) THEN
         INFO = IERR
         GO TO 9000
      END IF
C
C     %--------------------------------------------------------%
C     | POINTER INTO WORKL FOR ADDRESS OF H, RITZ, BOUNDS, Q   |
C     | ETC... AND THE REMAINING WORKSPACE.                    |
C     | ALSO UPDATE POINTER TO BE USED ON OUTPUT.              |
C     | MEMORY IS LAID OUT AS FOLLOWS:                         |
C     | WORKL(1:NCV*NCV) := GENERATED HESSENBERG MATRIX        |
C     | WORKL(NCV*NCV+1:NCV*NCV+2*NCV) := REAL AND IMAGINARY   |
C     |                                   PARTS OF RITZ VALUES |
C     | WORKL(NCV*NCV+2*NCV+1:NCV*NCV+3*NCV) := ERROR BOUNDS   |
C     %--------------------------------------------------------%
C
C     %-----------------------------------------------------------%
C     | THE FOLLOWING IS USED AND SET BY DNEUPD.                  |
C     | WORKL(NCV*NCV+3*NCV+1:NCV*NCV+4*NCV) := THE UNTRANSFORMED |
C     |                             REAL PART OF THE RITZ VALUES. |
C     | WORKL(NCV*NCV+4*NCV+1:NCV*NCV+5*NCV) := THE UNTRANSFORMED |
C     |                        IMAGINARY PART OF THE RITZ VALUES. |
C     | WORKL(NCV*NCV+5*NCV+1:NCV*NCV+6*NCV) := THE UNTRANSFORMED |
C     |                           ERROR BOUNDS OF THE RITZ VALUES |
C     | WORKL(NCV*NCV+6*NCV+1:2*NCV*NCV+6*NCV) := HOLDS THE UPPER |
C     |                             QUASI-TRIANGULAR MATRIX FOR H |
C     | WORKL(2*NCV*NCV+6*NCV+1: 3*NCV*NCV+6*NCV) := HOLDS THE    |
C     |       ASSOCIATED MATRIX REPRESENTATION OF THE INVARIANT   |
C     |       SUBSPACE FOR H.                                     |
C     | GRAND TOTAL OF NCV * ( 3 * NCV + 6 ) LOCATIONS.           |
C     %-----------------------------------------------------------%
C
      IH     = IPNTR(5)
      RITZR  = IPNTR(6)
      RITZI  = IPNTR(7)
      BOUNDS = IPNTR(8)
      LDH    = NCV
      LDQ    = NCV
      IHEIGR = BOUNDS + LDH
      IHEIGI = IHEIGR + LDH
      IHBDS  = IHEIGI + LDH
      IUPTRI = IHBDS  + LDH
      INVSUB = IUPTRI + LDH*NCV
      IPNTR(9)  = IHEIGR
      IPNTR(10) = IHEIGI
      IPNTR(11) = IHBDS
      IPNTR(12) = IUPTRI
      IPNTR(13) = INVSUB
C DUE TO CRS512      WRR = 1
C DUE TO CRS512      WRI = NCV + 1
C DUE TO CRS512      IWEV = WRI + NCV

C     %-----------------------------------------%
C     | IRR POINTS TO THE REAL PART OF THE RITZ |
C     |     VALUES COMPUTED BY _NEIGH BEFORE    |
C     |     EXITING _NAUP2.                     |
C     | IRI POINTS TO THE IMAGINARY PART OF THE |
C     |     RITZ VALUES COMPUTED BY _NEIGH      |
C     |     BEFORE EXITING _NAUP2.              |
C     | IBD POINTS TO THE RITZ ESTIMATES        |
C     |     COMPUTED BY _NEIGH BEFORE EXITING   |
C     |     _NAUP2.                             |
C     %-----------------------------------------%

      IRR = IPNTR(14)+NCV*NCV
      IRI = IRR+NCV
      IBD = IRI+NCV

C     %------------------------------------%
C     | RNORM IS B-NORM OF THE RESID(1:N). |
C     %------------------------------------%

      RNORM = WORKL(IH+2)
      WORKL(IH+2) = ZERO

      IF (RVEC) THEN

C        %-------------------------------------------%
C        | GET CONVERGED RITZ VALUE ON THE BOUNDARY. |
C        | NOTE: CONVERGED RITZ VALUES HAVE BEEN     |
C        | PLACED IN THE FIRST NCONV LOCATIONS IN    |
C        | WORKL(RITZR) AND WORKL(RITZI).  THEY HAVE |
C        | BEEN SORTED (IN _NAUP2) ACCORDING TO THE  |
C        | WHICH SELECTION CRITERION.                |
C        %-------------------------------------------%

         IF (WHICH .EQ. 'LM' .OR. WHICH .EQ. 'SM') THEN
            THRES = FLAPY2( WORKL(RITZR), WORKL(RITZI) )
         ELSE IF (WHICH .EQ. 'LR' .OR. WHICH .EQ. 'SR') THEN
            THRES = WORKL(RITZR)
         ELSE IF (WHICH .EQ. 'LI' .OR. WHICH .EQ. 'SI') THEN
            THRES = ABS( WORKL(RITZI) )
         END IF

         IF (MSGLVL .GT. 2) THEN
             CALL DVOUT(LOGFIL, 1, THRES, NDIGIT,
     &           '_NEUPD: THRESHOLD EIGENVALUE USED FOR RE-ORDERING')
         END IF

C        %----------------------------------------------------------%
C        | CHECK TO SEE IF ALL CONVERGED RITZ VALUES APPEAR AT THE  |
C        | TOP OF THE UPPER QUASI-TRIANGULAR MATRIX COMPUTED BY     |
C        | _NEIGH IN _NAUP2.  THIS IS DONE IN THE FOLLOWING WAY:    |
C        |                                                          |
C        | 1) FOR EACH RITZ VALUE OBTAINED FROM _NEIGH, COMPARE IT  |
C        |    WITH THE THRESHOLD RITZ VALUE COMPUTED ABOVE TO       |
C        |    DETERMINE WHETHER IT IS A WANTED ONE.                 |
C        |                                                          |
C        | 2) IF IT IS WANTED, THEN CHECK THE CORRESPONDING RITZ    |
C        |    ESTIMATE TO SEE IF IT HAS CONVERGED.  IF IT HAS, SET  |
C        |    CORREPONDING ENTRY IN THE LOGICAL ARRAY SELECT TO     |
C        |    .TRUE..                                               |
C        |                                                          |
C        | IF SELECT(J) = .TRUE. AND J > NCONV, THEN THERE IS A     |
C        | CONVERGED RITZ VALUE THAT DOES NOT APPEAR AT THE TOP OF  |
C        | THE UPPER QUASI-TRIANGULAR MATRIX COMPUTED BY _NEIGH IN  |
C        | _NAUP2.  REORDERING IS NEEDED.                           |
C        %----------------------------------------------------------%

         REORD = .FALSE.
         KTRORD = 0
         DO 10 J = 0, NCV-1
            SELECT(J+1) = .FALSE.
            IF (WHICH .EQ. 'LM') THEN
               IF (FLAPY2(WORKL(IRR+J), WORKL(IRI+J))
     &            .GE. THRES) THEN
                  TEMP1 = MAX( EPS23,
     &                         FLAPY2( WORKL(IRR+J), WORKL(IRI+J) ) )
                  IF (WORKL(IBD+J) .LE. TOL*TEMP1)
     &               SELECT(J+1) = .TRUE.
               END IF
            ELSE IF (WHICH .EQ. 'SM') THEN
               IF (FLAPY2(WORKL(IRR+J), WORKL(IRI+J))
     &            .LE. THRES) THEN
                  TEMP1 = MAX( EPS23,
     &                         FLAPY2( WORKL(IRR+J), WORKL(IRI+J) ) )
                  IF (WORKL(IBD+J) .LE. TOL*TEMP1)
     &               SELECT(J+1) = .TRUE.
               END IF
            ELSE IF (WHICH .EQ. 'LR') THEN
               IF (WORKL(IRR+J) .GE. THRES) THEN
                  TEMP1 = MAX( EPS23,
     &                         FLAPY2( WORKL(IRR+J), WORKL(IRI+J) ) )
                  IF (WORKL(IBD+J) .LE. TOL*TEMP1)
     &               SELECT(J+1) = .TRUE.
               END IF
            ELSE IF (WHICH .EQ. 'SR') THEN
               IF (WORKL(IRR+J) .LE. THRES) THEN
                  TEMP1 = MAX( EPS23,
     &                         FLAPY2( WORKL(IRR+J), WORKL(IRI+J) ) )
                  IF (WORKL(IBD+J) .LE. TOL*TEMP1)
     &               SELECT(J+1) = .TRUE.
               END IF
            ELSE IF (WHICH .EQ. 'LI') THEN
               IF (ABS(WORKL(IRI+J)) .GE. THRES) THEN
                  TEMP1 = MAX( EPS23,
     &                         FLAPY2( WORKL(IRR+J), WORKL(IRI+J) ) )
                  IF (WORKL(IBD+J) .LE. TOL*TEMP1)
     &               SELECT(J+1) = .TRUE.
               END IF
            ELSE IF (WHICH .EQ. 'SI') THEN
               IF (ABS(WORKL(IRI+J)) .LE. THRES) THEN
                  TEMP1 = MAX( EPS23,
     &                         FLAPY2( WORKL(IRR+J), WORKL(IRI+J) ) )
                  IF (WORKL(IBD+J) .LE. TOL*TEMP1)
     &               SELECT(J+1) = .TRUE.
               END IF
            END IF
            IF (J+1 .GT. NCONV ) REORD = ( SELECT(J+1) .OR. REORD )
            IF (SELECT(J+1)) KTRORD = KTRORD + 1
 10      CONTINUE

         IF (MSGLVL .GT. 2) THEN
             CALL IVOUT(LOGFIL, 1, KTRORD, NDIGIT,
     &            '_NEUPD: NUMBER OF SPECIFIED EIGENVALUES')
             CALL IVOUT(LOGFIL, 1, NCONV, NDIGIT,
     &            '_NEUPD: NUMBER OF "CONVERGED" EIGENVALUES')
         END IF

C        %-----------------------------------------------------------%
C        | CALL LAPACK ROUTINE FLAHQR TO COMPUTE THE REAL SCHUR FORM |
C        | OF THE UPPER HESSENBERG MATRIX RETURNED BY DNAUPD.        |
C        | MAKE A COPY OF THE UPPER HESSENBERG MATRIX.               |
C        | INITIALIZE THE SCHUR VECTOR MATRIX Q TO THE IDENTITY.     |
C        %-----------------------------------------------------------%

         CALL DCOPY (LDH*NCV, WORKL(IH), 1, WORKL(IUPTRI), 1)
C DUE TO CRP_102 CALL FLASET ('ALL', NCV, NCV, ZERO, ONE,
C WORKL(INVSUB), LDQ)
         CALL FLASET ('A', NCV, NCV, ZERO, ONE, WORKL(INVSUB), LDQ)
         CALL FLAHQR (.TRUE., .TRUE., NCV, 1, NCV, WORKL(IUPTRI), LDH,
     &        WORKL(IHEIGR), WORKL(IHEIGI), 1, NCV,
     &        WORKL(INVSUB), LDQ, IERR)
         CALL DCOPY (NCV, WORKL(INVSUB+NCV-1), LDQ, WORKL(IHBDS), 1)

         IF (IERR .NE. 0) THEN
            INFO = -8
            GO TO 9000
         END IF

         IF (MSGLVL .GT. 1) THEN
             CALL DVOUT (LOGFIL, NCV, WORKL(IHEIGR), NDIGIT,
     &           '_NEUPD: REAL PART OF THE EIGENVALUES OF H')
             CALL DVOUT (LOGFIL, NCV, WORKL(IHEIGI), NDIGIT,
     &           '_NEUPD: IMAGINARY PART OF THE EIGENVALUES OF H')
             CALL DVOUT (LOGFIL, NCV, WORKL(IHBDS), NDIGIT,
     &           '_NEUPD: LAST ROW OF THE SCHUR VECTOR MATRIX')
             IF (MSGLVL .GT. 3) THEN
               CALL DMOUT (LOGFIL, NCV, NCV, WORKL(IUPTRI), LDH, NDIGIT,
     &              '_NEUPD: THE UPPER QUASI-TRIANGULAR MATRIX ')
             ENDIF
         ENDIF

         IF (REORD) THEN

C           %-----------------------------------------------------%
C           | REORDER THE COMPUTED UPPER QUASI-TRIANGULAR MATRIX. |
C           %-----------------------------------------------------%

            CALL FTRSEN ('N', 'V', SELECT, NCV, WORKL(IUPTRI), LDH,
     &           WORKL(INVSUB), LDQ, WORKL(IHEIGR), WORKL(IHEIGI),
     &           NCONV, CONDS, SEP, WORKL(IHBDS), NCV, IWORK, 1, IERR)
            IF (IERR .EQ. 1) THEN
               INFO = 1
               GO TO 9000
            END IF

            IF (MSGLVL .GT. 2) THEN
                CALL DVOUT (LOGFIL, NCV, WORKL(IHEIGR), NDIGIT,
     &           '_NEUPD: REAL PART OF THE EIGENVALUES OF H--REORDERED')
                CALL DVOUT (LOGFIL, NCV, WORKL(IHEIGI), NDIGIT,
     &           '_NEUPD: IMAG PART OF THE EIGENVALUES OF H--REORDERED')
                IF (MSGLVL .GT. 3) THEN
                   CALL DMOUT (LOGFIL, NCV, NCV, WORKL(IUPTRI), LDQ,
     &                  NDIGIT,
     &              '_NEUPD: QUASI-TRIANGULAR MATRIX AFTER RE-ORDERING')
                ENDIF
            ENDIF
         ENDIF

C        %---------------------------------------%
C        | COPY THE LAST ROW OF THE SCHUR VECTOR |
C        | INTO WORKL(IHBDS).  THIS WILL BE USED |
C        | TO COMPUTE THE RITZ ESTIMATES OF      |
C        | CONVERGED RITZ VALUES.                |
C        %---------------------------------------%

         CALL DCOPY(NCV, WORKL(INVSUB+NCV-1), LDQ, WORKL(IHBDS), 1)

C        %----------------------------------------------------%
C        | PLACE THE COMPUTED EIGENVALUES OF H INTO DR AND DI |
C        | IF A SPECTRAL TRANSFORMATION WAS NOT USED.         |
C        %----------------------------------------------------%

         IF (TYPE .EQ. 'REGULR') THEN
            CALL DCOPY (NCONV, WORKL(IHEIGR), 1, DR, 1)
            CALL DCOPY (NCONV, WORKL(IHEIGI), 1, DI, 1)
         END IF

C        %----------------------------------------------------------%
C        | COMPUTE THE QR FACTORIZATION OF THE MATRIX REPRESENTING  |
C        | THE WANTED INVARIANT SUBSPACE LOCATED IN THE FIRST NCONV |
C        | COLUMNS OF WORKL(INVSUB,LDQ).                            |
C        %----------------------------------------------------------%

         CALL FGEQR2 (NCV, NCONV, WORKL(INVSUB), LDQ, WORKEV,
     &        WORKEV(NCV+1), IERR)

C        %---------------------------------------------------------%
C        | * POSTMULTIPLY V BY Q USING FORM2R.                     |
C        | * COPY THE FIRST NCONV COLUMNS OF VQ INTO Z.            |
C        | * POSTMULTIPLY Z BY R.                                  |
C        | THE N BY NCONV MATRIX Z IS NOW A MATRIX REPRESENTATION  |
C        | OF THE APPROXIMATE INVARIANT SUBSPACE ASSOCIATED WITH   |
C        | THE RITZ VALUES IN WORKL(IHEIGR) AND WORKL(IHEIGI)      |
C        | THE FIRST NCONV COLUMNS OF V ARE NOW APPROXIMATE SCHUR  |
C        | VECTORS ASSOCIATED WITH THE REAL UPPER QUASI-TRIANGULAR |
C        | MATRIX OF ORDER NCONV IN WORKL(IUPTRI)                  |
C        %---------------------------------------------------------%
C DUE TO CRP_102 CALL FORM2R ('RIGHT', 'NOTRANSPOSE', N, NCV, NCONV,
C
         CALL FORM2R ('R', 'N', N, NCV, NCONV,
     &        WORKL(INVSUB), LDQ, WORKEV, V, LDV, WORKD(N+1), IERR)
         CALL FLACPY ('A', N, NCONV, V, LDV, Z, LDZ)

         DO 20 J=1, NCONV

C           %---------------------------------------------------%
C           | PERFORM BOTH A COLUMN AND ROW SCALING IF THE      |
C           | DIAGONAL ELEMENT OF WORKL(INVSUB,LDQ) IS NEGATIVE |
C           | I'M LAZY AND DON'T TAKE ADVANTAGE OF THE UPPER    |
C           | QUASI-TRIANGULAR FORM OF WORKL(IUPTRI,LDQ)        |
C           | NOTE THAT SINCE Q IS ORTHOGONAL, R IS A DIAGONAL  |
C           | MATRIX CONSISTING OF PLUS OR MINUS ONES           |
C           %---------------------------------------------------%

            IF (WORKL(INVSUB+(J-1)*LDQ+J-1) .LT. ZERO) THEN
               CALL DSCAL (NCONV, -ONE, WORKL(IUPTRI+J-1), LDQ)
               CALL DSCAL (NCONV, -ONE, WORKL(IUPTRI+(J-1)*LDQ), 1)
            END IF

 20      CONTINUE

         IF (HOWMNY .EQ. 'A') THEN

C           %--------------------------------------------%
C           | COMPUTE THE NCONV WANTED EIGENVECTORS OF T |
C           | LOCATED IN WORKL(IUPTRI,LDQ).              |
C           %--------------------------------------------%

            DO 30 J=1, NCV
               IF (J .LE. NCONV) THEN
                  SELECT(J) = .TRUE.
               ELSE
                  SELECT(J) = .FALSE.
               END IF
 30         CONTINUE

            CALL FTREVC ('R', 'S', SELECT, NCV, WORKL(IUPTRI),
     &           LDQ, VL, 1, WORKL(INVSUB), LDQ, NCV, OUTNCV, WORKEV,
     &           IERR)

            IF (IERR .NE. 0) THEN
                INFO = -9
                GO TO 9000
            END IF

C           %------------------------------------------------%
C           | SCALE THE RETURNING EIGENVECTORS SO THAT THEIR |
C           | EUCLIDEAN NORMS ARE ALL ONE. LAPACK SUBROUTINE |
C           | FTREVC RETURNS EACH EIGENVECTOR NORMALIZED SO  |
C           | THAT THE ELEMENT OF LARGEST MAGNITUDE HAS      |
C           | MAGNITUDE 1,                                   |
C           %------------------------------------------------%

            ICONJ = 0
            DO 40 J=1, NCONV

               IF ( WORKL(IHEIGI+J-1) .EQ. ZERO ) THEN

C                 %----------------------%
C                 | REAL EIGENVALUE CASE |
C                 %----------------------%

                  TEMP = DNRM2( NCV, WORKL(INVSUB+(J-1)*LDQ), 1 )
                  CALL DSCAL ( NCV, ONE / TEMP,
     &                 WORKL(INVSUB+(J-1)*LDQ), 1 )

               ELSE

C                 %-------------------------------------------%
C                 | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
C                 | SINCE THE REAL AND IMAGINARY PART OF      |
C                 | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
C                 | COLUMNS, WE FURTHER NORMALIZE BY THE      |
C                 | SQUARE ROOT OF TWO.                       |
C                 %-------------------------------------------%

                 IF (ICONJ .EQ. 0) THEN
                   TEMP = FLAPY2( DNRM2( NCV, WORKL(INVSUB+(J-1)*LDQ),
     &                    1 ), DNRM2( NCV, WORKL(INVSUB+J*LDQ),  1) )
                   CALL DSCAL ( NCV, ONE / TEMP,
     &                    WORKL(INVSUB+(J-1)*LDQ), 1 )
                   CALL DSCAL ( NCV, ONE / TEMP,
     &                    WORKL(INVSUB+J*LDQ), 1 )
                   ICONJ = 1
                 ELSE
                     ICONJ = 0
                  END IF

               END IF

 40         CONTINUE

            CALL DGEMV('T', NCV, NCONV, ONE, WORKL(INVSUB),
     &                LDQ, WORKL(IHBDS), 1, ZERO,  WORKEV, 1)

            ICONJ = 0
            DO 45 J=1, NCONV
               IF (WORKL(IHEIGI+J-1) .NE. ZERO) THEN

C                 %-------------------------------------------%
C                 | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
C                 | SINCE THE REAL AND IMAGINARY PART OF      |
C                 | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
C                 %-------------------------------------------%

                  IF (ICONJ .EQ. 0) THEN
                     WORKEV(J) = FLAPY2(WORKEV(J), WORKEV(J+1))
                     WORKEV(J+1) = WORKEV(J)
                     ICONJ = 1
                  ELSE
                     ICONJ = 0
                  END IF
               END IF
 45         CONTINUE

            IF (MSGLVL .GT. 2) THEN
                CALL DCOPY(NCV, WORKL(INVSUB+NCV-1), LDQ,
     &                    WORKL(IHBDS), 1)
                CALL DVOUT (LOGFIL, NCV, WORKL(IHBDS), NDIGIT,
     &              '_NEUPD: LAST ROW OF THE EIGENVECTOR MATRIX FOR T')
                IF (MSGLVL .GT. 3) THEN
                  CALL DMOUT (LOGFIL, NCV, NCV, WORKL(INVSUB), LDQ,
     &                 NDIGIT, '_NEUPD: THE EIGENVECTOR MATRIX FOR T')
                ENDIF
            ENDIF

C           %---------------------------------------%
C           | COPY RITZ ESTIMATES INTO WORKL(IHBDS) |
C           %---------------------------------------%

            CALL DCOPY(NCONV, WORKEV, 1, WORKL(IHBDS), 1)

C           %---------------------------------------------------------%
C           | COMPUTE THE QR FACTORIZATION OF THE EIGENVECTOR MATRIX  |
C           | ASSOCIATED WITH LEADING PORTION OF T IN THE FIRST NCONV |
C           | COLUMNS OF WORKL(INVSUB,LDQ).                           |
C           %---------------------------------------------------------%

            CALL FGEQR2 (NCV, NCONV, WORKL(INVSUB), LDQ, WORKEV,
     &                   WORKEV(NCV+1), IERR)

C           %----------------------------------------------%
C           | * POSTMULTIPLY Z BY Q.                       |
C           | * POSTMULTIPLY Z BY R.                       |
C           | THE N BY NCONV MATRIX Z IS NOW CONTAINS THE  |
C           | RITZ VECTORS ASSOCIATED WITH THE RITZ VALUES |
C           | IN WORKL(IHEIGR) AND WORKL(IHEIGI).          |
C           %----------------------------------------------%
C DUE TO CRP102
C          CALL FORM2R ('RIGHT', 'NOTRANSPOSE', N, NCV, NCONV,
C     &         WORKL(INVSUB), LDQ, WORKEV, Z, LDZ, WORKD(N+1), IERR)
            CALL FORM2R ('R', 'N', N, NCV, NCONV,
     &           WORKL(INVSUB), LDQ, WORKEV, Z, LDZ, WORKD(N+1), IERR)

C DUE TO CRP102 CALL DTRMM('RIGHT','UPPER','NO TRANSPOSE','NON-UNIT',
            CALL DTRMM ('R', 'U', 'N', 'N',
     &                  N, NCONV, ONE, WORKL(INVSUB), LDQ, Z, LDZ)

         END IF

      ELSE
C
C        %------------------------------------------------------%
C        | AN APPROXIMATE INVARIANT SUBSPACE IS NOT NEEDED.     |
C        | PLACE THE RITZ VALUES COMPUTED DNAUPD INTO DR AND DI |
C        %------------------------------------------------------%

         CALL DCOPY (NCONV, WORKL(RITZR), 1, DR, 1)
         CALL DCOPY (NCONV, WORKL(RITZI), 1, DI, 1)
         CALL DCOPY (NCONV, WORKL(RITZR), 1, WORKL(IHEIGR), 1)
         CALL DCOPY (NCONV, WORKL(RITZI), 1, WORKL(IHEIGI), 1)
         CALL DCOPY (NCONV, WORKL(BOUNDS), 1, WORKL(IHBDS), 1)
      END IF

C     %------------------------------------------------%
C     | TRANSFORM THE RITZ VALUES AND POSSIBLY VECTORS |
C     | AND CORRESPONDING ERROR BOUNDS OF OP TO THOSE  |
C     | OF A*X = LAMBDA*B*X.                           |
C     %------------------------------------------------%

      IF (TYPE .EQ. 'REGULR') THEN

         IF (RVEC)
     &      CALL DSCAL (NCV, RNORM, WORKL(IHBDS), 1)

      ELSE

C        %---------------------------------------%
C        |   A SPECTRAL TRANSFORMATION WAS USED. |
C        | * DETERMINE THE RITZ ESTIMATES OF THE |
C        |   RITZ VALUES IN THE ORIGINAL SYSTEM. |
C        %---------------------------------------%

         IF (TYPE .EQ. 'SHIFTI') THEN

            IF (RVEC)
     &         CALL DSCAL (NCV, RNORM, WORKL(IHBDS), 1)

            DO 50 K=1, NCV
              TEMP = FLAPY2( WORKL(IHEIGR+K-1),
     &                       WORKL(IHEIGI+K-1) )

              IF (TEMP * TEMP .LE. EPS) THEN
                IF (MSGLVL.GT.0) THEN
                  WRITE(LOGFIL,*)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)'&         DNEUPD_1                 &'
               WRITE(LOGFIL,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
               WRITE(LOGFIL,*)'& EPS    = ',EPS
               WRITE(LOGFIL,*)'& TEMP*2 = ',TEMP*TEMP
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)
                ENDIF
                WORKL(IHBDS+K-1)=ABS(WORKL(IHBDS+K-1))/EPS
               ELSE
                 WORKL(IHBDS+K-1) = ABS( WORKL(IHBDS+K-1) )
     &                          / TEMP / TEMP
               ENDIF
 50         CONTINUE
C
         ELSE IF (TYPE .EQ. 'REALPT') THEN
C
            DO 60 K=1, NCV
 60         CONTINUE
C
         ELSE IF (TYPE .EQ. 'IMAGPT') THEN
C
            DO 70 K=1, NCV
 70         CONTINUE
C
         END IF

C        %-----------------------------------------------------------%
C        | *  TRANSFORM THE RITZ VALUES BACK TO THE ORIGINAL SYSTEM. |
C        |    FOR TYPE = 'SHIFTI' THE TRANSFORMATION IS              |
C        |             LAMBDA = 1/THETA + SIGMA                      |
C        |    FOR TYPE = 'REALPT' OR 'IMAGPT' THE USER MUST FROM     |
C        |    RAYLEIGH QUOTIENTS OR A PROJECTION. SEE REMARK 3 ABOVE.|
C        | NOTES:                                                    |
C        | *THE RITZ VECTORS ARE NOT AFFECTED BY THE TRANSFORMATION. |
C        %-----------------------------------------------------------%

         IF (TYPE .EQ. 'SHIFTI') THEN
C
            DO 80 K=1, NCV
               TEMP = FLAPY2( WORKL(IHEIGR+K-1),
     &                        WORKL(IHEIGI+K-1) )
               IF (TEMP * TEMP.LE.EPS) THEN
                 WORKL(IHEIGR+K-1) = WORKL(IHEIGR+K-1) / EPS
     &                           + SIGMAR
                 WORKL(IHEIGI+K-1) = -WORKL(IHEIGI+K-1) / EPS
     &                           + SIGMAI
                IF (MSGLVL.GT.0) THEN
                  WRITE(LOGFIL,*)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)'&         DNEUPD_2                 &'
               WRITE(LOGFIL,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
               WRITE(LOGFIL,*)'& EPS    = ',EPS
               WRITE(LOGFIL,*)'& TEMP*2 = ',TEMP*TEMP
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                  WRITE(LOGFIL,*)
                ENDIF
                      ELSE
                 WORKL(IHEIGR+K-1) = WORKL(IHEIGR+K-1) / TEMP / TEMP
     &                           + SIGMAR
                 WORKL(IHEIGI+K-1) = -WORKL(IHEIGI+K-1) / TEMP / TEMP
     &                           + SIGMAI
               ENDIF

 80         CONTINUE
C
            CALL DCOPY (NCONV, WORKL(IHEIGR), 1, DR, 1)
            CALL DCOPY (NCONV, WORKL(IHEIGI), 1, DI, 1)
C
         ELSE IF (TYPE .EQ. 'REALPT' .OR. TYPE .EQ. 'IMAGPT') THEN
C
            CALL DCOPY (NCONV, WORKL(IHEIGR), 1, DR, 1)
            CALL DCOPY (NCONV, WORKL(IHEIGI), 1, DI, 1)
C
         END IF
C
      END IF
C
      IF (TYPE .EQ. 'SHIFTI' .AND. MSGLVL .GT. 1) THEN
          CALL DVOUT (LOGFIL, NCONV, DR, NDIGIT,
     &    '_NEUPD: UNTRANSFORMED REAL PART OF THE RITZ VALUESS.')
          CALL DVOUT (LOGFIL, NCONV, DI, NDIGIT,
     &    '_NEUPD: UNTRANSFORMED IMAG PART OF THE RITZ VALUESS.')
          CALL DVOUT (LOGFIL, NCONV, WORKL(IHBDS), NDIGIT,
     &    '_NEUPD: RITZ ESTIMATES OF UNTRANSFORMED RITZ VALUES.')
      ELSE IF (TYPE .EQ. 'REGULR' .AND. MSGLVL .GT. 1) THEN
          CALL DVOUT (LOGFIL, NCONV, DR, NDIGIT,
     &    '_NEUPD: REAL PARTS OF CONVERGED RITZ VALUES.')
          CALL DVOUT (LOGFIL, NCONV, DI, NDIGIT,
     &    '_NEUPD: IMAG PARTS OF CONVERGED RITZ VALUES.')
          CALL DVOUT (LOGFIL, NCONV, WORKL(IHBDS), NDIGIT,
     &    '_NEUPD: ASSOCIATED RITZ ESTIMATES.')
      ENDIF

C     %-------------------------------------------------%
C     | EIGENVECTOR PURIFICATION STEP. FORMALLY PERFORM |
C     | ONE OF INVERSE SUBSPACE ITERATION. ONLY USED    |
C     | FOR MODE = 2.                                   |
C     %-------------------------------------------------%

      IF (RVEC .AND. HOWMNY .EQ. 'A' .AND. TYPE .EQ. 'SHIFTI') THEN

C        %------------------------------------------------%
C        | PURIFY THE COMPUTED RITZ VECTORS BY ADDING A   |
C        | LITTLE BIT OF THE RESIDUAL VECTOR:             |
C        |                      T                         |
C        |          RESID(:)*( E    S ) / THETA           |
C        |                      NCV                       |
C        | WHERE H S = S THETA. REMEMBER THAT WHEN THETA  |
C        | HAS NONZERO IMAGINARY PART, THE CORRESPONDING  |
C        | RITZ VECTOR IS STORED ACROSS TWO COLUMNS OF Z. |
C        %------------------------------------------------%

         ICONJ = 0
         DO 110 J=1, NCONV
            IF (WORKL(IHEIGI+J-1) .EQ. ZERO) THEN
               IF (ABS(WORKL(IHEIGR+J-1)).LE.EPS) THEN
                 WORKEV(J) =  WORKL(INVSUB+(J-1)*LDQ+NCV-1) /
     &                        EPS
                IF (MSGLVL.GT.0) THEN
                  WRITE(LOGFIL,*)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)'&         DNEUPD_3                 &'
               WRITE(LOGFIL,*)'& DIV PAR EPS AU LIEU DE WORKL     &'
               WRITE(LOGFIL,*)'& EPS    = ',EPS
               WRITE(LOGFIL,*)'& WORKL  = ',WORKL(IHEIGR+J-1)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)
                ENDIF
               ELSE
                 WORKEV(J) =  WORKL(INVSUB+(J-1)*LDQ+NCV-1) /
     &                        WORKL(IHEIGR+J-1)
               ENDIF

            ELSE IF (ICONJ .EQ. 0) THEN
               TEMP = FLAPY2( WORKL(IHEIGR+J-1), WORKL(IHEIGI+J-1) )

               IF (TEMP * TEMP.LE.EPS) THEN
                 WORKEV(J) = ( WORKL(INVSUB+(J-1)*LDQ+NCV-1) *
     &                         WORKL(IHEIGR+J-1) +
     &                         WORKL(INVSUB+J*LDQ+NCV-1) *
     &                         WORKL(IHEIGI+J-1) ) / EPS
                 WORKEV(J+1) = ( WORKL(INVSUB+J*LDQ+NCV-1) *
     &                           WORKL(IHEIGR+J-1) -
     &                           WORKL(INVSUB+(J-1)*LDQ+NCV-1) *
     &                           WORKL(IHEIGI+J-1) ) / EPS
                IF (MSGLVL.GT.0) THEN
                  WRITE(LOGFIL,*)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)'&         DNEUPD_4                 &'
               WRITE(LOGFIL,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
               WRITE(LOGFIL,*)'& EPS    = ',EPS
               WRITE(LOGFIL,*)'& TEMP*2 = ',TEMP*TEMP
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                  WRITE(LOGFIL,*)
                ENDIF
               ELSE
                 WORKEV(J) = ( WORKL(INVSUB+(J-1)*LDQ+NCV-1) *
     &                         WORKL(IHEIGR+J-1) +
     &                         WORKL(INVSUB+J*LDQ+NCV-1) *
     &                         WORKL(IHEIGI+J-1) ) / TEMP / TEMP
                 WORKEV(J+1) = ( WORKL(INVSUB+J*LDQ+NCV-1) *
     &                           WORKL(IHEIGR+J-1) -
     &                           WORKL(INVSUB+(J-1)*LDQ+NCV-1) *
     &                           WORKL(IHEIGI+J-1) ) / TEMP / TEMP
               ENDIF
               ICONJ = 1
            ELSE
               ICONJ = 0
            END IF
 110     CONTINUE

C        %---------------------------------------%
C        | PERFORM A RANK ONE UPDATE TO Z AND    |
C        | PURIFY ALL THE RITZ VECTORS TOGETHER. |
C        %---------------------------------------%

         CALL DGER (N, NCONV, ONE, RESID, 1, WORKEV, 1, Z, LDZ)

      END IF

 9000 CONTINUE

C     %---------------%
C     | END OF DNEUPD |
C     %---------------%

      END
