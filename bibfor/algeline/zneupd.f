      SUBROUTINE ZNEUPD (RVEC , HOWMNY, SELECT, D     ,
     &                   Z    , LDZ   , SIGMA , WORKEV,
     &                   BMAT , N     , WHICH , NEV   ,
     &                   TOL  , RESID , NCV   , V     ,
     &                   LDV  , IPARAM, IPNTR , WORKD ,
     &                   WORKL, LWORKL, RWORK , INFO  )
C MODIF ALGELINE  DATE 11/01/2005   AUTEUR VABHHTS J.PELLET 
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C TOLE CRP_20 CRP_21 CRP_4
C
C     SUBROUTINE ARPACK CALCULANT LES MODES PROPRES DU PROBLEME
C     INITIAL.
C---------------------------------------------------------------------
C\BEGINDOC
C
C\NAME: ZNEUPD
C
C\DESCRIPTION:
C  THIS SUBROUTINE RETURNS THE CONVERGED APPROXIMATIONS TO EIGENVALUES
C  OF A*Z = LAMBDA*B*Z AND (OPTIONALLY):
C
C      (1) THE CORRESPONDING APPROXIMATE EIGENVECTORS;
C
C      (2) AN ORTHONORMAL BASIS FOR THE ASSOCIATED APPROXIMATE
C          INVARIANT SUBSPACE;
C
C      (3) BOTH.
C
C  THERE IS NEGLIGIBLE ADDITIONAL COST TO OBTAIN EIGENVECTORS.  AN
C  ORTHONORMAL
C  BASIS IS ALWAYS COMPUTED.  THERE IS AN ADDITIONAL STORAGE COST OF
C  N*NEV
C  IF BOTH ARE REQUESTED (IN THIS CASE A SEPARATE ARRAY Z MUST BE
C  SUPPLIED).
C
C  THE APPROXIMATE EIGENVALUES AND EIGENVECTORS OF  A*Z = LAMBDA*B*Z
C  ARE DERIVED FROM APPROXIMATE EIGENVALUES AND EIGENVECTORS OF
C  OF THE LINEAR OPERATOR OP PRESCRIBED BY THE MODE SELECTION IN THE
C  CALL TO ZNAUPD .  ZNAUPD  MUST BE CALLED BEFORE THIS ROUTINE IS
C  CALLED.
C  THESE APPROXIMATE EIGENVALUES AND VECTORS ARE COMMONLY CALLED RITZ
C  VALUES AND RITZ VECTORS RESPECTIVELY.  THEY ARE REFERRED TO AS SUCH
C  IN THE COMMENTS THAT FOLLOW.   THE COMPUTED ORTHONORMAL BASIS FOR
C  THE
C  INVARIANT SUBSPACE CORRESPONDING TO THESE RITZ VALUES IS REFERRED TO
C  AS A SCHUR BASIS.
C
C  THE DEFINITION OF OP AS WELL AS OTHER TERMS AND THE RELATION OF
C  COMPUTED
C  RITZ VALUES AND VECTORS OF OP WITH RESPECT TO THE GIVEN PROBLEM
C  A*Z = LAMBDA*B*Z MAY BE FOUND IN THE HEADER OF ZNAUPD .  FOR A BRIEF
C  DESCRIPTION, SEE DEFINITIONS OF IPARAM(7), MODE AND WHICH IN THE
C  DOCUMENTATION OF ZNAUPD .
C
C\USAGE:
C  CALL ZNEUPD
C     ( RVEC, HOWMNY, SELECT, D, Z, LDZ, SIGMA, WORKEV, BMAT,
C       N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD,
C       WORKL, LWORKL, RWORK, INFO )
C
C\ARGUMENTS:
C  RVEC    LOGICAL  (INPUT)
C          SPECIFIES WHETHER A BASIS FOR THE INVARIANT SUBSPACE
C          CORRESPONDING
C          TO THE CONVERGED RITZ VALUE APPROXIMATIONS FOR THE
C          EIGENPROBLEM
C          A*Z = LAMBDA*B*Z IS COMPUTED.
C
C             RVEC = .FALSE.     COMPUTE RITZ VALUES ONLY.
C
C             RVEC = .TRUE.      COMPUTE RITZ VECTORS OR SCHUR VECTORS.
C                                SEE REMARKS BELOW.
C
C  HOWMNY  CHARACTER*1  (INPUT)
C          SPECIFIES THE FORM OF THE BASIS FOR THE INVARIANT SUBSPACE
C          CORRESPONDING TO THE CONVERGED RITZ VALUES THAT IS TO BE
C          COMPUTED.
C
C          = 'A': COMPUTE NEV RITZ VECTORS;
C          = 'P': COMPUTE NEV SCHUR VECTORS;
C          = 'S': COMPUTE SOME OF THE RITZ VECTORS, SPECIFIED
C                 BY THE LOGICAL ARRAY SELECT.
C
C  SELECT  LOGICAL ARRAY OF DIMENSION NCV.  (INPUT)
C          IF HOWMNY = 'S', SELECT SPECIFIES THE RITZ VECTORS TO BE
C          COMPUTED. TO SELECT THE  RITZ VECTOR CORRESPONDING TO A
C          RITZ VALUE D(J), SELECT(J) MUST BE SET TO .TRUE..
C          IF HOWMNY = 'A' OR 'P', SELECT NEED NOT BE INITIALIZED
C          BUT IT IS USED AS INTERNAL WORKSPACE.
C
C  D       COMPLEX*16  ARRAY OF DIMENSION NEV+1.  (OUTPUT)
C          ON EXIT, D CONTAINS THE  RITZ  APPROXIMATIONS
C          TO THE EIGENVALUES LAMBDA FOR A*Z = LAMBDA*B*Z.
C
C  Z       COMPLEX*16  N BY NEV ARRAY    (OUTPUT)
C          ON EXIT, IF RVEC = .TRUE. AND HOWMNY = 'A', THEN THE COLUMNS
C          OF Z REPRESENTS APPROXIMATE EIGENVECTORS (RITZ VECTORS)
C          CORRESPONDING
C          TO THE NCONV=IPARAM(5) RITZ VALUES FOR EIGENSYSTEM
C          A*Z = LAMBDA*B*Z.
C
C          IF RVEC = .FALSE. OR HOWMNY = 'P', THEN Z IS NOT REFERENCED.
C
C          NOTE: IF IF RVEC = .TRUE. AND A SCHUR BASIS IS NOT REQUIRED,
C          THE ARRAY Z MAY BE SET EQUAL TO FIRST NEV+1 COLUMNS OF THE
C          ARNOLDI
C          BASIS ARRAY V COMPUTED BY ZNAUPD .  IN THIS CASE THE ARNOLDI
C          BASIS WILL BE DESTROYED AND OVERWRITTEN WITH THE EIGENVECTOR
C          BASIS.
C
C  LDZ     INTEGER.  (INPUT)
C          THE LEADING DIMENSION OF THE ARRAY Z.  IF RITZ VECTORS ARE
C          DESIRED, THEN  LDZ .GE.  MAX( 1, N ) IS REQUIRED.
C          IN ANY CASE,  LDZ .GE. 1 IS REQUIRED.
C
C  SIGMA   COMPLEX*16   (INPUT)
C          IF IPARAM(7) = 3 THEN SIGMA REPRESENTS THE SHIFT.
C          NOT REFERENCED IF IPARAM(7) = 1 OR 2.
C
C  WORKEV  COMPLEX*16  WORK ARRAY OF DIMENSION 2*NCV.  (WORKSPACE)
C
C  **** THE REMAINING ARGUMENTS MUST BE THE SAME AS FOR THE   ****
C  **** CALL TO ZNAUPD  THAT WAS JUST COMPLETED.               ****
C
C  NOTE: THE REMAINING ARGUMENTS
C
C           BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR,
C           WORKD, WORKL, LWORKL, RWORK, INFO
C
C         MUST BE PASSED DIRECTLY TO ZNEUPD  FOLLOWING THE LAST CALL
C         TO ZNAUPD .  THESE ARGUMENTS MUST NOT BE MODIFIED BETWEEN
C         THE THE LAST CALL TO ZNAUPD  AND THE CALL TO ZNEUPD .
C
C  THREE OF THESE PARAMETERS (V, WORKL AND INFO) ARE ALSO OUTPUT
C  PARAMETERS:
C
C  V       COMPLEX*16  N BY NCV ARRAY.  (INPUT/OUTPUT)
C
C          UPON INPUT: THE NCV COLUMNS OF V CONTAIN THE ARNOLDI BASIS
C                      VECTORS FOR OP AS CONSTRUCTED BY ZNAUPD  .
C
C          UPON OUTPUT: IF RVEC = .TRUE. THE FIRST NCONV=IPARAM(5)
C                       COLUMNS
C                       CONTAIN APPROXIMATE SCHUR VECTORS THAT SPAN THE
C                       DESIRED INVARIANT SUBSPACE.
C
C          NOTE: IF THE ARRAY Z HAS BEEN SET EQUAL TO FIRST NEV+1
C          COLUMNS
C          OF THE ARRAY V AND RVEC=.TRUE. AND HOWMNY= 'A', THEN THE
C          ARNOLDI BASIS HELD BY V HAS BEEN OVERWRITTEN BY THE DESIRED
C          RITZ VECTORS.  IF A SEPARATE ARRAY Z HAS BEEN PASSED THEN
C          THE FIRST NCONV=IPARAM(5) COLUMNS OF V WILL CONTAIN
C          APPROXIMATE
C          SCHUR VECTORS THAT SPAN THE DESIRED INVARIANT SUBSPACE.
C
C  WORKL   DOUBLE PRECISION  WORK ARRAY OF LENGTH LWORKL.
C          (OUTPUT/WORKSPACE)
C          WORKL(1:NCV*NCV+2*NCV) CONTAINS INFORMATION OBTAINED IN
C          ZNAUPD .  THEY ARE NOT CHANGED BY ZNEUPD .
C          WORKL(NCV*NCV+2*NCV+1:3*NCV*NCV+4*NCV) HOLDS THE
C          UNTRANSFORMED RITZ VALUES, THE UNTRANSFORMED ERROR
C          ESTIMATES OF
C          THE RITZ VALUES, THE UPPER TRIANGULAR MATRIX FOR H, AND THE
C          ASSOCIATED MATRIX REPRESENTATION OF THE INVARIANT SUBSPACE
C          FOR H.
C
C          NOTE: IPNTR(9:13) CONTAINS THE POINTER INTO WORKL FOR
C          ADDRESSES OF THE ABOVE INFORMATION COMPUTED BY ZNEUPD .
C         -------------------------------------------------------------
C          IPNTR(9):  POINTER TO THE NCV RITZ VALUES OF THE
C                     ORIGINAL SYSTEM.
C          IPNTR(10): NOT USED
C          IPNTR(11): POINTER TO THE NCV CORRESPONDING ERROR ESTIMATES.
C          IPNTR(12): POINTER TO THE NCV BY NCV UPPER TRIANGULAR
C                     SCHUR MATRIX FOR H.
C          IPNTR(13): POINTER TO THE NCV BY NCV MATRIX OF EIGENVECTORS
C                     OF THE UPPER HESSENBERG MATRIX H. ONLY REFERENCED
C                     BY ZNEUPD  IF RVEC = .TRUE. SEE REMARK 2 BELOW.
C        -------------------------------------------------------------
C
C  INFO    INTEGER.  (OUTPUT)
C          ERROR FLAG ON OUTPUT.
C          =  0: NORMAL EXIT.
C
C          =  1: THE SCHUR FORM COMPUTED BY LAPACK ROUTINE CSHEQR
C                COULD NOT BE REORDERED BY LAPACK ROUTINE GTRSEN .
C                RE-ENTER SUBROUTINE ZNEUPD  WITH IPARAM(5)=NCV AND
C                INCREASE THE SIZE OF THE ARRAY D TO HAVE
C                DIMENSION AT LEAST DIMENSION NCV AND ALLOCATE AT LEAST
C                NCV
C                COLUMNS FOR Z. NOTE: NOT NECESSARY IF Z AND V SHARE
C                THE SAME SPACE. PLEASE NOTIFY THE AUTHORS IF THIS ERROR
C                OCCURS.
C
C          = -1: N MUST BE POSITIVE.
C          = -2: NEV MUST BE POSITIVE.
C          = -3: NCV-NEV >= 2 AND LESS THAN OR EQUAL TO N.
C          = -5: WHICH MUST BE ONE OF 'LM', 'SM', 'LR', 'SR', 'LI', 'SI'
C          = -6: BMAT MUST BE ONE OF 'I' OR 'G'.
C          = -7: LENGTH OF PRIVATE WORK WORKL ARRAY IS NOT SUFFICIENT.
C          = -8: ERROR RETURN FROM LAPACK EIGENVALUE CALCULATION.
C                THIS SHOULD NEVER HAPPENED.
C          = -9: ERROR RETURN FROM CALCULATION OF EIGENVECTORS.
C                INFORMATIONAL ERROR FROM LAPACK ROUTINE GTREVC .
C          = -10: IPARAM(7) MUST BE 1,2,3
C          = -11: IPARAM(7) = 1 AND BMAT = 'G' ARE INCOMPATIBLE.
C          = -12: HOWMNY = 'S' NOT YET IMPLEMENTED
C          = -13: HOWMNY MUST BE ONE OF 'A' OR 'P' IF RVEC = .TRUE.
C          = -14: ZNAUPD  DID NOT FIND ANY EIGENVALUES TO SUFFICIENT
C                 ACCURACY.
C          = -15: ZNEUPD  GOT A DIFFERENT COUNT OF THE NUMBER OF
C                 CONVERGED RITZ VALUES THAN ZNAUPD  GOT.  THIS
C                 INDICATES THE USER
C                 PROBABLY MADE AN ERROR IN PASSING DATA FROM ZNAUPD TO
C                 ZNEUPD  OR THAT THE DATA WAS MODIFIED BEFORE ENTERING
C                 ZNEUPD
C
C\BEGINLIB
C
C\REFERENCES:
C  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
C     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
C     PP 357-385.
C  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
C     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
C     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
C  3. B. NOUR-OMID, B. N. PARLETT, T. ERICSSON AND P. S. JENSEN,
C     "HOW TO IMPLEMENT THE SPECTRAL TRANSFORMATION", MATH COMP.,
C     VOL. 48, NO. 178, APRIL, 1987 PP. 664-673.
C
C\ROUTINES CALLED:
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     ZMOUT    ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     ZVOUT    ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     GGEQR2   LAPACK ROUTINE THAT COMPUTES THE QR FACTORIZATION OF
C             A MATRIX.
C     GLACPY   LAPACK MATRIX COPY ROUTINE.
C     ZLAHQR   LAPACK ROUTINE THAT COMPUTES THE SCHUR FORM OF A
C             UPPER HESSENBERG MATRIX.
C     GLASET   LAPACK MATRIX INITIALIZATION ROUTINE.
C     GTREVC   LAPACK ROUTINE TO COMPUTE THE EIGENVECTORS OF A MATRIX
C             IN UPPER TRIANGULAR FORM.
C     GTRSEN   LAPACK ROUTINE THAT RE-ORDERS THE SCHUR FORM.
C     GUNM2R   LAPACK ROUTINE THAT APPLIES AN ORTHOGONAL MATRIX IN
C             FACTORED FORM.
C     ZTRMM    LEVEL 3 BLAS MATRIX TIMES AN UPPER TRIANGULAR MATRIX.
C     ZGERU    LEVEL 2 BLAS RANK ONE UPDATE TO A MATRIX.
C     ZCOPY    LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     ZSCAL    LEVEL 1 BLAS THAT SCALES A VECTOR.
C     ZDSCAL   LEVEL 1 BLAS THAT SCALES A COMPLEX VECTOR BY A REAL
C              NUMBER.
C     DZNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A COMPLEX VECTOR.
C
C\REMARKS
C
C  1. CURRENTLY ONLY HOWMNY = 'A' AND 'P' ARE IMPLEMENTED.
C
C  2. SCHUR VECTORS ARE AN ORTHOGONAL REPRESENTATION FOR THE BASIS OF
C     RITZ VECTORS. THUS, THEIR NUMERICAL PROPERTIES ARE OFTEN SUPERIOR.
C     IF RVEC = .TRUE. THEN THE RELATIONSHIP
C             A * V(:,1:IPARAM(5)) = V(:,1:IPARAM(5)) * T, AND
C       TRANSPOSE( V(:,1:IPARAM(5)) ) * V(:,1:IPARAM(5)) = I
C     ARE APPROXIMATELY SATISFIED.
C     HERE T IS THE LEADING SUBMATRIX OF ORDER IPARAM(5) OF THE
C     UPPER TRIANGULAR MATRIX STORED WORKL(IPNTR(12)).
C
C\AUTHORS
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     CHAO YANG                    HOUSTON, TEXAS
C     DEPT. OF COMPUTATIONAL &
C     APPLIED MATHEMATICS
C     RICE UNIVERSITY
C     HOUSTON, TEXAS
C
C\SCCS INFORMATION: @(#)
C FILE: NEUPD.F   SID: 2.7   DATE OF SID: 09/20/00   RELEASE: 2
C
C\ENDLIB
C
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

C
C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%
C
      CHARACTER*1 BMAT, HOWMNY
      CHARACTER*2 WHICH
      LOGICAL    RVEC
      INTEGER    INFO, LDZ, LDV, LWORKL, N, NCV, NEV
      COMPLEX*16 SIGMA
      REAL*8     TOL
C
C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%
C
      INTEGER    IPARAM(11), IPNTR(14)
      LOGICAL    SELECT(*)
      REAL*8     RWORK(*)
      COMPLEX*16 D(*)     , RESID(*)     , V(LDV,*),
     &           Z(LDZ,*),
     &           WORKD(3*N) , WORKL(LWORKL), WORKEV(2*NCV)
C
C     %------------%
C     | PARAMETERS |
C     %------------%
C
      COMPLEX*16 ONE, ZERO
      PARAMETER  (ONE = (1.0D+0, 0.0D+0) , ZERO = (0.0D+0, 0.0D+0) )
C
C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%
C
      CHARACTER*6  TYPE
      INTEGER*4 IERR4
      INTEGER    BOUNDS, IERR  , IH    , IHBDS, IHEIG , NCONV ,
     &           INVSUB, IUPTRI, IWEV  , J    , LDH   , LDQ   ,
     &           MODE  , MSGLVL, RITZ  , WR   , K     , IRZ   ,
     &           IBD   , OUTNCV, NP   , NUMCNV, JJ    ,
     &           ISHIFT
      COMPLEX*16 RNORM, TEMP, VL(1)
      REAL*8     CONDS, SEP, RTEMP, EPS23, EPS
      LOGICAL    REORD
C
C
C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%
C
      REAL*8     DZNRM2 , FLAPY2 , R8MIEM, R8PREM
C
      COMPLEX*16 ZDOTC
C
C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
C     %------------------------%
C     | SET DEFAULT PARAMETERS |
C     %------------------------%
C
      MSGLVL = MNEUPD
      EPS = R8MIEM()**(2.0D+0 / 3.0D+0)
      MODE = IPARAM(7)
      NCONV = IPARAM(5)
      INFO = 0
C
C
C     %---------------------------------%
C     | GET MACHINE DEPENDENT CONSTANT. |
C     %---------------------------------%
C
      EPS23 = R8PREM()*0.5D0
      EPS23 = EPS23**(2.0D+0 / 3.0D+0)
C
C     %-------------------------------%
C     | QUICK RETURN                  |
C     | CHECK FOR INCOMPATIBLE INPUT  |
C     %-------------------------------%
C
      IERR = 0
C
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
      ELSE IF (LWORKL .LT. 3*NCV**2 + 4*NCV) THEN
         IERR = -7
      ELSE IF ( (HOWMNY .NE. 'A' .AND.
     &           HOWMNY .NE. 'P' .AND.
     &           HOWMNY .NE. 'S') .AND. RVEC ) THEN
         IERR = -13
      ELSE IF (HOWMNY .EQ. 'S' ) THEN
         IERR = -12
      END IF
C
      IF (MODE .EQ. 1 .OR. MODE .EQ. 2) THEN
         TYPE = 'REGULR'
      ELSE IF (MODE .EQ. 3 ) THEN
         TYPE = 'SHIFTI'
      ELSE
                                              IERR = -10
      END IF
      IF (MODE .EQ. 1 .AND. BMAT .EQ. 'G')    IERR = -11
C
C     %------------%
C     | ERROR EXIT |
C     %------------%
C
      IF (IERR .NE. 0) THEN
         INFO = IERR
         GO TO 9000
      END IF
C
C     %--------------------------------------------------------%
C     | POINTER INTO WORKL FOR ADDRESS OF H, RITZ, WORKEV, Q   |
C     | ETC... AND THE REMAINING WORKSPACE.                    |
C     | ALSO UPDATE POINTER TO BE USED ON OUTPUT.              |
C     | MEMORY IS LAID OUT AS FOLLOWS:                         |
C     | WORKL(1:NCV*NCV) := GENERATED HESSENBERG MATRIX        |
C     | WORKL(NCV*NCV+1:NCV*NCV+NCV) := RITZ VALUES            |
C     | WORKL(NCV*NCV+NCV+1:NCV*NCV+2*NCV) := ERROR BOUNDS     |
C     %--------------------------------------------------------%
C
C     %-----------------------------------------------------------%
C     | THE FOLLOWING IS USED AND SET BY ZNEUPD .                 |
C     | WORKL(NCV*NCV+2*NCV+1:NCV*NCV+3*NCV) := THE UNTRANSFORMED |
C     |                                      RITZ VALUES.         |
C     | WORKL(NCV*NCV+3*NCV+1:NCV*NCV+4*NCV) := THE UNTRANSFORMED |
C     |                                      ERROR BOUNDS OF      |
C     |                                      THE RITZ VALUES      |
C     | WORKL(NCV*NCV+4*NCV+1:2*NCV*NCV+4*NCV) := HOLDS THE UPPER |
C     |                                      TRIANGULAR MATRIX    |
C     |                                      FOR H.               |
C     | WORKL(2*NCV*NCV+4*NCV+1: 3*NCV*NCV+4*NCV) := HOLDS THE    |
C     |                                      ASSOCIATED MATRIX    |
C     |                                      REPRESENTATION OF    |
C     |                                      THE INVARIANT        |
C     |                                      SUBSPACE FOR H.      |
C     | GRAND TOTAL OF NCV * ( 3 * NCV + 4 ) LOCATIONS.           |
C     %-----------------------------------------------------------%
C
      IH     = IPNTR(5)
      RITZ   = IPNTR(6)
      BOUNDS = IPNTR(8)
      LDH    = NCV
      LDQ    = NCV
      IHEIG  = BOUNDS + LDH
      IHBDS  = IHEIG  + LDH
      IUPTRI = IHBDS  + LDH
      INVSUB = IUPTRI + LDH*NCV
      IPNTR(9)  = IHEIG
      IPNTR(11) = IHBDS
      IPNTR(12) = IUPTRI
      IPNTR(13) = INVSUB
C      WR = 1
C      IWEV = WR + NCV

C
C     %-----------------------------------------%
C     | IRZ POINTS TO THE RITZ VALUES COMPUTED  |
C     |     BY _NEIGH BEFORE EXITING _NAUP2.    |
C     | IBD POINTS TO THE RITZ ESTIMATES        |
C     |     COMPUTED BY _NEIGH BEFORE EXITING   |
C     |     _NAUP2.                             |
C     %-----------------------------------------%
C
      IRZ = IPNTR(14) + NCV*NCV
      IBD = IRZ + NCV
C
C     %------------------------------------%
C     | RNORM IS B-NORM OF THE RESID(1:N). |
C     %------------------------------------%
C
      RNORM = WORKL(IH+2)
      WORKL(IH+2) = ZERO
C
      IF (MSGLVL .GT. 2) THEN
         CALL ZVOUT (LOGFIL, NCV, WORKL(IRZ), NDIGIT,
     &   '_NEUPD: RITZ VALUES PASSED IN FROM _NAUPD.')
         CALL ZVOUT (LOGFIL, NCV, WORKL(IBD), NDIGIT,
     &   '_NEUPD: RITZ ESTIMATES PASSED IN FROM _NAUPD.')
      END IF
C
      IF (RVEC) THEN
C
         REORD = .FALSE.
C
C        %---------------------------------------------------%
C        | USE THE TEMPORARY BOUNDS ARRAY TO STORE INDICES   |
C        | THESE WILL BE USED TO MARK THE SELECT ARRAY LATER |
C        %---------------------------------------------------%
C
         DO 10 J = 1,NCV
            WORKL(BOUNDS+J-1) = J
            SELECT(J) = .FALSE.
   10    CONTINUE
C
C        %-------------------------------------%
C        | SELECT THE WANTED RITZ VALUES.      |
C        | SORT THE RITZ VALUES SO THAT THE    |
C        | WANTED ONES APPEAR AT THE TAILING   |
C        | NEV POSITIONS OF WORKL(IRR) AND     |
C        | WORKL(IRI).  MOVE THE CORRESPONDING |
C        | ERROR ESTIMATES IN WORKL(IBD)       |
C        | ACCORDINGLY.                        |
C        %-------------------------------------%
C
         NP     = NCV - NEV
         ISHIFT = 0
         CALL ZNGETS (ISHIFT, WHICH     , NEV          ,
     &                NP    , WORKL(IRZ), WORKL(BOUNDS))
C
         IF (MSGLVL .GT. 2) THEN
            CALL ZVOUT  (LOGFIL, NCV, WORKL(IRZ), NDIGIT,
     &      '_NEUPD: RITZ VALUES AFTER CALLING _NGETS.')
            CALL ZVOUT  (LOGFIL, NCV, WORKL(BOUNDS), NDIGIT,
     &      '_NEUPD: RITZ VALUE INDICES AFTER CALLING _NGETS.')
         END IF
C
C        %-----------------------------------------------------%
C        | RECORD INDICES OF THE CONVERGED WANTED RITZ VALUES  |
C        | MARK THE SELECT ARRAY FOR POSSIBLE REORDERING       |
C        %-----------------------------------------------------%
C
         NUMCNV = 0
         DO 11 J = 1,NCV
            RTEMP = MAX(EPS23,
     &                 FLAPY2  ( DBLE (WORKL(IRZ+NCV-J)),
     &                          DIMAG (WORKL(IRZ+NCV-J)) ))
            JJ = WORKL(BOUNDS + NCV - J)
            IF (NUMCNV .LT. NCONV .AND.
     &          FLAPY2 ( DBLE (WORKL(IBD+JJ-1)),
     &          DIMAG (WORKL(IBD+JJ-1)) )
     &          .LE. TOL*RTEMP) THEN
               SELECT(JJ) = .TRUE.
               NUMCNV = NUMCNV + 1
               IF (JJ .GT. NEV) REORD = .TRUE.
            ENDIF
   11    CONTINUE
C
C        %-----------------------------------------------------------%
C        | CHECK THE COUNT (NUMCNV) OF CONVERGED RITZ VALUES WITH    |
C        | THE NUMBER (NCONV) REPORTED BY DNAUPD.  IF THESE TWO      |
C        | ARE DIFFERENT THEN THERE HAS PROBABLY BEEN AN ERROR       |
C        | CAUSED BY INCORRECT PASSING OF THE DNAUPD DATA.           |
C        %-----------------------------------------------------------%
C
         IF (MSGLVL .GT. 2) THEN
             CALL IVOUT(LOGFIL, 1, NUMCNV, NDIGIT,
     &            '_NEUPD: NUMBER OF SPECIFIED EIGENVALUES')
             CALL IVOUT(LOGFIL, 1, NCONV, NDIGIT,
     &            '_NEUPD: NUMBER OF "CONVERGED" EIGENVALUES')
         END IF
C
         IF (NUMCNV .NE. NCONV) THEN
            INFO = -15
            GO TO 9000
         END IF
C
C        %-------------------------------------------------------%
C        | CALL LAPACK ROUTINE ZLAHQR  TO COMPUTE THE SCHUR FORM |
C        | OF THE UPPER HESSENBERG MATRIX RETURNED BY ZNAUPD .   |
C        | MAKE A COPY OF THE UPPER HESSENBERG MATRIX.           |
C        | INITIALIZE THE SCHUR VECTOR MATRIX Q TO THE IDENTITY. |
C        %-------------------------------------------------------%
C
         CALL ZCOPY (LDH*NCV, WORKL(IH), 1, WORKL(IUPTRI), 1)
         CALL GLASET ('A', NCV, NCV          ,
     &                ZERO , ONE, WORKL(INVSUB),
     &                LDQ)
         CALL ZLAHQR (.TRUE., .TRUE.       , NCV          ,
     &                1     , NCV          , WORKL(IUPTRI),
     &                LDH   , WORKL(IHEIG) , 1            ,
     &                NCV   , WORKL(INVSUB), LDQ          ,
     &                IERR4)
         IERR=IERR4
         CALL ZCOPY (NCV         , WORKL(INVSUB+NCV-1), LDQ,
     &               WORKL(IHBDS), 1)
C
         IF (IERR .NE. 0) THEN
            INFO = -8
            GO TO 9000
         END IF
C
         IF (MSGLVL .GT. 1) THEN
            CALL ZVOUT  (LOGFIL, NCV, WORKL(IHEIG), NDIGIT,
     &           '_NEUPD: EIGENVALUES OF H')
            CALL ZVOUT  (LOGFIL, NCV, WORKL(IHBDS), NDIGIT,
     &           '_NEUPD: LAST ROW OF THE SCHUR VECTOR MATRIX')
            IF (MSGLVL .GT. 3) THEN
               CALL ZMOUT  (LOGFIL       , NCV, NCV   ,
     &                     WORKL(IUPTRI), LDH, NDIGIT,
     &              '_NEUPD: THE UPPER TRIANGULAR MATRIX ')
            END IF
         END IF
C
         IF (REORD) THEN
C
C           %-----------------------------------------------%
C           | REORDER THE COMPUTED UPPER TRIANGULAR MATRIX. |
C           %-----------------------------------------------%
C
            CALL GTRSEN ('N'       , 'V'          , SELECT      ,
     &                   NCV          , WORKL(IUPTRI), LDH         ,
     &                   WORKL(INVSUB), LDQ          , WORKL(IHEIG),
     &                   NCONV        , CONDS        , SEP         ,
     &                   WORKEV       , NCV          , IERR)
C
            IF (IERR .EQ. 1) THEN
               INFO = 1
               GO TO 9000
            END IF
C
            IF (MSGLVL .GT. 2) THEN
                CALL ZVOUT  (LOGFIL, NCV, WORKL(IHEIG), NDIGIT,
     &           '_NEUPD: EIGENVALUES OF H--REORDERED')
                IF (MSGLVL .GT. 3) THEN
                   CALL ZMOUT (LOGFIL       , NCV, NCV   ,
     &                         WORKL(IUPTRI), LDQ, NDIGIT,
     &              '_NEUPD: TRIANGULAR MATRIX AFTER RE-ORDERING')
                END IF
            END IF
C
         END IF
C
C        %---------------------------------------------%
C        | COPY THE LAST ROW OF THE SCHUR BASIS MATRIX |
C        | TO WORKL(IHBDS).  THIS VECTOR WILL BE USED  |
C        | TO COMPUTE THE RITZ ESTIMATES OF CONVERGED  |
C        | RITZ VALUES.                                |
C        %---------------------------------------------%
C
         CALL ZCOPY (NCV         , WORKL(INVSUB+NCV-1), LDQ,
     &               WORKL(IHBDS), 1)
C
C        %--------------------------------------------%
C        | PLACE THE COMPUTED EIGENVALUES OF H INTO D |
C        | IF A SPECTRAL TRANSFORMATION WAS NOT USED. |
C        %--------------------------------------------%
C
         IF (TYPE .EQ. 'REGULR') THEN
            CALL ZCOPY (NCONV, WORKL(IHEIG), 1, D, 1)
         END IF
C
C        %----------------------------------------------------------%
C        | COMPUTE THE QR FACTORIZATION OF THE MATRIX REPRESENTING  |
C        | THE WANTED INVARIANT SUBSPACE LOCATED IN THE FIRST NCONV |
C        | COLUMNS OF WORKL(INVSUB,LDQ).                            |
C        %----------------------------------------------------------%
C
         CALL GGEQR2 (NCV , NCONV , WORKL(INVSUB),
     &                LDQ , WORKEV, WORKEV(NCV+1),
     &                IERR)
C
C        %--------------------------------------------------------%
C        | * POSTMULTIPLY V BY Q USING GUNM2R .                    |
C        | * COPY THE FIRST NCONV COLUMNS OF VQ INTO Z.           |
C        | * POSTMULTIPLY Z BY R.                                 |
C        | THE N BY NCONV MATRIX Z IS NOW A MATRIX REPRESENTATION |
C        | OF THE APPROXIMATE INVARIANT SUBSPACE ASSOCIATED WITH  |
C        | THE RITZ VALUES IN WORKL(IHEIG). THE FIRST NCONV       |
C        | COLUMNS OF V ARE NOW APPROXIMATE SCHUR VECTORS         |
C        | ASSOCIATED WITH THE UPPER TRIANGULAR MATRIX OF ORDER   |
C        | NCONV IN WORKL(IUPTRI).                                |
C        %--------------------------------------------------------%
C
         CALL GUNM2R ('R', 'N', N            ,
     &                NCV    , NCONV        , WORKL(INVSUB),
     &                LDQ    , WORKEV       , V            ,
     &                LDV    , WORKD(N+1)   , IERR)
         CALL GLACPY ('A', N, NCONV, V, LDV, Z, LDZ)
C
         DO 20 J=1, NCONV
C
C           %---------------------------------------------------%
C           | PERFORM BOTH A COLUMN AND ROW SCALING IF THE      |
C           | DIAGONAL ELEMENT OF WORKL(INVSUB,LDQ) IS NEGATIVE |
C           | I'M LAZY AND DON'T TAKE ADVANTAGE OF THE UPPER    |
C           | TRIANGULAR FORM OF WORKL(IUPTRI,LDQ).             |
C           | NOTE THAT SINCE Q IS ORTHOGONAL, R IS A DIAGONAL  |
C           | MATRIX CONSISTING OF PLUS OR MINUS ONES.          |
C           %---------------------------------------------------%
C
            IF ( DBLE ( WORKL(INVSUB+(J-1)*LDQ+J-1) ) .LT.
     &                  DBLE (ZERO) ) THEN
               CALL ZSCAL (NCONV, -ONE, WORKL(IUPTRI+J-1), LDQ)
               CALL ZSCAL (NCONV, -ONE, WORKL(IUPTRI+(J-1)*LDQ), 1)
            END IF
C
 20      CONTINUE
C
         IF (HOWMNY .EQ. 'A') THEN
C
C           %--------------------------------------------%
C           | COMPUTE THE NCONV WANTED EIGENVECTORS OF T |
C           | LOCATED IN WORKL(IUPTRI,LDQ).              |
C           %--------------------------------------------%
C
            DO 30 J=1, NCV
               IF (J .LE. NCONV) THEN
                  SELECT(J) = .TRUE.
               ELSE
                  SELECT(J) = .FALSE.
               END IF
 30         CONTINUE
C
            CALL GTREVC ('R', 'S'     , SELECT       ,
     &                   NCV    , WORKL(IUPTRI), LDQ          ,
     &                   VL     , 1            , WORKL(INVSUB),
     &                   LDQ    , NCV          , OUTNCV       ,
     &                   WORKEV , RWORK        , IERR)
C
            IF (IERR .NE. 0) THEN
                INFO = -9
                GO TO 9000
            END IF
C
C           %------------------------------------------------%
C           | SCALE THE RETURNING EIGENVECTORS SO THAT THEIR |
C           | EUCLIDEAN NORMS ARE ALL ONE. LAPACK SUBROUTINE |
C           | GTREVC  RETURNS EACH EIGENVECTOR NORMALIZED SO  |
C           | THAT THE ELEMENT OF LARGEST MAGNITUDE HAS      |
C           | MAGNITUDE 1.                                   |
C           %------------------------------------------------%
C
            DO 40 J=1, NCONV
                  RTEMP = DZNRM2 (NCV, WORKL(INVSUB+(J-1)*LDQ), 1)
                  RTEMP = DBLE (ONE) / RTEMP
                  CALL ZDSCAL  ( NCV, RTEMP,
     &                 WORKL(INVSUB+(J-1)*LDQ), 1 )
C
C                 %------------------------------------------%
C                 | RITZ ESTIMATES CAN BE OBTAINED BY TAKING |
C                 | THE INNER PRODUCT OF THE LAST ROW OF THE |
C                 | SCHUR BASIS OF H WITH EIGENVECTORS OF T. |
C                 | NOTE THAT THE EIGENVECTOR MATRIX OF T IS |
C                 | UPPER TRIANGULAR, THUS THE LENGTH OF THE |
C                 | INNER PRODUCT CAN BE SET TO J.           |
C                 %------------------------------------------%
C
                  WORKEV(J) = ZDOTC (J, WORKL(IHBDS), 1,
     &                        WORKL(INVSUB+(J-1)*LDQ), 1)
 40         CONTINUE
C
            IF (MSGLVL .GT. 2) THEN
               CALL ZCOPY (NCONV, WORKL(INVSUB+NCV-1), LDQ,
     &                    WORKL(IHBDS), 1)
               CALL ZVOUT  (LOGFIL, NCONV, WORKL(IHBDS), NDIGIT,
     &            '_NEUPD: LAST ROW OF THE EIGENVECTOR MATRIX FOR T')
               IF (MSGLVL .GT. 3) THEN
                  CALL ZMOUT (LOGFIL       , NCV, NCV   ,
     &                        WORKL(INVSUB), LDQ, NDIGIT,
     &               '_NEUPD: THE EIGENVECTOR MATRIX FOR T')
               END IF
            END IF
C
C           %---------------------------------------%
C           | COPY RITZ ESTIMATES INTO WORKL(IHBDS) |
C           %---------------------------------------%
C
            CALL ZCOPY (NCONV, WORKEV, 1, WORKL(IHBDS), 1)
C
C           %----------------------------------------------%
C           | THE EIGENVECTOR MATRIX Q OF T IS TRIANGULAR. |
C           | FORM Z*Q.                                    |
C           %----------------------------------------------%
C
            CALL ZTRMM ('R'   , 'U'      , 'N',
     &                  'N', N            , NCONV         ,
     &                  ONE       , WORKL(INVSUB), LDQ           ,
     &                  Z         , LDZ)
         END IF
C
      ELSE
C
C        %--------------------------------------------------%
C        | AN APPROXIMATE INVARIANT SUBSPACE IS NOT NEEDED. |
C        | PLACE THE RITZ VALUES COMPUTED ZNAUPD  INTO D.    |
C        %--------------------------------------------------%
C
         CALL ZCOPY (NCONV, WORKL(RITZ), 1, D, 1)
         CALL ZCOPY (NCONV, WORKL(RITZ), 1, WORKL(IHEIG), 1)
         CALL ZCOPY (NCONV, WORKL(BOUNDS), 1, WORKL(IHBDS), 1)
C
      END IF
C
C     %------------------------------------------------%
C     | TRANSFORM THE RITZ VALUES AND POSSIBLY VECTORS |
C     | AND CORRESPONDING ERROR BOUNDS OF OP TO THOSE  |
C     | OF A*X = LAMBDA*B*X.                           |
C     %------------------------------------------------%
C
      IF (TYPE .EQ. 'REGULR') THEN
C
         IF (RVEC)
     &      CALL ZSCAL (NCV, RNORM, WORKL(IHBDS), 1)
C
      ELSE
C
C        %---------------------------------------%
C        |   A SPECTRAL TRANSFORMATION WAS USED. |
C        | * DETERMINE THE RITZ ESTIMATES OF THE |
C        |   RITZ VALUES IN THE ORIGINAL SYSTEM. |
C        %---------------------------------------%
C
         IF (RVEC)
     &      CALL ZSCAL (NCV, RNORM, WORKL(IHBDS), 1)
C
         DO 50 K=1, NCV
            TEMP = WORKL(IHEIG+K-1)
              IF (ABS(TEMP * TEMP) .LE. EPS) THEN
                IF (MSGLVL.GT.0) THEN
                  WRITE(LOGFIL,*)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)'&         ZNEUPD_1                 &'
               WRITE(LOGFIL,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
               WRITE(LOGFIL,*)'& EPS    = ',EPS
               WRITE(LOGFIL,*)'& TEMP*2 = ',TEMP*TEMP
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)
                ENDIF
                WORKL(IHBDS+K-1) = WORKL(IHBDS+K-1)/EPS
              ELSE
                WORKL(IHBDS+K-1) = WORKL(IHBDS+K-1) / TEMP / TEMP
            ENDIF
  50     CONTINUE
C
      END IF
C
C     %-----------------------------------------------------------%
C     | *  TRANSFORM THE RITZ VALUES BACK TO THE ORIGINAL SYSTEM. |
C     |    FOR TYPE = 'SHIFTI' THE TRANSFORMATION IS              |
C     |             LAMBDA = 1/THETA + SIGMA                      |
C     | NOTES:                                                    |
C     | *THE RITZ VECTORS ARE NOT AFFECTED BY THE TRANSFORMATION. |
C     %-----------------------------------------------------------%
C
      IF (TYPE .EQ. 'SHIFTI') THEN
         DO 60 K=1, NCONV
            D(K) = ONE / WORKL(IHEIG+K-1) + SIGMA
  60     CONTINUE
      END IF
C
      IF (TYPE .NE. 'REGULR' .AND. MSGLVL .GT. 1) THEN
         CALL ZVOUT  (LOGFIL, NCONV, D, NDIGIT,
     &     '_NEUPD: UNTRANSFORMED RITZ VALUES.')
         CALL ZVOUT  (LOGFIL, NCONV, WORKL(IHBDS), NDIGIT,
     &     '_NEUPD: RITZ ESTIMATES OF THE UNTRANSFORMED RITZ VALUES.')
      ELSE IF ( MSGLVL .GT. 1) THEN
         CALL ZVOUT  (LOGFIL, NCONV, D, NDIGIT,
     &     '_NEUPD: CONVERGED RITZ VALUES.')
         CALL ZVOUT  (LOGFIL, NCONV, WORKL(IHBDS), NDIGIT,
     &     '_NEUPD: ASSOCIATED RITZ ESTIMATES.')
      END IF
C
C     %-------------------------------------------------%
C     | EIGENVECTOR PURIFICATION STEP. FORMALLY PERFORM |
C     | ONE OF INVERSE SUBSPACE ITERATION. ONLY USED    |
C     | FOR MODE = 3. SEE REFERENCE 3.                  |
C     %-------------------------------------------------%
C
      IF (RVEC .AND. HOWMNY .EQ. 'A' .AND. TYPE .EQ. 'SHIFTI') THEN
C
C        %------------------------------------------------%
C        | PURIFY THE COMPUTED RITZ VECTORS BY ADDING A   |
C        | LITTLE BIT OF THE RESIDUAL VECTOR:             |
C        |                      T                         |
C        |          RESID(:)*( E    S ) / THETA           |
C        |                      NCV                       |
C        | WHERE H S = S THETA.                           |
C        %------------------------------------------------%
C
         DO 100 J=1, NCONV
            IF (WORKL(IHEIG+J-1) .NE. ZERO) THEN
               WORKEV(J) =  WORKL(INVSUB+(J-1)*LDQ+NCV-1) /
     &                      WORKL(IHEIG+J-1)
            ENDIF
 100     CONTINUE

C        %---------------------------------------%
C        | PERFORM A RANK ONE UPDATE TO Z AND    |
C        | PURIFY ALL THE RITZ VECTORS TOGETHER. |
C        %---------------------------------------%
C
         CALL ZGERU  (N, NCONV, ONE, RESID, 1, WORKEV, 1, Z, LDZ)
C
      END IF
C
 9000 CONTINUE
C
C
C     %---------------%
C     | END OF ZNEUPD |
C     %---------------%
C
      END
