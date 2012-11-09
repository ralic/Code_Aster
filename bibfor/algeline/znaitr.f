      SUBROUTINE ZNAITR
     &   (IDO, BMAT, N, K, NP, RESID, RNORM, V, LDV, H, LDH,
     &    IPNTR, WORKD, INFO, ALPHA)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C
C     SUBROUTINE ARPACK OPERANT NP ETAPE D'ARNOLDI A PARTIR D'UNE
C     FACTORISATION D'ORDRE K.
C-----------------------------------------------------------------------
C\BEGINDOC
C
C\NAME: ZNAITR
C
C\DESCRIPTION:
C  REVERSE COMMUNICATION INTERFACE FOR APPLYING NP ADDITIONAL STEPS TO
C  A K STEP NONSYMMETRIC ARNOLDI FACTORIZATION.
C
C  INPUT:  OP*V_{K}  -  V_{K}*H = R_{K}*E_{K}^T
C
C          WITH (V_{K}^T)*B*V_{K} = I, (V_{K}^T)*B*R_{K} = 0.
C
C  OUTPUT: OP*V_{K+P}  -  V_{K+P}*H = R_{K+P}*E_{K+P}^T
C
C          WITH (V_{K+P}^T)*B*V_{K+P} = I, (V_{K+P}^T)*B*R_{K+P} = 0.
C
C  WHERE OP AND B ARE AS IN ZNAUPD.  THE B-NORM OF R_{K+P} IS ALSO
C  COMPUTED AND RETURNED.
C
C\USAGE:
C  CALL ZNAITR
C     ( IDO, BMAT, N, K, NP, RESID, RNORM, V, LDV, H, LDH,
C       IPNTR, WORKD, INFO )
C
C\ARGUMENTS
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
C          RECOMPUTED IN FORMING OP * Q.
C
C  BMAT    CHARACTER*1.  (INPUT)
C          BMAT SPECIFIES THE TYPE OF THE MATRIX B THAT DEFINES THE
C          SEMI-INNER PRODUCT FOR THE OPERATOR OP.  SEE ZNAUPD.
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
C  RESID   COMPLEX*16 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C          ON INPUT:  RESID CONTAINS THE RESIDUAL VECTOR R_{K}.
C          ON OUTPUT: RESID CONTAINS THE RESIDUAL VECTOR R_{K+P}.
C
C  RNORM   DOUBLE PRECISION SCALAR.  (INPUT/OUTPUT)
C          B-NORM OF THE STARTING RESIDUAL ON INPUT.
C          B-NORM OF THE UPDATED RESIDUAL R_{K+P} ON OUTPUT.
C
C  V       COMPLEX*16 N BY K+NP ARRAY.  (INPUT/OUTPUT)
C          ON INPUT:  V CONTAINS THE ARNOLDI VECTORS IN THE FIRST K
C          COLUMNS.
C          ON OUTPUT: V CONTAINS THE NEW NP ARNOLDI VECTORS IN THE NEXT
C          NP COLUMNS.  THE FIRST K COLUMNS ARE UNCHANGED.
C
C  LDV     INTEGER.  (INPUT)
C          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  H       COMPLEX*16 (K+NP) BY (K+NP) ARRAY.  (INPUT/OUTPUT)
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
C  WORKD   COMPLEX*16 WORK ARRAY OF LENGTH 3*N.  (REVERSE COMMUNICATION)
C          DISTRIBUTED ARRAY TO BE USED IN THE BASIC ARNOLDI ITERATION
C          FOR REVERSE COMMUNICATION.  THE CALLING PROGRAM SHOULD NOT
C          USE WORKD AS TEMPORARY WORKSPACE DURING THE ITERATION !!!!!!
C          ON INPUT, WORKD(1:N) = B*RESID AND IS USED TO SAVE SOME
C          COMPUTATION AT THE FIRST STEP.
C
C  INFO    INTEGER.  (OUTPUT)
C          = 0: NORMAL EXIT.
C          > 0: SIZE OF THE SPANNING INVARIANT SUBSPACE OF OP FOUND.
C
C\ENDDOC
C
C----------------------------------------------------------------------
C
C\BEGINLIB
C
C\LOCAL VARIABLES:
C     XXXXXX  COMPLEX*16
C
C\REFERENCES:
C  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
C     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
C     PP 357-385.
C  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
C     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
C     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
C
C\ROUTINES CALLED:
C     ZGETV0  ARPACK ROUTINE TO GENERATE THE INITIAL VECTOR.
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     ZMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     ZVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C       LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
C     ZLASCL  LAPACK ROUTINE FOR CAREFUL SCALING OF A MATRIX.
C     DLABAD  LAPACK ROUTINE FOR DEFINING THE UNDERFLOW AND OVERFLOW
C             LIMITS.
C     DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C     ZGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
C     ZAXPY   LEVEL 1 BLAS THAT COMPUTES A VECTOR TRIAD.
C     ZCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     ZDOTC   LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT OF TWO
C              VECTORS.
C     ZSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
C     ZDSCAL  LEVEL 1 BLAS THAT SCALES A COMPLEX VECTOR BY A REAL
C             NUMBER.
C     DZNRM2  LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
C
C\AUTHOR
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
C     APPLIED MATHEMATICS
C     RICE UNIVERSITY
C     HOUSTON, TEXAS
C
C\SCCS INFORMATION: @(#)
C FILE: NAITR.F   SID: 2.3   DATE OF SID: 8/27/96   RELEASE: 2
C
C\REMARKS
C  THE ALGORITHM IMPLEMENTED IS:
C
C  RESTART = .FALSE.
C  GIVEN V_{K} = [V_{1}, ..., V_{K}], R_{K};
C  R_{K} CONTAINS THE INITIAL RESIDUAL VECTOR EVEN FOR K = 0;
C  ALSO ASSUME THAT RNORM = || B*R_{K} || AND B*R_{K} ARE ALREADY
C  COMPUTED BY THE CALLING PROGRAM.
C
C  BETAJ = RNORM ; P_{K+1} = B*R_{K} ;
C  FOR  J = K+1, ..., K+NP  DO
C     1) IF ( BETAJ < TOL ) STOP OR RESTART DEPENDING ON J.
C        ( AT PRESENT TOL IS ZERO )
C        IF ( RESTART ) GENERATE A NEW STARTING VECTOR.
C     2) V_{J} = R(J-1)/BETAJ;  V_{J} = [V_{J-1}, V_{J}];
C        P_{J} = P_{J}/BETAJ
C     3) R_{J} = OP*V_{J} WHERE OP IS DEFINED AS IN ZNAUPD
C        FOR SHIFT-INVERT MODE P_{J} = B*V_{J} IS ALREADY AVAILABLE.
C        WNORM = || OP*V_{J} ||
C     4) COMPUTE THE J-TH STEP RESIDUAL VECTOR.
C        W_{J} =  V_{J}^T * B * OP * V_{J}
C        R_{J} =  OP*V_{J} - V_{J} * W_{J}
C        H(:,J) = W_{J};
C        H(J,J-1) = RNORM
C        RNORM = || R_(J) ||
C        IF (RNORM > 0.717*WNORM) ACCEPT STEP AND GO BACK TO 1)
C     5) RE-ORTHOGONALIZATION STEP:
C        S = V_{J}'*B*R_{J}
C        R_{J} = R_{J} - V_{J}*S;  RNORM1 = || R_{J} ||
C        ALPHAJ = ALPHAJ + S_{J};
C     6) ITERATIVE REFINEMENT STEP:
C        IF (RNORM1 > 0.717*RNORM) THEN
C           RNORM = RNORM1
C           ACCEPT STEP AND GO BACK TO 1)
C        ELSE
C           RNORM = RNORM1
C           IF THIS IS THE FIRST TIME IN STEP 6), GO TO 5)
C           ELSE R_{J} LIES IN THE SPAN OF V_{J} NUMERICALLY.
C              SET R_{J} = 0 AND RNORM = 0; GO TO 1)
C        ENDIF
C  END DO
C
C\ENDLIB
C
C-----------------------------------------------------------------------
C TOLE CRP_4
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C
C     %----------------------------------------------------%
C     | INCLUDE FILES FOR DEBUGGING AND TIMING INFORMATION |
C     %----------------------------------------------------%
C
      INTEGER LOGFIL, NDIGIT, MGETV0,
     &  MNAUPD, MNAUP2, MNAITR, MNEIGH, MNAPPS, MNGETS, MNEUPD
      COMMON /DEBUG/
     &  LOGFIL, NDIGIT, MGETV0,
     &  MNAUPD, MNAUP2, MNAITR, MNEIGH, MNAPPS, MNGETS, MNEUPD
      INTEGER NOPX, NBX, NRORTH, NITREF, NRSTRT
      COMMON /INFOR/
     &  NOPX, NBX, NRORTH, NITREF, NRSTRT
C
C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%
C
      CHARACTER*1 BMAT
      INTEGER IDO, INFO, K, LDH, LDV, N, NP
      REAL*8 RNORM, ALPHA
C
C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%
C
      INTEGER    IPNTR(3)
      COMPLEX*16 H(LDH,K+NP), RESID(N), V(LDV,K+NP), WORKD(3*N)
C
C     %------------%
C     | PARAMETERS |
C     %------------%
C
      COMPLEX*16 ONE, ZERO
      REAL*8     RONE, RZERO
      PARAMETER (ONE = (1.0D+0, 0.0D+0), ZERO = (0.0D+0, 0.0D+0),
     &           RONE = 1.0D+0, RZERO = 0.0D+0)
C
C     %--------------%
C     | LOCAL ARRAYS |
C     %--------------%
C
      REAL*8 RTEMP(2)
C
C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%
C
      LOGICAL    FIRST, ORTH1, ORTH2, RSTART, STEP3, STEP4
      INTEGER*4  INFOL4
      INTEGER    IERR, I, IPJ, IRJ, IVJ, ITER, ITRY, J, MSGLVL,
     &           JJ
      REAL*8      SMLNUM, TST1, ULP, UNFL, BETAJ,
     &           TEMP1, RNORM1, WNORM, RBID
      COMPLEX*16 CNORM
C
      SAVE       FIRST, ORTH1, ORTH2, RSTART, STEP3, STEP4,
     &           IERR, IPJ, IRJ, IVJ, ITER, ITRY, J, MSGLVL,
     &           BETAJ, RNORM1, SMLNUM, ULP, UNFL, WNORM
C
C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%
C
      INTEGER ISBAEM
      COMPLEX*16 ZDOTC
      REAL*8     DZNRM2, ZLANHS, DLAPY2, R8PREM, R8MIEM
C
C     %-----------------%
C     | DATA STATEMENTS |
C     %-----------------%
C
      DATA       FIRST / .TRUE. /
C
C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
      CALL MATFPE(-1)
C
      RBID=0.D0
      IF (FIRST) THEN
C
C        %-----------------------------------------%
C        | SET MACHINE-DEPENDENT CONSTANTS FOR THE |
C        | THE SPLITTING AND DEFLATION CRITERION.  |
C        | IF NORM(H) <= SQRT(OVFL),               |
C        | OVERFLOW SHOULD NOT OCCUR.              |
C        | REFERENCE: LAPACK SUBROUTINE ZLAHQR     |
C        %-----------------------------------------%
C
         UNFL = R8MIEM()
C DUE TO CRS512         OVFL = ONE / UNFL
         ULP = R8PREM() * 0.5D0 * ISBAEM()
         SMLNUM = UNFL*( N / ULP )
         FIRST = .FALSE.
      END IF
C
      IF (IDO .EQ. 0) THEN
C
C        %-------------------------------%
C        | INITIALIZE TIMING STATISTICS  |
C        | & MESSAGE LEVEL FOR DEBUGGING |
C        %-------------------------------%
C
         MSGLVL = MNAITR
C
C        %------------------------------%
C        | INITIAL CALL TO THIS ROUTINE |
C        %------------------------------%
C
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
C
C     %-------------------------------------------------%
C     | WHEN IN REVERSE COMMUNICATION MODE ONE OF:      |
C     | STEP3, STEP4, ORTH1, ORTH2, RSTART              |
C     | WILL BE .TRUE. WHEN ....                        |
C     | STEP3: RETURN FROM COMPUTING OP*V_{J}.          |
C     | STEP4: RETURN FROM COMPUTING B-NORM OF OP*V_{J} |
C     | ORTH1: RETURN FROM COMPUTING B-NORM OF R_{J+1}  |
C     | ORTH2: RETURN FROM COMPUTING B-NORM OF          |
C     |        CORRECTION TO THE RESIDUAL VECTOR.       |
C     | RSTART: RETURN FROM OP COMPUTATIONS NEEDED BY   |
C     |         ZGETV0.                                 |
C     %-------------------------------------------------%
C
      IF (STEP3)  GO TO 50
      IF (STEP4)  GO TO 60
      IF (ORTH1)  GO TO 70
      IF (ORTH2)  GO TO 90
      IF (RSTART) GO TO 30
C
C     %-----------------------------%
C     | ELSE THIS IS THE FIRST STEP |
C     %-----------------------------%
C
C     %--------------------------------------------------------------%
C     |                                                              |
C     |        A R N O L D I     I T E R A T I O N     L O O P       |
C     |                                                              |
C     | NOTE:  B*R_{J-1} IS ALREADY IN WORKD(1:N)=WORKD(IPJ:IPJ+N-1) |
C     %--------------------------------------------------------------%

 1000 CONTINUE
C
         IF (MSGLVL .GT. 1) THEN
            CALL IVOUT (LOGFIL, 1, J, NDIGIT,
     &                  '_NAITR: GENERATING ARNOLDI VECTOR NUMBER')
            CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT,
     &                  '_NAITR: B-NORM OF THE CURRENT RESIDUAL IS')
         END IF
C
C        %---------------------------------------------------%
C        | STEP 1: CHECK IF THE B NORM OF J-TH RESIDUAL      |
C        | VECTOR IS ZERO. EQUIVALENT TO DETERMINE WHETHER   |
C        | AN EXACT J-STEP ARNOLDI FACTORIZATION IS PRESENT. |
C        %---------------------------------------------------%
C
         BETAJ = RNORM
         IF (RNORM .GT. RZERO) GO TO 40
C
C           %---------------------------------------------------%
C           | INVARIANT SUBSPACE FOUND, GENERATE A NEW STARTING |
C           | VECTOR WHICH IS ORTHOGONAL TO THE CURRENT ARNOLDI |
C           | BASIS AND CONTINUE THE ITERATION.                 |
C           %---------------------------------------------------%
C
            IF (MSGLVL .GT. 0) THEN
               CALL IVOUT (LOGFIL, 1, J, NDIGIT,
     &                     '_NAITR: ****** RESTART AT STEP ******')
            END IF
C
C           %---------------------------------------------%
C           | ITRY IS THE LOOP VARIABLE THAT CONTROLS THE |
C           | MAXIMUM AMOUNT OF TIMES THAT A RESTART IS   |
C           | ATTEMPTED. NRSTRT IS USED BY STAT.H         |
C           %---------------------------------------------%
C
            BETAJ  = RZERO
            NRSTRT = NRSTRT + 1
            ITRY   = 1
   20       CONTINUE
            RSTART = .TRUE.
            IDO    = 0
   30       CONTINUE
C
C           %--------------------------------------%
C           | IF IN REVERSE COMMUNICATION MODE AND |
C           | RSTART = .TRUE. FLOW RETURNS HERE.   |
C           %--------------------------------------%
C
            CALL ZGETV0 (IDO, BMAT, .FALSE., N, J, V, LDV,
     &                   RESID, RNORM, IPNTR, WORKD, IERR, ALPHA)
            IF (IDO .NE. 99) GO TO 9000
            IF (IERR .LT. 0) THEN
               ITRY = ITRY + 1
               IF (ITRY .LE. 3) GO TO 20
C
C              %------------------------------------------------%
C              | GIVE UP AFTER SEVERAL RESTART ATTEMPTS.        |
C              | SET INFO TO THE SIZE OF THE INVARIANT SUBSPACE |
C              | WHICH SPANS OP AND EXIT.                       |
C              %------------------------------------------------%
C
               INFO = J - 1
               IDO = 99
               GO TO 9000
            END IF
C
   40    CONTINUE
C
C        %---------------------------------------------------------%
C        | STEP 2:  V_{J} = R_{J-1}/RNORM AND P_{J} = P_{J}/RNORM  |
C        | NOTE THAT P_{J} = B*R_{J-1}. IN ORDER TO AVOID OVERFLOW |
C        | WHEN RECIPROCATING A SMALL RNORM, TEST AGAINST LOWER    |
C        | MACHINE BOUND.                                          |
C        %---------------------------------------------------------%
C
         CALL ZCOPY (N, RESID, 1, V(1,J), 1)
         IF ( RNORM .GE. UNFL) THEN
             TEMP1 = RONE / RNORM
             CALL ZDSCAL (N, TEMP1, V(1,J), 1)
             CALL ZDSCAL (N, TEMP1, WORKD(IPJ), 1)
         ELSE
C
C            %-----------------------------------------%
C            | TO SCALE BOTH V_{J} AND P_{J} CAREFULLY |
C            | USE LAPACK ROUTINE ZLASCL               |
C            %-----------------------------------------%
C
             CALL ZLASCL ('G', I, I, RNORM, RONE,
     &                    N, 1, V(1,J), N, INFOL4)
             CALL ZLASCL ('G', I, I, RNORM, RONE,
     &                    N, 1, WORKD(IPJ), N, INFOL4)
         END IF
C
C        %------------------------------------------------------%
C        | STEP 3:  R_{J} = OP*V_{J}; NOTE THAT P_{J} = B*V_{J} |
C        | NOTE THAT THIS IS NOT QUITE YET R_{J}. SEE STEP 4    |
C        %------------------------------------------------------%
C
         STEP3 = .TRUE.
         NOPX  = NOPX + 1
         CALL ZCOPY (N, V(1,J), 1, WORKD(IVJ), 1)
         IPNTR(1) = IVJ
         IPNTR(2) = IRJ
         IPNTR(3) = IPJ
         IDO = 1
C
C        %-----------------------------------%
C        | EXIT IN ORDER TO COMPUTE OP*V_{J} |
C        %-----------------------------------%
C
         GO TO 9000
   50    CONTINUE
C
C        %----------------------------------%
C        | BACK FROM REVERSE COMMUNICATION; |
C        | WORKD(IRJ:IRJ+N-1) := OP*V_{J}   |
C        | IF STEP3 = .TRUE.                |
C        %----------------------------------%
C
         STEP3 = .FALSE.
C
C        %------------------------------------------%
C        | PUT ANOTHER COPY OF OP*V_{J} INTO RESID. |
C        %------------------------------------------%
C
         CALL ZCOPY (N, WORKD(IRJ), 1, RESID, 1)
C
C        %---------------------------------------%
C        | STEP 4:  FINISH EXTENDING THE ARNOLDI |
C        |          FACTORIZATION TO LENGTH J.   |
C        %---------------------------------------%
C
         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            STEP4 = .TRUE.
            IPNTR(1) = IRJ
            IPNTR(2) = IPJ
            IDO = 2
C
C           %-------------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*OP*V_{J} |
C           %-------------------------------------%
C
            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL ZCOPY (N, RESID, 1, WORKD(IPJ), 1)
         END IF
   60    CONTINUE
C
C        %----------------------------------%
C        | BACK FROM REVERSE COMMUNICATION; |
C        | WORKD(IPJ:IPJ+N-1) := B*OP*V_{J} |
C        | IF STEP4 = .TRUE.                |
C        %----------------------------------%
C
         STEP4 = .FALSE.
C
C        %-------------------------------------%
C        | THE FOLLOWING IS NEEDED FOR STEP 5. |
C        | COMPUTE THE B-NORM OF OP*V_{J}.     |
C        %-------------------------------------%
C
         IF (BMAT .EQ. 'G') THEN
             CNORM = ZDOTC (N, RESID, 1, WORKD(IPJ), 1)
             WNORM = SQRT( DLAPY2(DBLE(CNORM),DIMAG(CNORM)) )
         ELSE IF (BMAT .EQ. 'I') THEN
             WNORM = DZNRM2(N, RESID, 1)
         END IF
C
C        %-----------------------------------------%
C        | COMPUTE THE J-TH RESIDUAL CORRESPONDING |
C        | TO THE J STEP FACTORIZATION.            |
C        | USE CLASSICAL GRAM SCHMIDT AND COMPUTE: |
C        | W_{J} <-  V_{J}^T * B * OP * V_{J}      |
C        | R_{J} <-  OP*V_{J} - V_{J} * W_{J}      |
C        %-----------------------------------------%
C
C
C        %------------------------------------------%
C        | COMPUTE THE J FOURIER COEFFICIENTS W_{J} |
C        | WORKD(IPJ:IPJ+N-1) CONTAINS B*OP*V_{J}.  |
C        %------------------------------------------%
C
         CALL ZGEMV ('C', N, J, ONE, V, LDV, WORKD(IPJ), 1,
     &               ZERO, H(1,J), 1)
C
C        %--------------------------------------%
C        | ORTHOGONALIZE R_{J} AGAINST V_{J}.   |
C        | RESID CONTAINS OP*V_{J}. SEE STEP 3. |
C        %--------------------------------------%
C
         CALL ZGEMV ('N', N, J, -ONE, V, LDV, H(1,J), 1,
     &               ONE, RESID, 1)
C
         IF (J .GT. 1) H(J,J-1) = DCMPLX(BETAJ, RZERO)
C
C
         ORTH1 = .TRUE.
C
         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            CALL ZCOPY (N, RESID, 1, WORKD(IRJ), 1)
            IPNTR(1) = IRJ
            IPNTR(2) = IPJ
            IDO = 2
C
C           %----------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*R_{J} |
C           %----------------------------------%
C
            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL ZCOPY (N, RESID, 1, WORKD(IPJ), 1)
         END IF
   70    CONTINUE
C
C        %---------------------------------------------------%
C        | BACK FROM REVERSE COMMUNICATION IF ORTH1 = .TRUE. |
C        | WORKD(IPJ:IPJ+N-1) := B*R_{J}.                    |
C        %---------------------------------------------------%
C
         ORTH1 = .FALSE.
C
C        %------------------------------%
C        | COMPUTE THE B-NORM OF R_{J}. |
C        %------------------------------%
C
         IF (BMAT .EQ. 'G') THEN
            CNORM = ZDOTC (N, RESID, 1, WORKD(IPJ), 1)
            RNORM = SQRT( DLAPY2(DBLE(CNORM),DIMAG(CNORM)) )
         ELSE IF (BMAT .EQ. 'I') THEN
            RNORM = DZNRM2(N, RESID, 1)
         END IF
C
C        %-----------------------------------------------------------%
C        | STEP 5: RE-ORTHOGONALIZATION / ITERATIVE REFINEMENT PHASE |
C        | MAXIMUM NITER_ITREF TRIES.                                |
C        |                                                           |
C        |          S      = V_{J}^T * B * R_{J}                     |
C        |          R_{J}  = R_{J} - V_{J}*S                         |
C        |          ALPHAJ = ALPHAJ + S_{J}                          |
C        |                                                           |
C        | THE STOPPING CRITERIA USED FOR ITERATIVE REFINEMENT IS    |
C        | DISCUSSED IN PARLETT'S BOOK SEP, PAGE 107 AND IN GRAGG &  |
C        | REICHEL ACM TOMS PAPER; ALGORITHM 686, DEC. 1990.         |
C        | DETERMINE IF WE NEED TO CORRECT THE RESIDUAL. THE GOAL IS |
C        | TO ENFORCE ||V(:,1:J)^T * R_{J}|| .LE. EPS * || R_{J} ||  |
C        | THE FOLLOWING TEST DETERMINES WHETHER THE SINE OF THE     |
C        | ANGLE BETWEEN  OP*X AND THE COMPUTED RESIDUAL IS LESS     |
C        | THAN OR EQUAL TO 0.717.                                   |
C        %-----------------------------------------------------------%
C
         IF ( RNORM .GT. ALPHA*WNORM ) GO TO 100
C
         ITER  = 0
         NRORTH = NRORTH + 1
C
C        %---------------------------------------------------%
C        | ENTER THE ITERATIVE REFINEMENT PHASE. IF FURTHER  |
C        | REFINEMENT IS NECESSARY, LOOP BACK HERE. THE LOOP |
C        | VARIABLE IS ITER. PERFORM A STEP OF CLASSICAL     |
C        | GRAM-SCHMIDT USING ALL THE ARNOLDI VECTORS V_{J}  |
C        %---------------------------------------------------%
C
   80    CONTINUE
C
         IF (MSGLVL .GT. 2) THEN
            RTEMP(1) = WNORM
            RTEMP(2) = RNORM
            CALL DVOUT (LOGFIL, 2, RTEMP, NDIGIT,
     &    '_NAITR: RE-ORTHOGONALIZATION; WNORM AND RNORM ARE')
            CALL ZVOUT (LOGFIL, J, H(1,J), NDIGIT,
     &                  '_NAITR: J-TH COLUMN OF H')
         END IF
C
C        %----------------------------------------------------%
C        | COMPUTE V_{J}^T * B * R_{J}.                       |
C        | WORKD(IRJ:IRJ+J-1) = V(:,1:J)'*WORKD(IPJ:IPJ+N-1). |
C        %----------------------------------------------------%
C
         CALL ZGEMV ('C', N, J, ONE, V, LDV, WORKD(IPJ), 1,
     &               ZERO, WORKD(IRJ), 1)
C
C        %---------------------------------------------%
C        | COMPUTE THE CORRECTION TO THE RESIDUAL:     |
C        | R_{J} = R_{J} - V_{J} * WORKD(IRJ:IRJ+J-1). |
C        | THE CORRECTION TO H IS V(:,1:J)*H(1:J,1:J)  |
C        | + V(:,1:J)*WORKD(IRJ:IRJ+J-1)*E'_J.         |
C        %---------------------------------------------%
C
         CALL ZGEMV ('N', N, J, -ONE, V, LDV, WORKD(IRJ), 1,
     &               ONE, RESID, 1)
         CALL ZAXPY (J, ONE, WORKD(IRJ), 1, H(1,J), 1)
C
         ORTH2 = .TRUE.
         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            CALL ZCOPY (N, RESID, 1, WORKD(IRJ), 1)
            IPNTR(1) = IRJ
            IPNTR(2) = IPJ
            IDO = 2
C
C           %-----------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*R_{J}. |
C           | R_{J} IS THE CORRECTED RESIDUAL.  |
C           %-----------------------------------%
C
            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL ZCOPY (N, RESID, 1, WORKD(IPJ), 1)
         END IF
   90    CONTINUE
C
C        %---------------------------------------------------%
C        | BACK FROM REVERSE COMMUNICATION IF ORTH2 = .TRUE. |
C        %---------------------------------------------------%
C
C        %-----------------------------------------------------%
C        | COMPUTE THE B-NORM OF THE CORRECTED RESIDUAL R_{J}. |
C        %-----------------------------------------------------%
C
         IF (BMAT .EQ. 'G') THEN
             CNORM  = ZDOTC (N, RESID, 1, WORKD(IPJ), 1)
             RNORM1 = SQRT( DLAPY2(DBLE(CNORM),DIMAG(CNORM)) )
         ELSE IF (BMAT .EQ. 'I') THEN
             RNORM1 = DZNRM2(N, RESID, 1)
         END IF
C
         IF (MSGLVL .GT. 0 .AND. ITER .GT. 0 ) THEN
            CALL IVOUT (LOGFIL, 1, J, NDIGIT,
     &           '_NAITR: ITERATIVE REFINEMENT FOR ARNOLDI RESIDUAL')
            IF (MSGLVL .GT. 2) THEN
                RTEMP(1) = RNORM
                RTEMP(2) = RNORM1
                CALL DVOUT (LOGFIL, 2, RTEMP, NDIGIT,
     &           '_NAITR: ITERATIVE REFINEMENT ; RNORM AND RNORM1 ARE')
            END IF
         END IF
C
C        %-----------------------------------------%
C        | DETERMINE IF WE NEED TO PERFORM ANOTHER |
C        | STEP OF RE-ORTHOGONALIZATION.           |
C        %-----------------------------------------%
C
         IF ( RNORM1 .GT. ALPHA*RNORM ) THEN
C
C           %---------------------------------------%
C           | NO NEED FOR FURTHER REFINEMENT.       |
C           | THE COSINE OF THE ANGLE BETWEEN THE   |
C           | CORRECTED RESIDUAL VECTOR AND THE OLD |
C           | RESIDUAL VECTOR IS GREATER THAN 0.717 |
C           | IN OTHER WORDS THE CORRECTED RESIDUAL |
C           | AND THE OLD RESIDUAL VECTOR SHARE AN  |
C           | ANGLE OF LESS THAN ARCCOS(0.717)      |
C           %---------------------------------------%
C
            RNORM = RNORM1
C
         ELSE
C
C           %-------------------------------------------%
C           | ANOTHER STEP OF ITERATIVE REFINEMENT STEP |
C           | IS REQUIRED. NITREF IS USED BY STAT.H     |
C           %-------------------------------------------%
C
            NITREF = NITREF + 1
            RNORM  = RNORM1
            ITER   = ITER + 1
            IF (ITER .LE. 1) GO TO 80
C
C           %-------------------------------------------------%
C           | OTHERWISE RESID IS NUMERICALLY IN THE SPAN OF V |
C           %-------------------------------------------------%
C
            DO 95 JJ = 1, N
               RESID(JJ) = ZERO
  95        CONTINUE
            RNORM = RZERO
         END IF
C
C        %----------------------------------------------%
C        | BRANCH HERE DIRECTLY IF ITERATIVE REFINEMENT |
C        | WASN'T NECESSARY OR AFTER AT MOST NITER_REF  |
C        | STEPS OF ITERATIVE REFINEMENT.               |
C        %----------------------------------------------%
C
  100    CONTINUE
C
         RSTART = .FALSE.
         ORTH2  = .FALSE.
C
C        %------------------------------------%
C        | STEP 6: UPDATE  J = J+1;  CONTINUE |
C        %------------------------------------%
C
         J = J + 1
         IF (J .GT. K+NP) THEN
            IDO = 99
            DO 110 I = MAX(1,K), K+NP-1
C
C              %--------------------------------------------%
C              | CHECK FOR SPLITTING AND DEFLATION.         |
C              | USE A STANDARD TEST AS IN THE QR ALGORITHM |
C              | REFERENCE: LAPACK SUBROUTINE ZLAHQR        |
C              %--------------------------------------------%
C
               TST1 = DLAPY2(DBLE(H(I,I)),DIMAG(H(I,I)))
     &              + DLAPY2(DBLE(H(I+1,I+1)), DIMAG(H(I+1,I+1)))
               IF( TST1.EQ.DBLE(ZERO) )
     &              TST1 = ZLANHS( '1', K+NP, H, LDH, RBID )
               IF( DLAPY2(DBLE(H(I+1,I)),DIMAG(H(I+1,I))) .LE.
     &                    MAX( ULP*TST1, SMLNUM ) )
     &             H(I+1,I) = ZERO
 110        CONTINUE
C
            IF (MSGLVL .GT. 2) THEN
               CALL ZMOUT (LOGFIL, K+NP, K+NP, H, LDH, NDIGIT,
     &          '_NAITR: FINAL UPPER HESSENBERG MATRIX H OF ORDER K+NP')
            END IF
C
            GO TO 9000
         END IF
C
C        %--------------------------------------------------------%
C        | LOOP BACK TO EXTEND THE FACTORIZATION BY ANOTHER STEP. |
C        %--------------------------------------------------------%
C
      GO TO 1000
C
C     %---------------------------------------------------------------%
C     |                                                               |
C     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
C     |                                                               |
C     %---------------------------------------------------------------%
C
 9000 CONTINUE
      CALL MATFPE(1)
C
C     %---------------%
C     | END OF ZNAITR |
C     %---------------%
C
      END
