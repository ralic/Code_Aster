      SUBROUTINE ZGETV0 
     &   ( IDO, BMAT, INITV, N, J, V, LDV, RESID, RNORM, 
     &     IPNTR, WORKD, IERR, ALPHA )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C
C     SUBROUTINE ARPACK GENERANT UN VECTEUR INITIAL DANS IM(OP).
C-----------------------------------------------------------------------
C\BEGINDOC
C
C\NAME: ZGETV0
C
C\DESCRIPTION: 
C  GENERATE A RANDOM INITIAL RESIDUAL VECTOR FOR THE ARNOLDI PROCESS.
C  FORCE THE RESIDUAL VECTOR TO BE IN THE RANGE OF THE OPERATOR OP.  
C
C\USAGE:
C  CALL ZGETV0
C     ( IDO, BMAT, INITV, N, J, V, LDV, RESID, RNORM, 
C       IPNTR, WORKD, IERR )
C
C\ARGUMENTS
C  IDO     INTEGER.  (INPUT/OUTPUT)
C          REVERSE COMMUNICATION FLAG.  IDO MUST BE ZERO ON THE FIRST
C          CALL TO ZGETV0.
C          -------------------------------------------------------------
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
C         -------------------------------------------------------------
C
C  BMAT    CHARACTER*1.  (INPUT)
C          BMAT SPECIFIES THE TYPE OF THE MATRIX B IN THE (GENERALIZED)
C          EIGENVALUE PROBLEM A*X = LAMBDA*B*X.
C          B = 'I' -> STANDARD EIGENVALUE PROBLEM A*X = LAMBDA*X
C          B = 'G' -> GENERALIZED EIGENVALUE PROBLEM A*X = LAMBDA*B*X
C
C
C  INITV   LOGICAL VARIABLE.  (INPUT)
C          .TRUE.  => THE INITIAL RESIDUAL VECTOR IS GIVEN IN RESID.
C          .FALSE. => GENERATE A RANDOM INITIAL RESIDUAL VECTOR.
C
C  N       INTEGER.  (INPUT)
C          DIMENSION OF THE PROBLEM.
C
C  J       INTEGER.  (INPUT)
C          INDEX OF THE RESIDUAL VECTOR TO BE GENERATED, WITH RESPECT 
C          TO THE ARNOLDI PROCESS.  J > 1 IN CASE OF A "RESTART".
C
C  V       COMPLEX*16 N BY J ARRAY.  (INPUT)
C          THE FIRST J-1 COLUMNS OF V CONTAIN THE CURRENT ARNOLDI BASIS
C          IF THIS IS A "RESTART".
C
C  LDV     INTEGER.  (INPUT)
C          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING 
C          PROGRAM.
C
C  RESID   COMPLEX*16 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C          INITIAL RESIDUAL VECTOR TO BE GENERATED.  IF RESID IS 
C          PROVIDED, FORCE RESID INTO THE RANGE OF THE OPERATOR OP.
C
C  RNORM   DOUBLE PRECISION SCALAR.  (OUTPUT)
C          B-NORM OF THE GENERATED RESIDUAL.
C
C  IPNTR   INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
C
C  WORKD   COMPLEX*16 WORK ARRAY OF LENGTH 2*N.(REVERSE COMMUNICATION).
C          ON EXIT, WORK(1:N) = B*RESID TO BE USED IN SSAITR.
C
C  IERR    INTEGER.  (OUTPUT)
C          =  0: NORMAL EXIT.
C          = -1: CANNOT GENERATE A NONTRIVIAL RESTARTED RESIDUAL VECTOR
C                IN THE RANGE OF THE OPERATOR OP.
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
C
C\ROUTINES CALLED:
C   ZVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C   GLARNV  LAPACK ROUTINE FOR GENERATING A RANDOM VECTOR. 
C   ZGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
C   ZCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER.
C   ZDOTC   LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT OF TWO 
C            VECTORS.
C   DZNRM2  LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR. 
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
C FILE: GETV0.F   SID: 2.3   DATE OF SID: 08/27/96   RELEASE: 2
C
C\ENDLIB
C
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C 
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
C
C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%
C
      CHARACTER*1  BMAT
      LOGICAL    INITV
      INTEGER    IDO, IERR, J, LDV, N
      REAL*8 RNORM, ALPHA
C
C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%
C
      INTEGER    IPNTR(3)
      COMPLEX*16 RESID(N), V(LDV,J), WORKD(2*N)
C
C     %------------%
C     | PARAMETERS |
C     %------------%
C
      COMPLEX*16 ONE, ZERO
      REAL*8     RZERO
      PARAMETER  (ONE = (1.0D+0, 0.0D+0), ZERO = (0.0D+0, 0.0D+0),
     &            RZERO = 0.0D+0)
C
C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%
C
      LOGICAL    FIRST, INITS, ORTH
      INTEGER    IDIST, ISEED(4), ITER, MSGLVL, JJ
      REAL*8     RNORM0
      COMPLEX*16 CNORM
      SAVE       FIRST, ISEED, INITS, ITER, MSGLVL, ORTH, RNORM0
C
C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%
C
      REAL*8  DZNRM2, FLAPY2
      COMPLEX*16 ZDOTC
C
C     %-----------------%
C     | DATA STATEMENTS |
C     %-----------------%
C
      DATA       INITS /.TRUE./
C
C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
C
C     %-----------------------------------%
C     | INITIALIZE THE SEED OF THE LAPACK |
C     | RANDOM NUMBER GENERATOR           |
C     %-----------------------------------%
C
      IF (INITS) THEN
          ISEED(1) = 1
          ISEED(2) = 3
          ISEED(3) = 5
          ISEED(4) = 7
          INITS = .FALSE.
      END IF
C
      IF (IDO .EQ.  0) THEN
C 
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
C
C        %-----------------------------------------------------%
C        | POSSIBLY GENERATE A RANDOM STARTING VECTOR IN RESID |
C        | USE A LAPACK RANDOM NUMBER GENERATOR USED BY THE    |
C        | MATRIX GENERATION ROUTINES.                         |
C        |    IDIST = 1: UNIFORM (0,1)  DISTRIBUTION;          |
C        |    IDIST = 2: UNIFORM (-1,1) DISTRIBUTION;          |
C        |    IDIST = 3: NORMAL  (0,1)  DISTRIBUTION;          |
C        %-----------------------------------------------------%
C
         IF (.NOT.INITV) THEN
            IDIST = 2
            CALL GLARNV (IDIST, ISEED, N, RESID)
         END IF
C 
C        %----------------------------------------------------------%
C        | FORCE THE STARTING VECTOR INTO THE RANGE OF OP TO HANDLE |
C        | THE GENERALIZED PROBLEM WHEN B IS POSSIBLY (SINGULAR).   |
C        %----------------------------------------------------------%
C
         IF (BMAT .EQ. 'G') THEN
            NOPX = NOPX + 1
            IPNTR(1) = 1
            IPNTR(2) = N + 1
            CALL ZCOPY (N, RESID, 1, WORKD, 1)
            IDO = -1
            GO TO 9000
         END IF
      END IF
C 
C     %----------------------------------------%
C     | BACK FROM COMPUTING B*(INITIAL-VECTOR) |
C     %----------------------------------------%
C
      IF (FIRST) GO TO 20
C
C     %-----------------------------------------------%
C     | BACK FROM COMPUTING B*(ORTHOGONALIZED-VECTOR) |
C     %-----------------------------------------------%
C
      IF (ORTH)  GO TO 40
C 
C     %------------------------------------------------------%
C     | STARTING VECTOR IS NOW IN THE RANGE OF OP; R = OP*R; |
C     | COMPUTE B-NORM OF STARTING VECTOR.                   |
C     %------------------------------------------------------%
C
      FIRST = .TRUE.
      IF (BMAT .EQ. 'G') THEN
         NBX = NBX + 1
         CALL ZCOPY (N, WORKD(N+1), 1, RESID, 1)
         IPNTR(1) = N + 1
         IPNTR(2) = 1
         IDO = 2
         GO TO 9000
      ELSE IF (BMAT .EQ. 'I') THEN
         CALL ZCOPY (N, RESID, 1, WORKD, 1)
      END IF
C 
   20 CONTINUE
C 
      FIRST = .FALSE.
      IF (BMAT .EQ. 'G') THEN
          CNORM  = ZDOTC (N, RESID, 1, WORKD, 1)
          RNORM0 = SQRT(FLAPY2(DBLE(CNORM),DIMAG(CNORM)))
      ELSE IF (BMAT .EQ. 'I') THEN
           RNORM0 = DZNRM2(N, RESID, 1)
      END IF
      RNORM  = RNORM0
C
C     %---------------------------------------------%
C     | EXIT IF THIS IS THE VERY FIRST ARNOLDI STEP |
C     %---------------------------------------------%
C
      IF (J .EQ. 1) GO TO 50
C 
C    %----------------------------------------------------------------
C    | OTHERWISE NEED TO B-ORTHOGONALIZE THE STARTING VECTOR AGAINST |
C    | THE CURRENT ARNOLDI BASIS USING GRAM-SCHMIDT WITH ITER. REF.  |
C    | THIS IS THE CASE WHERE AN INVARIANT SUBSPACE IS ENCOUNTERED   |
C    | IN THE MIDDLE OF THE ARNOLDI FACTORIZATION.                   |
C    |                                                               |
C    |       S = V^{T}*B*R;   R = R - V*S;                           |
C    |                                                               |
C    | STOPPING CRITERIA USED FOR ITER. REF. IS DISCUSSED IN         |
C    | PARLETT'S BOOK, PAGE 107 AND IN GRAGG & REICHEL TOMS PAPER.   |
C    %---------------------------------------------------------------%
C
      ORTH = .TRUE.
   30 CONTINUE
C
      CALL ZGEMV ('C', N, J-1, ONE, V, LDV, WORKD, 1, 
     &            ZERO, WORKD(N+1), 1)
      CALL ZGEMV ('N', N, J-1, -ONE, V, LDV, WORKD(N+1), 1, 
     &            ONE, RESID, 1)
C 
C     %----------------------------------------------------------%
C     | COMPUTE THE B-NORM OF THE ORTHOGONALIZED STARTING VECTOR |
C     %----------------------------------------------------------%
C
      IF (BMAT .EQ. 'G') THEN
         NBX = NBX + 1
         CALL ZCOPY (N, RESID, 1, WORKD(N+1), 1)
         IPNTR(1) = N + 1
         IPNTR(2) = 1
         IDO = 2
         GO TO 9000
      ELSE IF (BMAT .EQ. 'I') THEN
         CALL ZCOPY (N, RESID, 1, WORKD, 1)
      END IF
C 
   40 CONTINUE
C 
      IF (BMAT .EQ. 'G') THEN
         CNORM = ZDOTC (N, RESID, 1, WORKD, 1)
         RNORM = SQRT(FLAPY2(DBLE(CNORM),DIMAG(CNORM)))
      ELSE IF (BMAT .EQ. 'I') THEN
         RNORM = DZNRM2(N, RESID, 1)
      END IF
C
C     %--------------------------------------%
C     | CHECK FOR FURTHER ORTHOGONALIZATION. |
C     %--------------------------------------%
C
      IF (MSGLVL .GT. 2) THEN
          CALL DVOUT (LOGFIL, 1, RNORM0, NDIGIT, 
     &                '_GETV0: RE-ORTHONALIZATION ; RNORM0 IS')
          CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT, 
     &                '_GETV0: RE-ORTHONALIZATION ; RNORM IS')
      END IF
C
      IF (RNORM .GT. ALPHA*RNORM0) GO TO 50
C 
      ITER = ITER + 1
      IF (ITER .LE. 1) THEN
C
C        %-----------------------------------%
C        | PERFORM ITERATIVE REFINEMENT STEP |
C        %-----------------------------------%
C
         RNORM0 = RNORM
         GO TO 30
      ELSE
C
C        %------------------------------------%
C        | ITERATIVE REFINEMENT STEP "FAILED" |
C        %------------------------------------%
C
         DO 45 JJ = 1, N
            RESID(JJ) = ZERO
   45    CONTINUE
         RNORM = RZERO
         IERR = -1
      END IF
C 
   50 CONTINUE
C
      IF (MSGLVL .GT. 0) THEN
         CALL DVOUT (LOGFIL, 1, RNORM, NDIGIT,
     &       '_GETV0: B-NORM OF INITIAL / RESTARTED STARTING VECTOR')
      END IF
      IF (MSGLVL .GT. 2) THEN
         CALL ZVOUT (LOGFIL, N, RESID, NDIGIT,
     &       '_GETV0: INITIAL / RESTARTED STARTING VECTOR')
      END IF
      IDO = 99
C 
 9000 CONTINUE
C
C     %---------------%
C     | END OF ZGETV0 |
C     %---------------%
C
      END
