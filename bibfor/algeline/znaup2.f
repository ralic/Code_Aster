      SUBROUTINE ZNAUP2 
     &   ( IDO, BMAT, N, WHICH, NEV, NP, TOL, RESID, 
     &     ISHIFT, MXITER, V, LDV, H, LDH, RITZ, BOUNDS, 
     &     Q, LDQ, WORKL, IPNTR, WORKD, RWORK, INFO, NEQACT, ALPHA)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 31/01/2005   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRP_20
C TOLE CRP_21
C
C     SUBROUTINE ARPACK CALCULANT LES VALEURS PROPRES DE (OP) VIA
C     IRAM.
C---------------------------------------------------------------------
C\BEGINDOC
C
C\NAME: ZNAUP2 
C
C\DESCRIPTION: 
C  INTERMEDIATE LEVEL INTERFACE CALLED BY ZNAUPD .
C
C\USAGE:
C  CALL ZNAUP2 
C     ( IDO, BMAT, N, WHICH, NEV, NP, TOL, RESID,
C       ISHIFT, MXITER, V, LDV, H, LDH, RITZ, BOUNDS, 
C       Q, LDQ, WORKL, IPNTR, WORKD, RWORK, INFO )
C
C\ARGUMENTS
C
C  IDO, BMAT, N, WHICH, NEV, TOL, RESID: SAME AS DEFINED IN ZNAUPD .
C  ISHIFT, MXITER: SEE THE DEFINITION OF IPARAM IN ZNAUPD .
C
C  NP      INTEGER.  (INPUT/OUTPUT)
C          CONTAINS THE NUMBER OF IMPLICIT SHIFTS TO APPLY DURING
C          EACH ARNOLDI ITERATION.
C          IF ISHIFT=1, NP IS ADJUSTED DYNAMICALLY AT EACH ITERATION
C          TO ACCELERATE CONVERGENCE AND PREVENT STAGNATION.
C          THIS IS ALSO ROUGHLY EQUAL TO THE NUMBER OF MATRIX-VECTOR
C          PRODUCTS (INVOLVING THE OPERATOR OP) PER ARNOLDI ITERATION.
C          THE LOGIC FOR ADJUSTING IS CONTAINED WITHIN THE CURRENT
C          SUBROUTINE.
C          IF ISHIFT=0, NP IS THE NUMBER OF SHIFTS THE USER NEEDS
C          TO PROVIDE VIA REVERSE COMUNICATION. 0 < NP < NCV-NEV.
C          NP MAY BE LESS THAN NCV-NEV SINCE A LEADING BLOCK OF THE 
C          CURRENT
C          UPPER HESSENBERG MATRIX HAS SPLIT OFF AND CONTAINS "UNWANTED"
C          RITZ VALUES.
C          UPON TERMINATION OF THE IRA ITERATION, NP CONTAINS THE NUMBER
C          OF "CONVERGED" WANTED RITZ VALUES.
C
C  V       COMPLEX*16  N BY (NEV+NP) ARRAY.  (INPUT/OUTPUT)
C          THE ARNOLDI BASIS VECTORS ARE RETURNED IN THE FIRST NEV 
C          COLUMNS OF V.
C
C  LDV     INTEGER.  (INPUT)
C          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING 
C          PROGRAM.
C
C  H       COMPLEX*16  (NEV+NP) BY (NEV+NP) ARRAY.  (OUTPUT)
C          H IS USED TO STORE THE GENERATED UPPER HESSENBERG MATRIX
C
C  LDH     INTEGER.  (INPUT)
C          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING 
C          PROGRAM.
C
C  RITZ    COMPLEX*16  ARRAY OF LENGTH NEV+NP.  (OUTPUT)
C          RITZ(1:NEV)  CONTAINS THE COMPUTED RITZ VALUES OF OP.
C
C  BOUNDS  COMPLEX*16  ARRAY OF LENGTH NEV+NP.  (OUTPUT)
C          BOUNDS(1:NEV) CONTAIN THE ERROR BOUNDS CORRESPONDING TO 
C          THE COMPUTED RITZ VALUES.
C          
C  Q       COMPLEX*16  (NEV+NP) BY (NEV+NP) ARRAY.  (WORKSPACE)
C          PRIVATE (REPLICATED) WORK ARRAY USED TO ACCUMULATE THE
C          ROTATION IN THE SHIFT APPLICATION STEP.
C
C  LDQ     INTEGER.  (INPUT)
C          LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  WORKL   COMPLEX*16  WORK ARRAY OF LENGTH AT LEAST 
C          (NEV+NP)**2 + 3*(NEV+NP).  (WORKSPACE)
C          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
C          THE FRONT END.  IT IS USED IN SHIFTS CALCULATION, SHIFTS
C          APPLICATION AND CONVERGENCE CHECKING.
C
C
C  IPNTR   INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
C          POINTER TO MARK THE STARTING LOCATIONS IN THE WORKD FOR 
C          VECTORS USED BY THE ARNOLDI ITERATION.
C          -------------------------------------------------------------
C          IPNTR(1): POINTER TO THE CURRENT OPERAND VECTOR X.
C          IPNTR(2): POINTER TO THE CURRENT RESULT VECTOR Y.
C          IPNTR(3): POINTER TO THE VECTOR B * X WHEN USED IN THE 
C                    SHIFT-AND-INVERT MODE.  X IS THE CURRENT OPERAND.
C          -------------------------------------------------------------
C          
C  WORKD   COMPLEX*16  WORK ARRAY OF LENGTH 3*N.  (WORKSPACE)
C          DISTRIBUTED ARRAY TO BE USED IN THE BASIC ARNOLDI ITERATION
C          FOR REVERSE COMMUNICATION.  THE USER SHOULD NOT USE WORKD
C          AS TEMPORARY WORKSPACE DURING THE ITERATION !!!!!!!!!!
C          SEE DATA DISTRIBUTION NOTE IN ZNAUPD .
C
C  RWORK   DOUBLE PRECISION    WORK ARRAY OF LENGTH  NEV+NP ( WORKSPACE)
C          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
C          THE FRONT END.
C
C  INFO    INTEGER.  (INPUT/OUTPUT)
C          IF INFO .EQ. 0, A RANDOMLY INITIAL RESIDUAL VECTOR IS USED.
C          IF INFO .NE. 0, RESID CONTAINS THE INITIAL RESIDUAL VECTOR,
C                          POSSIBLY FROM A PREVIOUS RUN.
C          ERROR FLAG ON OUTPUT.
C          =     0: NORMAL RETURN.
C          =     1: MAXIMUM NUMBER OF ITERATIONS TAKEN.
C                   ALL POSSIBLE EIGENVALUES OF OP HAS BEEN FOUND.  
C                   NP RETURNS THE NUMBER OF CONVERGED RITZ VALUES.
C          =     2: NO SHIFTS COULD BE APPLIED.
C          =    -8: ERROR RETURN FROM LAPACK EIGENVALUE CALCULATION;
C                   THIS SHOULD NEVER HAPPEN.
C          =    -9: STARTING VECTOR IS ZERO.
C          = -9999: COULD NOT BUILD AN ARNOLDI FACTORIZATION.
C                   SIZE THAT WAS BUILT IN RETURNED IN NP.
C
C\ENDDOC
C
C-----------------------------------------------------------------------
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
C     ZGETV0   ARPACK INITIAL VECTOR GENERATION ROUTINE. 
C     ZNAITR   ARPACK ARNOLDI FACTORIZATION ROUTINE.
C     ZNAPPS   ARPACK APPLICATION OF IMPLICIT SHIFTS ROUTINE.
C     ZNEIGH   ARPACK COMPUTE RITZ VALUES AND ERROR BOUNDS ROUTINE. 
C     ZNGETS   ARPACK REORDER RITZ VALUES AND ERROR BOUNDS ROUTINE.
C     ZSORTC   ARPACK SORTING ROUTINE.
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     ZMOUT    ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     ZVOUT    ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     DVOUT    ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     DLAPY2   LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C     ZCOPY    LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     ZDOTC    LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT OF TWO 
C               VECTORS. 
C     GLSWAP    LEVEL 1 BLAS THAT SWAPS TWO VECTORS.
C     DZNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
C
C\AUTHOR
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITYA
C     CHAO YANG                    HOUSTON, TEXAS
C     DEPT. OF COMPUTATIONAL &
C     APPLIED MATHEMATICS 
C     RICE UNIVERSITY           
C     HOUSTON, TEXAS 
C 
C\SCCS INFORMATION: @(#)
C FILE: NAUP2.F   SID: 2.6   DATE OF SID: 06/01/00   RELEASE: 2
C
C\REMARKS
C     1. NONE
C
C\ENDLIB
C
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     %----------------------------------------------------%
C     | INCLUDE FILES FOR DEBUGGING AND TIMING INFORMATION |
C     %----------------------------------------------------%

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
      CHARACTER*2 WHICH
      INTEGER IDO, INFO, ISHIFT, LDH, LDQ, LDV, MXITER,
     &  N, NEV, NP, NEQACT
      REAL*8 TOL, ALPHA
C
C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%
C
      INTEGER    IPNTR(13)
      COMPLEX*16 BOUNDS(NEV+NP), H(LDH,NEV+NP), Q(LDQ,NEV+NP), 
     &           RESID(N), RITZ(NEV+NP),  V(LDV,NEV+NP), 
     &           WORKD(3*N), WORKL( (NEV+NP)*(NEV+NP+3) )
      REAL*8 RWORK(NEV+NP)
C
C     %------------%
C     | PARAMETERS |
C     %------------%
C
      COMPLEX*16 ZERO
      REAL*8     RZERO
      PARAMETER (ZERO = (0.0D+0, 0.0D+0) ,RZERO = 0.0D+0 )
C
C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%
C
      LOGICAL    CNORM , GETV0, INITV , UPDATE, USHIFT
      INTEGER    IERR  , ITER , KPLUSP, MSGLVL, NCONV, 
     &           NEVBEF, NEV0 , NP0   , NPTEMP, I    ,
     &           J    
      COMPLEX*16 CPNORM
      REAL*8     RNORM , EPS23, RTEMP
      CHARACTER*2 WPRIME
C
      SAVE       CNORM,  GETV0, INITV , UPDATE, USHIFT, 
     &           RNORM,  ITER , KPLUSP, MSGLVL, NCONV ,
     &           NEVBEF, NEV0 , NP0   , EPS23
C
C
C     %-----------------------%
C     | LOCAL ARRAY ARGUMENTS |
C     %-----------------------%
C
      INTEGER    KP(3)
C
C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%
C
      COMPLEX*16  ZDOTC 
      REAL*8      DZNRM2 , DLAPY2 , R8PREM
C
C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
      IF (IDO .EQ. 0) THEN
C 
         MSGLVL = MNAUP2
C        %-------------------------------------%
C        | GET THE MACHINE DEPENDENT CONSTANT. |
C        %-------------------------------------%

         EPS23 = R8PREM()*0.5D0
         EPS23 = EPS23**(2.0D+0 / 3.0D+0)
C 
         NEV0   = NEV
         NP0    = NP
C
C        %-------------------------------------%
C        | KPLUSP IS THE BOUND ON THE LARGEST  |
C        |        LANCZOS FACTORIZATION BUILT. |
C        | NCONV IS THE CURRENT NUMBER OF      |
C        |        "CONVERGED" EIGENVALUES.     |
C        | ITER IS THE COUNTER ON THE CURRENT  |
C        |      ITERATION STEP.                |
C        %-------------------------------------%
C
         KPLUSP = NEV + NP
         NCONV  = 0
         ITER   = 0
C 
C        %---------------------------------%
C        | GET MACHINE DEPENDENT CONSTANT. |
C        %---------------------------------%
C
         EPS23 = R8PREM()*0.5D0
         EPS23 = EPS23**(2.0D+0 / 3.0D+0)
C
C        %---------------------------------------%
C        | SET FLAGS FOR COMPUTING THE FIRST NEV |
C        | STEPS OF THE ARNOLDI FACTORIZATION.   |
C        %---------------------------------------%
C
         GETV0    = .TRUE.
         UPDATE   = .FALSE.
         USHIFT   = .FALSE.
         CNORM    = .FALSE.
C
         IF (INFO .NE. 0) THEN
C
C           %--------------------------------------------%
C           | USER PROVIDES THE INITIAL RESIDUAL VECTOR. |
C           %--------------------------------------------%
C
            INITV = .TRUE.
            INFO  = 0
         ELSE
            INITV = .FALSE.
         END IF
      END IF
C 
C     %---------------------------------------------%
C     | GET A POSSIBLY RANDOM STARTING VECTOR AND   |
C     | FORCE IT INTO THE RANGE OF THE OPERATOR OP. |
C     %---------------------------------------------%
C
   10 CONTINUE
C
      IF (GETV0) THEN
         CALL ZGETV0  (IDO, BMAT, INITV, N, 1, V, LDV, RESID, RNORM,
     &                IPNTR, WORKD, INFO, ALPHA)
C
         IF (IDO .NE. 99) GO TO 9000
C
         IF (RNORM .EQ. RZERO) THEN
C
C           %-----------------------------------------%
C           | THE INITIAL VECTOR IS ZERO. ERROR EXIT. | 
C           %-----------------------------------------%
C
            INFO = -9
            GO TO 1100
         END IF
         GETV0 = .FALSE.
         IDO  = 0
      END IF
C 
C     %-----------------------------------%
C     | BACK FROM REVERSE COMMUNICATION : |
C     | CONTINUE WITH UPDATE STEP         |
C     %-----------------------------------%
C
      IF (UPDATE) GO TO 20
C
C     %-------------------------------------------%
C     | BACK FROM COMPUTING USER SPECIFIED SHIFTS |
C     %-------------------------------------------%
C
      IF (USHIFT) GO TO 50
C
C     %-------------------------------------%
C     | BACK FROM COMPUTING RESIDUAL NORM   |
C     | AT THE END OF THE CURRENT ITERATION |
C     %-------------------------------------%
C
      IF (CNORM)  GO TO 100
C 
C     %----------------------------------------------------------%
C     | COMPUTE THE FIRST NEV STEPS OF THE ARNOLDI FACTORIZATION |
C     %----------------------------------------------------------%
C
      CALL ZNAITR  (IDO, BMAT, N, 0, NEV, RESID, RNORM, V, LDV, 
     &             H, LDH, IPNTR, WORKD, INFO, ALPHA)
C
      IF (IDO .NE. 99) GO TO 9000
C
      IF (INFO .GT. 0) THEN
         NP   = INFO
         MXITER = ITER
         INFO = -9999
         GO TO 1200
      END IF
C 
C     %--------------------------------------------------------------%
C     |                                                              |
C     |           M A I N  ARNOLDI  I T E R A T I O N  L O O P       |
C     |           EACH ITERATION IMPLICITLY RESTARTS THE ARNOLDI     |
C     |           FACTORIZATION IN PLACE.                            |
C     |                                                              |
C     %--------------------------------------------------------------%
C 
 1000 CONTINUE
C
         ITER = ITER + 1
C
         IF (MSGLVL .GT. 0) THEN
            CALL IVOUT (LOGFIL, 1, ITER, NDIGIT, 
     &           '_NAUP2: **** START OF MAJOR ITERATION NUMBER ****')
         END IF
C 
C        %-----------------------------------------------------------%
C        | COMPUTE NP ADDITIONAL STEPS OF THE ARNOLDI FACTORIZATION. |
C        | ADJUST NP SINCE NEV MIGHT HAVE BEEN UPDATED BY LAST CALL  |
C        | TO THE SHIFT APPLICATION ROUTINE ZNAPPS .                  |
C        %-----------------------------------------------------------%
C
         NP  = KPLUSP - NEV
C
         IF (MSGLVL .GT. 1) THEN
            CALL IVOUT (LOGFIL, 1, NEV, NDIGIT, 
     &     '_NAUP2: THE LENGTH OF THE CURRENT ARNOLDI FACTORIZATION')
            CALL IVOUT (LOGFIL, 1, NP, NDIGIT, 
     &           '_NAUP2: EXTEND THE ARNOLDI FACTORIZATION BY')
         END IF
C
C        %-----------------------------------------------------------%
C        | COMPUTE NP ADDITIONAL STEPS OF THE ARNOLDI FACTORIZATION. |
C        %-----------------------------------------------------------%
C
         IDO = 0
   20    CONTINUE
         UPDATE = .TRUE.
C
         CALL ZNAITR (IDO, BMAT, N, NEV, NP,  RESID, RNORM,
     &               V  , LDV , H, LDH, IPNTR, WORKD, INFO, ALPHA)
C
         IF (IDO .NE. 99) GO TO 9000
C
         IF (INFO .GT. 0) THEN
            NP = INFO
            MXITER = ITER
            IF (INFO.GE.NEQACT) THEN
             IF (MSGLVL.GT.0) THEN
               WRITE(LOGFIL,*)
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)'& ESPACE INVARIANT DE TAILLE &'
               WRITE(LOGFIL,*)'& NEQACT = ',NEQACT
               WRITE(LOGFIL,*)'& SHUNTAGE PARTIEL DE ZNAUP2 &'
               WRITE(LOGFIL,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
               WRITE(LOGFIL,*)
             ENDIF
            ELSE
             INFO = -9999
             GO TO 1200
           ENDIF
            GO TO 1200
         END IF
         UPDATE = .FALSE.
C
         IF (MSGLVL .GT. 1) THEN
            CALL DVOUT  (LOGFIL, 1, RNORM, NDIGIT, 
     &           '_NAUP2: CORRESPONDING B-NORM OF THE RESIDUAL')
         END IF
C 
C        %--------------------------------------------------------%
C        | COMPUTE THE EIGENVALUES AND CORRESPONDING ERROR BOUNDS |
C        | OF THE CURRENT UPPER HESSENBERG MATRIX.                |
C        %--------------------------------------------------------%
C
         CALL ZNEIGH  (RNORM, KPLUSP, H, LDH, RITZ, BOUNDS,
     &                Q, LDQ, WORKL, RWORK,  IERR)
C
         IF (IERR .NE. 0) THEN
            INFO = -8
            GO TO 1200
         END IF
C
C        %---------------------------------------------------%
C        | SELECT THE WANTED RITZ VALUES AND THEIR BOUNDS    |
C        | TO BE USED IN THE CONVERGENCE TEST.               |
C        | THE WANTED PART OF THE SPECTRUM AND CORRESPONDING |
C        | ERROR BOUNDS ARE IN THE LAST NEV LOC. OF RITZ,    |
C        | AND BOUNDS RESPECTIVELY.                          | 
C        %---------------------------------------------------%
C
         NEV = NEV0
         NP = NP0
C
C        %--------------------------------------------------%
C        | MAKE A COPY OF RITZ VALUES AND THE CORRESPONDING |
C        | RITZ ESTIMATES OBTAINED FROM ZNEIGH .             |
C        %--------------------------------------------------%
C
         CALL ZCOPY (KPLUSP,RITZ,1,WORKL(KPLUSP**2+1),1)
         CALL ZCOPY (KPLUSP,BOUNDS,1,WORKL(KPLUSP**2+KPLUSP+1),1)
C
C        %---------------------------------------------------%
C        | SELECT THE WANTED RITZ VALUES AND THEIR BOUNDS    |
C        | TO BE USED IN THE CONVERGENCE TEST.               |
C        | THE WANTED PART OF THE SPECTRUM AND CORRESPONDING |
C        | BOUNDS ARE IN THE LAST NEV LOC. OF RITZ           |
C        | BOUNDS RESPECTIVELY.                              |
C        %---------------------------------------------------%
C
         CALL ZNGETS  (ISHIFT, WHICH, NEV, NP, RITZ, BOUNDS)
C 
C        %------------------------------------------------------------%
C        | CONVERGENCE TEST: CURRENTLY WE USE THE FOLLOWING CRITERIA. |
C        | THE RELATIVE ACCURACY OF A RITZ VALUE IS CONSIDERED        |
C        | ACCEPTABLE IF:                                             |
C        |                                                            |
C        | ERROR_BOUNDS(I) .LE. TOL*MAX(EPS23, MAGNITUDE_OF_RITZ(I)). |
C        |                                                            |
C        %------------------------------------------------------------%
C
         NCONV  = 0
C
         DO 25 I = 1, NEV
            RTEMP = MAX( EPS23, DLAPY2 ( DBLE (RITZ(NP+I)),
     &                                  DIMAG (RITZ(NP+I)) ) ) 
            IF ( DLAPY2 (DBLE (BOUNDS(NP+I)),DIMAG (BOUNDS(NP+I))) 
     &                 .LE. TOL*RTEMP ) THEN
               NCONV = NCONV + 1
            END IF
   25    CONTINUE
C 
         IF (MSGLVL .GT. 2) THEN
            KP(1) = NEV
            KP(2) = NP
            KP(3) = NCONV
            CALL IVOUT (LOGFIL, 3, KP, NDIGIT, 
     &                  '_NAUP2: NEV, NP, NCONV ARE')
            CALL ZVOUT  (LOGFIL, KPLUSP, RITZ, NDIGIT,
     &           '_NAUP2: THE EIGENVALUES OF H')
            CALL ZVOUT  (LOGFIL, KPLUSP, BOUNDS, NDIGIT, 
     &          '_NAUP2: RITZ ESTIMATES OF THE CURRENT NCV RITZ VALUES')
         END IF
C
C        %---------------------------------------------------------%
C        | COUNT THE NUMBER OF UNWANTED RITZ VALUES THAT HAVE ZERO |
C        | RITZ ESTIMATES. IF ANY RITZ ESTIMATES ARE EQUAL TO ZERO |
C        | THEN A LEADING BLOCK OF H OF ORDER EQUAL TO AT LEAST    |
C        | THE NUMBER OF RITZ VALUES WITH ZERO RITZ ESTIMATES HAS  |
C        | SPLIT OFF. NONE OF THESE RITZ VALUES MAY BE REMOVED BY  |
C        | SHIFTING. DECREASE NP THE NUMBER OF SHIFTS TO APPLY. IF |
C        | NO SHIFTS MAY BE APPLIED, THEN PREPARE TO EXIT          |
C        %---------------------------------------------------------%
C
         NPTEMP = NP
         DO 30 J=1, NPTEMP
            IF (BOUNDS(J) .EQ. ZERO) THEN
               NP = NP - 1
               NEV = NEV + 1
            END IF
 30      CONTINUE
C     
         IF ( (NCONV .GE. NEV0) .OR. 
     &        (ITER .GT. MXITER) .OR.
     &        (NP .EQ. 0) ) THEN
C
            IF (MSGLVL .GT. 4) THEN
               CALL ZVOUT (LOGFIL, KPLUSP, WORKL(KPLUSP**2+1), NDIGIT,
     &             '_NAUP2: EIGENVALUES COMPUTED BY _NEIGH:')
               CALL ZVOUT (LOGFIL, KPLUSP, WORKL(KPLUSP**2+KPLUSP+1),
     &                     NDIGIT,
     &             '_NAUP2: RITZ ESTIMATES COMPUTED BY _NEIGH:')
            END IF
C     
C           %------------------------------------------------%
C           | PREPARE TO EXIT. PUT THE CONVERGED RITZ VALUES |
C           | AND CORRESPONDING BOUNDS IN RITZ(1:NCONV) AND  |
C           | BOUNDS(1:NCONV) RESPECTIVELY. THEN SORT. BE    |
C           | CAREFUL WHEN NCONV > NP                        |
C           %------------------------------------------------%
C
C           %------------------------------------------%
C           |  USE H( 3,1 ) AS STORAGE TO COMMUNICATE  |
C           |  RNORM TO ZNEUPD  IF NEEDED               |
C           %------------------------------------------%

            H(3,1) = DCMPLX (RNORM,RZERO)
C
C           %----------------------------------------------%
C           | SORT RITZ VALUES SO THAT CONVERGED RITZ      |
C           | VALUES APPEAR WITHIN THE FIRST NEV LOCATIONS |
C           | OF RITZ AND BOUNDS, AND THE MOST DESIRED ONE |
C           | APPEARS AT THE FRONT.                        |
C           %----------------------------------------------%
C
            IF (WHICH .EQ. 'LM') WPRIME = 'SM'
            IF (WHICH .EQ. 'SM') WPRIME = 'LM'
            IF (WHICH .EQ. 'LR') WPRIME = 'SR'
            IF (WHICH .EQ. 'SR') WPRIME = 'LR'
            IF (WHICH .EQ. 'LI') WPRIME = 'SI'
            IF (WHICH .EQ. 'SI') WPRIME = 'LI'
C
            CALL ZSORTC (WPRIME, .TRUE., KPLUSP, RITZ, BOUNDS)
C
C           %--------------------------------------------------%
C           | SCALE THE RITZ ESTIMATE OF EACH RITZ VALUE       |
C           | BY 1 / MAX(EPS23, MAGNITUDE OF THE RITZ VALUE).  |
C           %--------------------------------------------------%
C
            DO 35 J = 1, NEV0 
                RTEMP = MAX( EPS23, DLAPY2 ( DBLE (RITZ(J)),
     &                                       DIMAG (RITZ(J)) ) )
                BOUNDS(J) = BOUNDS(J)/RTEMP
 35         CONTINUE
C
C           %---------------------------------------------------%
C           | SORT THE RITZ VALUES ACCORDING TO THE SCALED RITZ |
C           | ESTIMATES.  THIS WILL PUSH ALL THE CONVERGED ONES |
C           | TOWARDS THE FRONT OF RITZ, BOUNDS (IN THE CASE    |
C           | WHEN NCONV < NEV.)                                |
C           %---------------------------------------------------%
C
            WPRIME = 'LM'
            CALL ZSORTC (WPRIME, .TRUE., NEV0, BOUNDS, RITZ)
C
C           %----------------------------------------------%
C           | SCALE THE RITZ ESTIMATE BACK TO ITS ORIGINAL |
C           | VALUE.                                       |
C           %----------------------------------------------%
C
            DO 40 J = 1, NEV0
                RTEMP = MAX( EPS23, DLAPY2 ( DBLE (RITZ(J)),
     &                                       DIMAG (RITZ(J)) ) )
                BOUNDS(J) = BOUNDS(J)*RTEMP
 40         CONTINUE
C
C           %-----------------------------------------------%
C           | SORT THE CONVERGED RITZ VALUES AGAIN SO THAT  |
C           | THE "THRESHOLD" VALUE APPEARS AT THE FRONT OF |
C           | RITZ AND BOUND.                               |
C           %-----------------------------------------------%
C
            CALL ZSORTC (WHICH, .TRUE., NCONV, RITZ, BOUNDS)
C
            IF (MSGLVL .GT. 1) THEN
               CALL ZVOUT  (LOGFIL, KPLUSP, RITZ, NDIGIT,
     &            '_NAUP2: SORTED EIGENVALUES')
               CALL ZVOUT  (LOGFIL, KPLUSP, BOUNDS, NDIGIT,
     &            '_NAUP2: SORTED RITZ ESTIMATES.')
            END IF
C
C           %------------------------------------%
C           | MAX ITERATIONS HAVE BEEN EXCEEDED. | 
C           %------------------------------------%
C
            IF (ITER .GT. MXITER .AND. NCONV .LT. NEV0) INFO = 1
C
C           %---------------------%
C           | NO SHIFTS TO APPLY. | 
C           %---------------------%
C
            IF (NP .EQ. 0 .AND. NCONV .LT. NEV0)  INFO = 2
C
            NP = NCONV
            GO TO 1100
C
         ELSE IF ( (NCONV .LT. NEV0) .AND. (ISHIFT .EQ. 1) ) THEN
C     
C           %-------------------------------------------------%
C           | DO NOT HAVE ALL THE REQUESTED EIGENVALUES YET.  |
C           | TO PREVENT POSSIBLE STAGNATION, ADJUST THE SIZE |
C           | OF NEV.                                         |
C           %-------------------------------------------------%
C
            NEVBEF = NEV
            NEV = NEV + MIN(NCONV, NP/2)
            IF (NEV .EQ. 1 .AND. KPLUSP .GE. 6) THEN
               NEV = KPLUSP / 2
            ELSE IF (NEV .EQ. 1 .AND. KPLUSP .GT. 3) THEN
               NEV = 2
            END IF
            NP = KPLUSP - NEV
C     
C           %---------------------------------------%
C           | IF THE SIZE OF NEV WAS JUST INCREASED |
C           | RESORT THE EIGENVALUES.               |
C           %---------------------------------------%
C     
            IF (NEVBEF .LT. NEV) 
     &         CALL ZNGETS  (ISHIFT, WHICH, NEV, NP, RITZ, BOUNDS)
C
         END IF              
C     
         IF (MSGLVL .GT. 0) THEN
            CALL IVOUT (LOGFIL, 1, NCONV, NDIGIT, 
     &           '_NAUP2: NO. OF "CONVERGED" RITZ VALUES AT THIS ITER.')
            IF (MSGLVL .GT. 1) THEN
               KP(1) = NEV
               KP(2) = NP
               CALL IVOUT (LOGFIL, 2, KP, NDIGIT, 
     &              '_NAUP2: NEV AND NP ARE')
               CALL ZVOUT  (LOGFIL, NEV, RITZ(NP+1), NDIGIT,
     &              '_NAUP2: "WANTED" RITZ VALUES ')
               CALL ZVOUT  (LOGFIL, NEV, BOUNDS(NP+1), NDIGIT,
     &              '_NAUP2: RITZ ESTIMATES OF THE "WANTED" VALUES ')
            END IF
         END IF
C
         IF (ISHIFT .EQ. 0) THEN
C
C           %-------------------------------------------------------%
C           | USER SPECIFIED SHIFTS: POP BACK OUT TO GET THE SHIFTS |
C           | AND RETURN THEM IN THE FIRST 2*NP LOCATIONS OF WORKL. |
C           %-------------------------------------------------------%
C
            USHIFT = .TRUE.
            IDO = 3
            GO TO 9000
         END IF
   50    CONTINUE
         USHIFT = .FALSE.
C
         IF ( ISHIFT .NE. 1 ) THEN
C 
C            %----------------------------------%
C            | MOVE THE NP SHIFTS FROM WORKL TO |
C            | RITZ, TO FREE UP WORKL           |
C            | FOR NON-EXACT SHIFT CASE.        |
C            %----------------------------------%
C
             CALL ZCOPY  (NP, WORKL, 1, RITZ, 1)
         END IF
C
         IF (MSGLVL .GT. 2) THEN 
            CALL IVOUT (LOGFIL, 1, NP, NDIGIT, 
     &                  '_NAUP2: THE NUMBER OF SHIFTS TO APPLY ')
            CALL ZVOUT  (LOGFIL, NP, RITZ, NDIGIT,
     &                  '_NAUP2: VALUES OF THE SHIFTS')
            IF ( ISHIFT .EQ. 1 ) 
     &          CALL ZVOUT  (LOGFIL, NP, BOUNDS, NDIGIT,
     &                  '_NAUP2: RITZ ESTIMATES OF THE SHIFTS')
         END IF
C
C        %---------------------------------------------------------%
C        | APPLY THE NP IMPLICIT SHIFTS BY QR BULGE CHASING.       |
C        | EACH SHIFT IS APPLIED TO THE WHOLE UPPER HESSENBERG     |
C        | MATRIX H.                                               |
C        | THE FIRST 2*N LOCATIONS OF WORKD ARE USED AS WORKSPACE. |
C        %---------------------------------------------------------%
C
         CALL ZNAPPS  (N, NEV, NP, RITZ, V, LDV, 
     &                H, LDH, RESID, Q, LDQ, WORKL, WORKD)
C
C        %---------------------------------------------%
C        | COMPUTE THE B-NORM OF THE UPDATED RESIDUAL. |
C        | KEEP B*RESID IN WORKD(1:N) TO BE USED IN    |
C        | THE FIRST STEP OF THE NEXT CALL TO ZNAITR .  |
C        %---------------------------------------------%
C
         CNORM = .TRUE.
         IF (BMAT .EQ. 'G') THEN
            NBX = NBX + 1
            CALL ZCOPY  (N, RESID, 1, WORKD(N+1), 1)
            IPNTR(1) = N + 1
            IPNTR(2) = 1
            IDO = 2
C 
C           %----------------------------------%
C           | EXIT IN ORDER TO COMPUTE B*RESID |
C           %----------------------------------%
C 
            GO TO 9000
         ELSE IF (BMAT .EQ. 'I') THEN
            CALL ZCOPY  (N, RESID, 1, WORKD, 1)
         END IF
C 
  100    CONTINUE
C 
C        %----------------------------------%
C        | BACK FROM REVERSE COMMUNICATION; |
C        | WORKD(1:N) := B*RESID            |
C        %----------------------------------%
C
C 
         IF (BMAT .EQ. 'G') THEN         
            CPNORM = ZDOTC  (N, RESID, 1, WORKD, 1)
            RNORM = SQRT(DLAPY2 (DBLE (CPNORM),DIMAG (CPNORM)))
         ELSE IF (BMAT .EQ. 'I') THEN
            RNORM = DZNRM2 (N, RESID, 1)
         END IF
         CNORM = .FALSE.
C
         IF (MSGLVL .GT. 2) THEN
            CALL DVOUT  (LOGFIL, 1, RNORM, NDIGIT, 
     &      '_NAUP2: B-NORM OF RESIDUAL FOR COMPRESSED FACTORIZATION')
            CALL ZMOUT  (LOGFIL, NEV, NEV, H, LDH, NDIGIT,
     &        '_NAUP2: COMPRESSED UPPER HESSENBERG MATRIX H')
         END IF
C 
      GO TO 1000
C
C     %---------------------------------------------------------------%
C     |                                                               |
C     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
C     |                                                               |
C     %---------------------------------------------------------------%
C
 1100 CONTINUE
C
      MXITER = ITER
      NEV = NCONV
C     
 1200 CONTINUE
      IDO = 99
C     
 9000 CONTINUE
C
C     %---------------%
C     | END OF ZNAUP2  |
C     %---------------%
C
      END
