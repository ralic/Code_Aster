      SUBROUTINE DNEIGH
     &  (RNORM, N, H, LDH, RITZR, RITZI, BOUNDS, Q, LDQ, WORKL, IERR)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE ARPACK CALCULANT LES MODES PROPRES DE LA MATRICE DE
C     HESSENBERG AINSI QUE LEURS ERREURS.
C-----------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  COMPUTE THE EIGENVALUES OF THE CURRENT UPPER HESSENBERG MATRIX
C  AND THE CORRESPONDING RITZ ESTIMATES GIVEN THE CURRENT RESIDUAL NORM.
C
C ARGUMENTS
C  RNORM   REAL*8 SCALAR.  (INPUT)
C          RESIDUAL NORM CORRESPONDING TO THE CURRENT UPPER HESSENBERG
C          MATRIX H.
C
C  N       INTEGER.  (INPUT)
C          SIZE OF THE MATRIX H.
C
C  H       REAL*8 N BY N ARRAY.  (INPUT)
C          H CONTAINS THE CURRENT UPPER HESSENBERG MATRIX.
C
C  LDH     INTEGER.  (INPUT)
C          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  RITZR,  REAL*8 ARRAYS OF LENGTH N.  (OUTPUT)
C  RITZI   ON OUTPUT, RITZR(1:N) (RESP. RITZI(1:N)) CONTAINS THE REAL
C          (RESPECTIVELY IMAGINARY) PARTS OF THE EIGENVALUES OF H.
C
C  BOUNDS  REAL*8 ARRAY OF LENGTH N.  (OUTPUT)
C         ON OUTPUT, BOUNDS CONTAINS THE RITZ ESTIMATES ASSOCIATED WITH
C         THE EIGENVALUES RITZR AND RITZI.  THIS IS EQUAL TO RNORM
C         TIMES THE LAST COMPONENTS OF THE EIGENVECTORS CORRESPONDING
C         TO THE EIGENVALUES IN RITZR AND RITZI.
C
C  Q       REAL*8 N BY N ARRAY.  (WORKSPACE)
C          WORKSPACE NEEDED TO STORE THE EIGENVECTORS OF H.
C
C  LDQ     INTEGER.  (INPUT)
C          LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  WORKL   REAL*8 WORK ARRAY OF LENGTH N**2 + 3*N.  (WORKSPACE)
C          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
C          THE FRONT END.  THIS IS NEEDED TO KEEP THE FULL SCHUR FORM
C          OF H AND ALSO IN THE CALCULATION OF THE EIGENVECTORS OF H.
C
C  IERR    INTEGER.  (OUTPUT)
C          ERROR EXIT FLAG FROM DLAQRB OR FTREVC.
C
C ENDDOC
C-----------------------------------------------------------------------
C
C BEGINLIB
C
C ROUTINES CALLED:
C     DLAQRB  ARPACK ROUTINE TO COMPUTE THE REAL SCHUR FORM OF AN
C             UPPER HESSENBERG MATRIX AND LAST ROW OF THE SCHUR VECTORS.
C     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     FLACPY  LAPACK MATRIX COPY ROUTINE.
C     FLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C     FTREVC  LAPACK ROUTINE TO COMPUTE THE EIGENVECTORS OF A MATRIX
C             IN UPPER QUASI-TRIANGULAR FORM
C     BLGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
C     BLCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
C     BLNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
C     BLSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
C
C INTRINSIC FUNCTIONS
C
C     ABS
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
C FILE: NEIGH.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE SECOND,
C            COMMON TIMING REMPLACE PAR COMMON INFOR,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C
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

      INTEGER IERR, N, LDH, LDQ
      REAL*8 RNORM

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      REAL*8 BOUNDS(N), H(LDH,N), Q(LDQ,N), RITZI(N), RITZR(N),
     &  WORKL(N*(N+3))

C     %------------%
C     | PARAMETERS |
C     %------------%

      REAL*8 ONE, ZERO
      PARAMETER (ONE = 1.0D+0, ZERO = 0.0D+0)

C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%

      LOGICAL SELECT(1)
      INTEGER I, ICONJ, MSGLVL
      REAL*8 TEMP, VL(1)

C     %-----------%
C     | FUNCTIONS |
C     %-----------%

      REAL*8 FLAPY2, BLNRM2

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

C     %-------------------------------%
C     | INITIALIZE TIMING STATISTICS  |
C     | & MESSAGE LEVEL FOR DEBUGGING |
C     %-------------------------------%

      MSGLVL = MNEIGH

      IF (MSGLVL .GT. 2) THEN
          CALL DMOUT (LOGFIL, N, N, H, LDH, NDIGIT,
     &         '_NEIGH: ENTERING UPPER HESSENBERG MATRIX H ')
      ENDIF

C     %-----------------------------------------------------------%
C     | 1. COMPUTE THE EIGENVALUES, THE LAST COMPONENTS OF THE    |
C     |    CORRESPONDING SCHUR VECTORS AND THE FULL SCHUR FORM T  |
C     |    OF THE CURRENT UPPER HESSENBERG MATRIX H.              |
C     | DLAQRB RETURNS THE FULL SCHUR FORM OF H IN WORKL(1:N**2)  |
C     | AND THE LAST COMPONENTS OF THE SCHUR VECTORS IN BOUNDS.   |
C     %-----------------------------------------------------------%
C DUE TO CRP_102 CALL FLACPY ('ALL', N, N, H, LDH, WORKL, N)
      CALL FLACPY ('A', N, N, H, LDH, WORKL, N)
      CALL DLAQRB (.TRUE., N, 1, N, WORKL, N, RITZR, RITZI, BOUNDS,
     &             IERR)
      IF (IERR .NE. 0) GO TO 9000

      IF (MSGLVL .GT. 1) THEN
          CALL DVOUT (LOGFIL, N, BOUNDS, NDIGIT,
     &              '_NEIGH: LAST ROW OF THE SCHUR MATRIX FOR H')
      ENDIF

C     %-----------------------------------------------------------%
C     | 2. COMPUTE THE EIGENVECTORS OF THE FULL SCHUR FORM T AND  |
C     |    APPLY THE LAST COMPONENTS OF THE SCHUR VECTORS TO GET  |
C     |    THE LAST COMPONENTS OF THE CORRESPONDING EIGENVECTORS. |
C     | REMEMBER THAT IF THE I-TH AND (I+1)-ST EIGENVALUES ARE    |
C     | COMPLEX CONJUGATE PAIRS, THEN THE REAL & IMAGINARY PART   |
C     | OF THE EIGENVECTOR COMPONENTS ARE SPLIT ACROSS ADJACENT   |
C     | COLUMNS OF Q.                                             |
C     %-----------------------------------------------------------%

      CALL FTREVC ('R', 'A', SELECT, N, WORKL, N, VL, N, Q, LDQ,
     &             N, N, WORKL(N*N+1), IERR)

      IF (IERR .NE. 0) GO TO 9000

C     %------------------------------------------------%
C     | SCALE THE RETURNING EIGENVECTORS SO THAT THEIR |
C     | EUCLIDEAN NORMS ARE ALL ONE. LAPACK SUBROUTINE |
C     | FTREVC RETURNS EACH EIGENVECTOR NORMALIZED SO  |
C     | THAT THE ELEMENT OF LARGEST MAGNITUDE HAS      |
C     | MAGNITUDE 1, HERE THE MAGNITUDE OF A COMPLEX   |
C     | NUMBER (X,Y) IS TAKEN TO BE |X| + |Y|.         |
C     %------------------------------------------------%

      ICONJ = 0
      DO 10 I=1, N
         IF ( ABS( RITZI(I) ) .LE. ZERO ) THEN

C           %----------------------%
C           | REAL EIGENVALUE CASE |
C           %----------------------%
            TEMP = BLNRM2( N, Q(1,I), 1 )
            CALL BLSCAL ( N, ONE / TEMP, Q(1,I), 1 )
         ELSE

C           %-------------------------------------------%
C           | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
C           | SINCE THE REAL AND IMAGINARY PART OF      |
C           | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
C           | COLUMNS, WE FURTHER NORMALIZE BY THE      |
C           | SQUARE ROOT OF TWO.                       |
C           %-------------------------------------------%

            IF (ICONJ .EQ. 0) THEN
               TEMP = FLAPY2( BLNRM2( N, Q(1,I), 1 ),
     &                        BLNRM2( N, Q(1,I+1), 1 ) )
               CALL BLSCAL ( N, ONE / TEMP, Q(1,I), 1 )
               CALL BLSCAL ( N, ONE / TEMP, Q(1,I+1), 1 )
               ICONJ = 1
            ELSE
               ICONJ = 0
            END IF
         END IF
   10 CONTINUE

      CALL BLGEMV ('T', N, N, ONE, Q, LDQ, BOUNDS, 1, ZERO, WORKL, 1)

      IF (MSGLVL .GT. 1) THEN
          CALL DVOUT (LOGFIL, N, WORKL, NDIGIT,
     &              '_NEIGH: LAST ROW OF THE EIGENVECTOR MATRIX FOR H')
      ENDIF

C     %----------------------------%
C     | COMPUTE THE RITZ ESTIMATES |
C     %----------------------------%

      ICONJ = 0
      DO 20 I = 1, N
         IF ( ABS( RITZI(I) ) .LE. ZERO ) THEN

C           %----------------------%
C           | REAL EIGENVALUE CASE |
C           %----------------------%

            BOUNDS(I) = RNORM * ABS( WORKL(I) )
         ELSE

C           %-------------------------------------------%
C           | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
C           | SINCE THE REAL AND IMAGINARY PART OF      |
C           | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
C           | COLUMNS, WE NEED TO TAKE THE MAGNITUDE    |
C           | OF THE LAST COMPONENTS OF THE TWO VECTORS |
C           %-------------------------------------------%

            IF (ICONJ .EQ. 0) THEN
               BOUNDS(I) = RNORM * FLAPY2( WORKL(I), WORKL(I+1) )
               BOUNDS(I+1) = BOUNDS(I)
               ICONJ = 1
            ELSE
               ICONJ = 0
            END IF
         END IF
   20 CONTINUE

      IF (MSGLVL .GT. 2) THEN
          CALL DVOUT (LOGFIL, N, RITZR, NDIGIT,
     &              '_NEIGH: REAL PART OF THE EIGENVALUES OF H')
          CALL DVOUT (LOGFIL, N, RITZI, NDIGIT,
     &              '_NEIGH: IMAGINARY PART OF THE EIGENVALUES OF H')
          CALL DVOUT (LOGFIL, N, BOUNDS, NDIGIT,
     &              '_NEIGH: RITZ ESTIMATES FOR THE EIGENVALUES OF H')
      ENDIF

 9000 CONTINUE

C     %---------------%
C     | END OF DNEIGH |
C     %---------------%

      END
