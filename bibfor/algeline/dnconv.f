      SUBROUTINE DNCONV
     &  (N, RITZR, RITZI, BOUNDS, TOL, NCONV)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 06/11/2006   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE ARPACK EFFECTUANT LES TESTS DE CONVERGENCE.
C-----------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  CONVERGENCE TESTING FOR THE NONSYMMETRIC ARNOLDI EIGENVALUE ROUTINE.
C
C ARGUMENTS
C  N       INTEGER.  (INPUT)
C          NUMBER OF RITZ VALUES TO CHECK FOR CONVERGENCE.
C
C  RITZR,  REAL*8 ARRAYS OF LENGTH N.  (INPUT)
C  RITZI   REAL AND IMAGINARY PARTS OF THE RITZ VALUES TO BE CHECKED
C          FOR CONVERGENCE.

C  BOUNDS  REAL*8 ARRAY OF LENGTH N.  (INPUT)
C          RITZ ESTIMATES FOR THE RITZ VALUES IN RITZR AND RITZI.
C
C  TOL     REAL*8 SCALAR.  (INPUT)
C          DESIRED BACKWARD ERROR FOR A RITZ VALUE TO BE CONSIDERED
C          "CONVERGED".
C
C  NCONV   INTEGER SCALAR.  (OUTPUT)
C          NUMBER OF "CONVERGED" RITZ VALUES.
C
C ENDDOC
C-----------------------------------------------------------------------
C BEGINLIB
C
C ROUTINES CALLED:
C   DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C
C INTRINSIC FUNCTIONS:
C   MAX.
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
C FILE: NCONV.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            DISPARITION DE SECOND ET DLAMCH,
C            DISPARITION DU COMMON TIMING ET DEBUG,
C            UTILISATION DE R8PREM(),
C            IMPLICIT NONE.
C
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%

      INTEGER    N, NCONV
      REAL*8 TOL

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      REAL*8 RITZR(N), RITZI(N), BOUNDS(N)

C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%

      INTEGER I
      REAL*8 TEMP, EPS23

C     %-----------%
C     | FUNCTIONS |
C     %-----------%

      REAL*8 DLAPY2, R8PREM

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
      CALL MATFPE(-1)

C     %-------------------------------------------------------------%
C     | CONVERGENCE TEST: UNLIKE IN THE SYMMETRIC CODE, I AM NOT    |
C     | USING THINGS LIKE REFINED ERROR BOUNDS AND GAP CONDITION    |
C     | BECAUSE I DON'T KNOW THE EXACT EQUIVALENT CONCEPT.          |
C     |                                                             |
C     | INSTEAD THE I-TH RITZ VALUE IS CONSIDERED "CONVERGED" WHEN: |
C     |                                                             |
C     |     BOUNDS(I) .LE. ( TOL * | RITZ | )                       |
C     |                                                             |
C     | FOR SOME APPROPRIATE CHOICE OF NORM.                        |
C     %-------------------------------------------------------------%

C     %---------------------------------%
C     | GET MACHINE DEPENDENT CONSTANT. |
C     %---------------------------------%
C
      EPS23 = (R8PREM()*0.5D0)**(2.0D+0 / 3.0D+0)
C
      NCONV  = 0
      DO 20 I = 1, N
         TEMP = MAX( EPS23, DLAPY2( RITZR(I), RITZI(I) ) )
         IF (BOUNDS(I) .LE. TOL*TEMP)   NCONV = NCONV + 1
   20 CONTINUE
C
      CALL MATFPE(1)

C     %---------------%
C     | END OF DNCONV |
C     %---------------%

      END
