      INTEGER   FUNCTION GZMAX1( N, CX, INCX )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C  PURPOSE
C  =======
C
C  GZMAX1 FINDS THE INDEX OF THE ELEMENT WHOSE REAL PART HAS MAXIMUM
C  ABSOLUTE VALUE.
C
C  BASED ON IZAMAX FROM LEVEL 1 BLAS.
C  THE CHANGE IS TO USE THE 'GENUINE' ABSOLUTE VALUE.
C
C  CONTRIBUTED BY NICK HIGHAM FOR USE WITH GLACON.
C
C  ARGUMENTS
C  =========
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF ELEMENTS IN THE VECTOR CX.
C
C  CX      (INPUT) COMPLEX*16 ARRAY, DIMENSION (N)
C          THE VECTOR WHOSE ELEMENTS WILL BE SUMMED.
C
C  INCX    (INPUT) INTEGER
C          THE SPACING BETWEEN SUCCESSIVE VALUES OF CX.  INCX >= 1.
C
C =====================================================================
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C
C     .. SCALAR ARGUMENTS ..
      INTEGER            INCX, N
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         CX( * )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, IX
      REAL*8             SMAX
      COMPLEX*16         ZDUM
C     ..
C     .. STATEMENT FUNCTIONS ..
      REAL*8             CABS1
C     ..
C     .. STATEMENT FUNCTION DEFINITIONS ..
C
C     NEXT LINE IS THE ONLY MODIFICATION.
      CABS1( ZDUM ) = ABS( DBLE( ZDUM ) )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      GZMAX1 = 0
      IF( N.LT.1 )
     $   GOTO 1000
      GZMAX1 = 1
      IF( N.EQ.1 )
     $   GOTO 1000
      IF( INCX.EQ.1 )
     $   GO TO 30
C
C     CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      SMAX = CABS1( CX( 1 ) )
      IX = IX + INCX
      DO 20 I = 2, N
         IF( CABS1( CX( IX ) ).LE.SMAX )
     $      GO TO 10
         GZMAX1 = I
         SMAX = CABS1( CX( IX ) )
   10    CONTINUE
         IX = IX + INCX
   20 CONTINUE
      GOTO 1000
C
C     CODE FOR INCREMENT EQUAL TO 1
C
   30 CONTINUE
      SMAX = CABS1( CX( 1 ) )
      DO 40 I = 2, N
         IF( CABS1( CX( I ) ).LE.SMAX )
     $      GO TO 40
         GZMAX1 = I
         SMAX = CABS1( CX( I ) )
   40 CONTINUE
 1000 CONTINUE
C
C     END OF GZMAX1
C
      END
