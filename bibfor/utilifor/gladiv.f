      COMPLEX*16   FUNCTION GLADIV( X, Y )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C
C  PURPOSE
C  =======
C
C  GLADIV := X / Y, WHERE X AND Y ARE COMPLEX.  THE COMPUTATION OF X / Y
C  WILL NOT OVERFLOW ON AN INTERMEDIARY STEP UNLESS THE RESULTS
C  OVERFLOWS.
C
C  ARGUMENTS
C  =========
C
C  X       (INPUT) COMPLEX*16
C  Y       (INPUT) COMPLEX*16
C          THE COMPLEX SCALARS X AND Y.
C
C  =====================================================================
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C     .. SCALAR ARGUMENTS ..
      COMPLEX*16         X, Y
C     ..
C
C     .. LOCAL SCALARS ..
      REAL*8             ZI, ZR
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      CALL FLADIV( DBLE( X ), DIMAG( X ), DBLE( Y ), DIMAG( Y ), ZR,
     $             ZI )
      GLADIV = DCMPLX( ZR, ZI )
C
 1000 CONTINUE
C
C     END OF GLADIV
C
      END
