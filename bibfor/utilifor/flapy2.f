      REAL*8 FUNCTION FLAPY2( X, Y )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK CALCULANT LA NORME D'UN VECTEUR DU PLAN.
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C  PURPOSE
C  =======
C
C  FLAPY2 RETURNS SQRT(X**2+Y**2), TAKING CARE NOT TO CAUSE UNNECESSARY
C  OVERFLOW.
C
C  ARGUMENTS
C  =========
C
C  X       (INPUT) REAL*8
C  Y       (INPUT) REAL*8
C          X AND Y SPECIFY THE VALUES X AND Y.
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            ABS, MAX, MIN, SQRT.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      REAL*8   X, Y
C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      REAL*8   ONE
      PARAMETER          ( ONE = 1.0D0 )
C     ..
C     .. LOCAL SCALARS ..
      REAL*8   W, XABS, YABS, Z
C     ..
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         FLAPY2 = W
      ELSE
         FLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      END IF
C
C     END OF FLAPY2
C
      END
