      REAL*8 FUNCTION GLAPY3( X, Y, Z )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C  Purpose
C  =======
C
C  GLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
C  unnecessary overflow.
C
C  Arguments
C  =========
C
C  X       (input) DOUBLE PRECISION
C  Y       (input) DOUBLE PRECISION
C  Z       (input) DOUBLE PRECISION
C          X, Y and Z specify the values x, y and z.
C
C  =====================================================================
C
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. Scalar Arguments ..
      REAL*8   X, Y, Z
C     ..
C     .. Parameters ..
      REAL*8             ZERO
      PARAMETER          ( ZERO = 0.0D0 )
C     ..
C     .. Local Scalars ..
      REAL*8              W, XABS, YABS, ZABS
C     ..
C     .. Executable Statements ..
C
      XABS = ABS( X )
      YABS = ABS( Y )
      ZABS = ABS( Z )
      W = MAX( XABS, YABS, ZABS )
      IF( W.EQ.ZERO ) THEN
         GLAPY3 = ZERO
      ELSE
         GLAPY3 = W*SQRT( ( XABS / W )**2+( YABS / W )**2+
     $            ( ZABS / W )**2 )
      END IF
 1000 CONTINUE
C
C     End of GLAPY3
C
      END
