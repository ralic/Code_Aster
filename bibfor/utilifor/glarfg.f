      SUBROUTINE GLARFG( N, ALPHA, X, INCX, TAU )
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
C  GLARFG GENERATES A COMPLEX ELEMENTARY REFLECTOR H OF ORDER N, SUCH
C  THAT
C
C        H' * ( ALPHA ) = ( BETA ),   H' * H = I.
C             (   X   )   (   0  )
C
C  WHERE ALPHA AND BETA ARE SCALARS, WITH BETA REAL, AND X IS AN
C  (N-1)-ELEMENT COMPLEX VECTOR. H IS REPRESENTED IN THE FORM
C
C        H = I - TAU * ( 1 ) * ( 1 V' ) ,
C                      ( V )
C
C  WHERE TAU IS A COMPLEX SCALAR AND V IS A COMPLEX (N-1)-ELEMENT
C  VECTOR. NOTE THAT H IS NOT HERMITIAN.
C
C  IF THE ELEMENTS OF X ARE ALL ZERO AND ALPHA IS REAL, THEN TAU = 0
C  AND H IS TAKEN TO BE THE UNIT MATRIX.
C
C  OTHERWISE  1 <= REAL(TAU) <= 2  AND  ABS(TAU-1) <= 1 .
C
C  ARGUMENTS
C  =========
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE ELEMENTARY REFLECTOR.
C
C  ALPHA   (INPUT/OUTPUT) COMPLEX*16
C          ON ENTRY, THE VALUE ALPHA.
C          ON EXIT, IT IS OVERWRITTEN WITH THE VALUE BETA.
C
C  X       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION
C                         (1+(N-2)*ABS(INCX))
C          ON ENTRY, THE VECTOR X.
C          ON EXIT, IT IS OVERWRITTEN WITH THE VECTOR V.
C
C  INCX    (INPUT) INTEGER
C          THE INCREMENT BETWEEN ELEMENTS OF X. INCX > 0.
C
C  TAU     (OUTPUT) COMPLEX*16
C          THE VALUE TAU.
C
C  =====================================================================
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      INTEGER            INCX, N
      COMPLEX*16         ALPHA, TAU
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         X( * )
C     ..
C     .. PARAMETERS ..
      REAL*8             ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            J, KNT
      REAL*8             ALPHI, ALPHR, BETA, RSAFMN, SAFMIN, XNORM
C     ..
C     .. EXTERNAL FUNCTIONS ..
      REAL*8             R8PREM, R8MIEM, GLAPY3, GLNRM2
      COMPLEX*16         GLADIV
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( N.LE.0 ) THEN
         TAU = ZERO
         GOTO 1000
      END IF
C
      XNORM = GLNRM2( N-1, X, INCX )
      ALPHR = DBLE( ALPHA )
      ALPHI = DIMAG( ALPHA )
C
      IF( XNORM.EQ.ZERO .AND. ALPHI.EQ.ZERO ) THEN
C
C        H  =  I
C
         TAU = ZERO
      ELSE
C
C        GENERAL CASE
C
         BETA = -SIGN( GLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
         SAFMIN = R8MIEM() / (R8PREM()*0.5D0)
         RSAFMN = ONE / SAFMIN
C
         IF( ABS( BETA ).LT.SAFMIN ) THEN
C
C           XNORM, BETA MAY BE INACCURATE; SCALE X AND RECOMPUTE THEM
C
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL GLSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHI = ALPHI*RSAFMN
            ALPHR = ALPHR*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
C
C           NEW BETA IS AT MOST 1, AT LEAST SAFMIN
C
            XNORM = GLNRM2( N-1, X, INCX )
            ALPHA = DCMPLX( ALPHR, ALPHI )
            BETA = -SIGN( GLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
            TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA )
            ALPHA = GLADIV( DCMPLX( ONE ), ALPHA-BETA )
            CALL ZLSCAL( N-1, ALPHA, X, INCX )
C
C           IF ALPHA IS SUBNORMAL, IT MAY LOSE RELATIVE ACCURACY
C
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA )
            ALPHA = GLADIV( DCMPLX( ONE ), ALPHA-BETA )
            CALL ZLSCAL( N-1, ALPHA, X, INCX )
            ALPHA = BETA
         END IF
      END IF
C
 1000 CONTINUE
C
C     END OF GLARFG
C
      END
