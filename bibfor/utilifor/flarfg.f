      SUBROUTINE FLARFG( N, ALPHA, X, INCX, TAU )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK CALCULANT UN REFLECTEUR H TEL QUE DECRIT
C     CI DESSOUS.
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C  PURPOSE
C  =======
C
C  FLARFG GENERATES A REAL ELEMENTARY REFLECTOR H OF ORDER N, SUCH
C  THAT
C
C        H * ( ALPHA ) = ( BETA ),   H' * H = I.
C            (   X   )   (   0  )
C
C  WHERE ALPHA AND BETA ARE SCALARS, AND X IS AN (N-1)-ELEMENT REAL
C  VECTOR. H IS REPRESENTED IN THE FORM
C
C        H = I - TAU * ( 1 ) * ( 1 V' ) ,
C                      ( V )
C
C  WHERE TAU IS A REAL SCALAR AND V IS A REAL (N-1)-ELEMENT
C  VECTOR.
C
C  IF THE ELEMENTS OF X ARE ALL ZERO, THEN TAU = 0 AND H IS TAKEN TO BE
C  THE UNIT MATRIX.
C
C  OTHERWISE  1 <= TAU <= 2.
C
C  ARGUMENTS
C  =========
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE ELEMENTARY REFLECTOR.
C
C  ALPHA   (INPUT/OUTPUT) REAL*8
C          ON ENTRY, THE VALUE ALPHA.
C          ON EXIT, IT IS OVERWRITTEN WITH THE VALUE BETA.
C
C  X       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION
C                         (1+(N-2)*ABS(INCX))
C          ON ENTRY, THE VECTOR X.
C          ON EXIT, IT IS OVERWRITTEN WITH THE VECTOR V.
C
C  INCX    (INPUT) INTEGER
C          THE INCREMENT BETWEEN ELEMENTS OF X. INCX > 0.
C
C  TAU     (OUTPUT) REAL*8
C          THE VALUE TAU.
C
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE DLAMCH PAR R8PREM ET R8MIEM,
C            REMPLACEMENT DE RETURN PAR GOTO 1000,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C INTRINSIC FUNCTION
C    ABS, SIGN
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      INTEGER            INCX, N
      REAL*8   ALPHA, TAU
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   X( * )

C     .. PARAMETERS ..
      REAL*8   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            J, KNT
      REAL*8   BETA, RSAFMN, SAFMIN, XNORM
C     ..
C     .. EXTERNAL FUNCTIONS ..
      REAL*8   R8PREM, R8MIEM, FLAPY2, DNRM2
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( N.LE.1 ) THEN
         TAU = ZERO
         GOTO 1000
      END IF
C
      XNORM = DNRM2( N-1, X, INCX )
C
      IF( XNORM.EQ.ZERO ) THEN
C
C        H  =  I
C
         TAU = ZERO
      ELSE
C
C        GENERAL CASE
C
         BETA = -SIGN( FLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = R8MIEM() / (R8PREM()*0.5D0)
         IF( ABS( BETA ).LT.SAFMIN ) THEN
C
C           XNORM, BETA MAY BE INACCURATE, SCALE X AND RECOMPUTE THEM
C
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     &         GO TO 10
C
C           NEW BETA IS AT MOST 1, AT LEAST SAFMIN
C
            XNORM = DNRM2( N-1, X, INCX )
            BETA = -SIGN( FLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
C
C           IF ALPHA IS SUBNORMAL, IT MAY LOSE RELATIVE ACCURACY
C
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         END IF
      END IF
C
 1000 CONTINUE
C
C     END OF FLARFG
C
      END
