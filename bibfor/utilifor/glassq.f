      SUBROUTINE GLASSQ( N, X, INCX, SCALE, SUMSQ )
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
C  PURPOSE
C  =======
C
C  GLASSQ RETURNS THE VALUES SCL AND SSQ SUCH THAT
C
C     ( SCL**2 )*SSQ = X( 1 )**2 +...+ X( N )**2 + ( SCALE**2 )*SUMSQ,
C
C  WHERE X( I ) = ABS( X( 1 + ( I - 1 )*INCX ) ). THE VALUE OF SUMSQ IS
C  ASSUMED TO BE AT LEAST UNITY AND THE VALUE OF SSQ WILL THEN SATISFY
C
C     1.0 .LE. SSQ .LE. ( SUMSQ + 2*N ).
C
C  SCALE IS ASSUMED TO BE NON-NEGATIVE AND SCL RETURNS THE VALUE
C
C     SCL = MAX( SCALE, ABS( REAL( X( I ) ) ), ABS( AIMAG( X( I ) ) ) ),
C            I
C
C  SCALE AND SUMSQ MUST BE SUPPLIED IN SCALE AND SUMSQ RESPECTIVELY.
C  SCALE AND SUMSQ ARE OVERWRITTEN BY SCL AND SSQ RESPECTIVELY.
C
C  THE ROUTINE MAKES ONLY ONE PASS THROUGH THE VECTOR X.
C
C  ARGUMENTS
C  =========
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF ELEMENTS TO BE USED FROM THE VECTOR X.
C
C  X       (INPUT) DOUBLE PRECISION
C          THE VECTOR X AS DESCRIBED ABOVE.
C             X( I )  = X( 1 + ( I - 1 )*INCX ), 1 <= I <= N.
C
C  INCX    (INPUT) INTEGER
C          THE INCREMENT BETWEEN SUCCESSIVE VALUES OF THE VECTOR X.
C          INCX > 0.
C
C  SCALE   (INPUT/OUTPUT) DOUBLE PRECISION
C          ON ENTRY, THE VALUE  SCALE  IN THE EQUATION ABOVE.
C          ON EXIT, SCALE IS OVERWRITTEN WITH THE VALUE  SCL .
C
C  SUMSQ   (INPUT/OUTPUT) DOUBLE PRECISION
C          ON ENTRY, THE VALUE  SUMSQ  IN THE EQUATION ABOVE.
C          ON EXIT, SUMSQ IS OVERWRITTEN WITH THE VALUE  SSQ .
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
      REAL*8             SCALE, SUMSQ
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         X( * )
C     ..
C     .. PARAMETERS ..
      REAL*8             ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            IX
      REAL*8             TEMP1
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( DBLE( X( IX ) ).NE.ZERO ) THEN
               TEMP1 = ABS( DBLE( X( IX ) ) )
               IF( SCALE.LT.TEMP1 ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SCALE = TEMP1
               ELSE
                  SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2
               END IF
            END IF
            IF( DIMAG( X( IX ) ).NE.ZERO ) THEN
               TEMP1 = ABS( DIMAG( X( IX ) ) )
               IF( SCALE.LT.TEMP1 ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SCALE = TEMP1
               ELSE
                  SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
C
 1000 CONTINUE
C
C     END OF GLASSQ
C
      END
