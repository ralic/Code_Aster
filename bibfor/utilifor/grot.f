      SUBROUTINE GROT( N, CX, INCX, CY, INCY, C, S )
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
C  GROT   APPLIES A PLANE ROTATION, WHERE THE COS (C) IS REAL AND THE
C  SIN (S) IS COMPLEX, AND THE VECTORS CX AND CY ARE COMPLEX.
C
C  ARGUMENTS
C  =========
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF ELEMENTS IN THE VECTORS CX AND CY.
C
C  CX      (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (N)
C          ON INPUT, THE VECTOR X.
C          ON OUTPUT, CX IS OVERWRITTEN WITH C*X + S*Y.
C
C  INCX    (INPUT) INTEGER
C          THE INCREMENT BETWEEN SUCCESSIVE VALUES OF CY.  INCX <> 0.
C
C  CY      (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (N)
C          ON INPUT, THE VECTOR Y.
C          ON OUTPUT, CY IS OVERWRITTEN WITH -CONJG(S)*X + C*Y.
C
C  INCY    (INPUT) INTEGER
C          THE INCREMENT BETWEEN SUCCESSIVE VALUES OF CY.  INCX <> 0.
C
C  C       (INPUT) DOUBLE PRECISION
C  S       (INPUT) COMPLEX*16
C          C AND S DEFINE A ROTATION
C             [  C          S  ]
C             [ -CONJG(S)   C  ]
C          WHERE C*C + S*CONJG(S) = 1.0.
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
C     .. SCALAR ARGUMENTS ..
      INTEGER            INCX, INCY, N
      REAL*8             C
      COMPLEX*16         S
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         CX( * ), CY( * )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, IX, IY
      COMPLEX*16         STEMP
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( N.LE.0 )
     $    GOTO 1000
      IF( INCX.EQ.1 .AND. INCY.EQ.1 )
     $   GO TO 20
C
C     CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF( INCX.LT.0 )
     $   IX = ( -N+1 )*INCX + 1
      IF( INCY.LT.0 )
     $   IY = ( -N+1 )*INCY + 1
      DO 10 I = 1, N
         STEMP = C*CX( IX ) + S*CY( IY )
         CY( IY ) = C*CY( IY ) - DCONJG( S )*CX( IX )
         CX( IX ) = STEMP
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      GOTO 1000
C
C     CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 CONTINUE
      DO 30 I = 1, N
         STEMP = C*CX( I ) + S*CY( I )
         CY( I ) = C*CY( I ) - DCONJG( S )*CX( I )
         CX( I ) = STEMP
   30 CONTINUE
 1000 CONTINUE

      END
