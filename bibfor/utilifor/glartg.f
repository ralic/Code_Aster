      SUBROUTINE GLARTG( F, G, CS, SN, R )
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
C
C  PURPOSE
C  =======
C
C  GLARTG GENERATES A PLANE ROTATION SO THAT
C
C     [  CS  SN  ]     [ F ]     [ R ]
C     [  __      ]  .  [   ]  =  [   ]   WHERE CS**2 + |SN|**2 = 1.
C     [ -SN  CS  ]     [ G ]     [ 0 ]
C
C  THIS IS A FASTER VERSION OF THE BLAS1 ROUTINE GROTG, EXCEPT FOR
C  THE FOLLOWING DIFFERENCES:
C     F AND G ARE UNCHANGED ON RETURN.
C     IF G=0, THEN CS=1 AND SN=0.
C     IF F=0 AND (G .NE. 0), THEN CS=0 AND SN=1 WITHOUT DOING ANY
C        FLOATING POINT OPERATIONS.
C
C  ARGUMENTS
C  =========
C
C  F       (INPUT) COMPLEX*16
C          THE FIRST COMPONENT OF VECTOR TO BE ROTATED.
C
C  G       (INPUT) COMPLEX*16
C          THE SECOND COMPONENT OF VECTOR TO BE ROTATED.
C
C  CS      (OUTPUT) DOUBLE PRECISION
C          THE COSINE OF THE ROTATION.
C
C  SN      (OUTPUT) COMPLEX*16
C          THE SINE OF THE ROTATION.
C
C  R       (OUTPUT) COMPLEX*16
C          THE NONZERO COMPONENT OF THE ROTATED VECTOR.
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
      REAL*8             CS
      COMPLEX*16         F, G, R, SN
C     ..
C     .. PARAMETERS ..
      REAL*8              ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      COMPLEX*16         CZERO
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ) )
C     ..
C     .. LOCAL SCALARS ..
      REAL*8             D, DI, F1, F2, FA, G1, G2, GA
      COMPLEX*16         FS, GS, SS, T
C     ..
C     .. STATEMENT FUNCTIONS ..
      REAL*8             ABS1, ABSSQ
C     ..
C     .. STATEMENT FUNCTION DEFINITIONS ..
      ABS1( T ) = ABS( DBLE( T ) ) + ABS( DIMAG( T ) )
      ABSSQ( T ) = DBLE( T )**2 + DIMAG( T )**2
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     [ 25 OR 38 OPS FOR MAIN PATHS ]
C
      IF( G.EQ.CZERO ) THEN
         CS = ONE
         SN = ZERO
         R = F
      ELSE IF( F.EQ.CZERO ) THEN
         CS = ZERO
C
         SN = DCONJG( G ) / ABS( G )
         R = ABS( G )
C
C         SN = ONE
C         R = G
C
      ELSE
         F1 = ABS1( F )
         G1 = ABS1( G )
         IF( F1.GE.G1 ) THEN
            GS = G / F1
            G2 = ABSSQ( GS )
            FS = F / F1
            F2 = ABSSQ( FS )
            D = SQRT( ONE+G2 / F2 )
            CS = ONE / D
            SN = DCONJG( GS )*FS*( CS / F2 )
            R = F*D
         ELSE
            FS = F / G1
            F2 = ABSSQ( FS )
            FA = SQRT( F2 )
            GS = G / G1
            G2 = ABSSQ( GS )
            GA = SQRT( G2 )
            D = SQRT( ONE+F2 / G2 )
            DI = ONE / D
            CS = ( FA / GA )*DI
            SS = ( DCONJG( GS )*FS ) / ( FA*GA )
            SN = SS*DI
            R = G*SS*D
         END IF
      END IF
 1000 CONTINUE
C
C     END OF GLARTG
C
      END
