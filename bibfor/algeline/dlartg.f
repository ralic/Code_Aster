      SUBROUTINE DLARTG( F, G, CS, SN, R )
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C 
C     SUBROUTINE LAPACK GENERANT UNE ROTATION PLANE QUI EST UNE
C     VERSION PLUS PRECISE QUE LA ROUTINE BLAS1 DROTG.
C---------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C  PURPOSE
C  =======
C
C  DLARTG GENERATE A PLANE ROTATION SO THAT
C
C     (  CS  SN  )  .  ( F )  =  ( R )   WHERE CS**2 + SN**2 = 1.
C     ( -SN  CS  )     ( G )     ( 0 )
C
C  THIS IS A SLOWER, MORE ACCURATE VERSION OF THE BLAS1 ROUTINE DROTG,
C  WITH THE FOLLOWING OTHER DIFFERENCES:
C     F AND G ARE UNCHANGED ON RETURN.
C     IF G=0, THEN CS=1 AND SN=0.
C     IF F=0 AND (G .NE. 0), THEN CS=0 AND SN=1 WITHOUT DOING ANY
C        FLOATING POINT OPERATIONS (SAVES WORK IN DBDSQR WHEN
C        THERE ARE ZEROS ON THE DIAGONAL).
C
C  IF F EXCEEDS G IN MAGNITUDE, CS WILL BE POSITIVE.
C
C  ARGUMENTS
C  =========
C
C  F       (INPUT) REAL*8
C          THE FIRST COMPONENT OF VECTOR TO BE ROTATED.
C
C  G       (INPUT) REAL*8
C          THE SECOND COMPONENT OF VECTOR TO BE ROTATED.
C
C  CS      (OUTPUT) REAL*8
C          THE COSINE OF THE ROTATION.
C
C  SN      (OUTPUT) REAL*8
C          THE SINE OF THE ROTATION.
C
C  R       (OUTPUT) REAL*8
C          THE NONZERO COMPONENT OF THE ROTATED VECTOR.
C
C ASTER INFORMATION
C 11/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE DLAMCH PAR R8PREM(), R8MIEM() ET 
C            ISBAEM().
C 28/01/2000 RAJOUT DE LA VARIABLE BASE.
C INTRINSIC FUNCTIONS
C            ABS, INT, LOG, MAX, SQRT, DBLE.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
      
C     .. SCALAR ARGUMENTS ..
      REAL*8   CS, F, G, R, SN
C     ..
C     .. PARAMETERS ..
      REAL*8   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      REAL*8   ONE
      PARAMETER          ( ONE = 1.0D0 )
      REAL*8   TWO
      PARAMETER          ( TWO = 2.0D0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            FIRST
      INTEGER            COUNT, I
      REAL*8   EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE, BASE
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER ISBAEM
      REAL*8 R8PREM, R8MIEM
C     ..
C     .. SAVE STATEMENT ..
      SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2
C     ..
C     .. DATA STATEMENTS ..
      DATA               FIRST / .TRUE. /
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( FIRST ) THEN
         FIRST = .FALSE.
         SAFMIN = R8MIEM()
         EPS = R8PREM()*0.5D0
         BASE = DBLE(ISBAEM())
         SAFMN2 = BASE**INT( LOG( SAFMIN / EPS ) /
     &            LOG( BASE ) / TWO )
         SAFMX2 = ONE / SAFMN2
      END IF
      IF( G.EQ.ZERO ) THEN
         CS = ONE
         SN = ZERO
         R = F
      ELSE IF( F.EQ.ZERO ) THEN
         CS = ZERO
         SN = ONE
         R = G
      ELSE
         F1 = F
         G1 = G
         SCALE = MAX( ABS( F1 ), ABS( G1 ) )
         IF( SCALE.GE.SAFMX2 ) THEN
            COUNT = 0
   10       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMN2
            G1 = G1*SAFMN2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.GE.SAFMX2 )
     &         GO TO 10
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 20 I = 1, COUNT
               R = R*SAFMX2
   20       CONTINUE
         ELSE IF( SCALE.LE.SAFMN2 ) THEN
            COUNT = 0
   30       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMX2
            G1 = G1*SAFMX2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.LE.SAFMN2 )
     &         GO TO 30
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 40 I = 1, COUNT
               R = R*SAFMN2
   40       CONTINUE
         ELSE
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
         END IF
         IF( ABS( F ).GT.ABS( G ) .AND. CS.LT.ZERO ) THEN
            CS = -CS
            SN = -SN
            R = -R
         END IF
      END IF
C
C     END OF DLARTG
C
      END
