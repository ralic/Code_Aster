      REAL*8  FUNCTION GLASUM(N,ZX,INCX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C     TAKES THE SUM OF THE ABSOLUTE VALUES.
C     JACK DONGARRA, 3/11/78.
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
C ======================================================================
C REMPLACE LA BLAS DZASUM SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
      COMPLEX*16 ZX(*)
      REAL*8  STEMP,GLABS1
      INTEGER I,INCX,IX,N
C
      GLASUM = 0.0D0
      STEMP = 0.0D0
      IF( N.LE.0 .OR. INCX.LE.0 )GOTO 9999
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      DO 10 I = 1,N
        STEMP = STEMP + GLABS1(ZX(IX))
        IX = IX + INCX
   10 CONTINUE
      GLASUM = STEMP
      GOTO 9999
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 CONTINUE
      DO 30 I = 1,N
        STEMP = STEMP + GLABS1(ZX(I))
   30 CONTINUE
      GLASUM = STEMP
9999  CONTINUE
      END
