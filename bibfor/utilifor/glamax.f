      INTEGER FUNCTION GLAMAX(N,ZX,INCX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, 1/15/85.
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C ======================================================================
C REMPLACE LA BLAS IZAMAX SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
      COMPLEX*16 ZX(*)
      REAL*8 SMAX
      INTEGER I,INCX,IX,N
      REAL*8  GLABS1
C
      GLAMAX = 0
      IF( (N.LT.1) .OR. (INCX.LE.0)) GOTO 9999
      GLAMAX = 1
      IF(N.EQ.1) GOTO 9999
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      SMAX = GLABS1(ZX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF(GLABS1(ZX(IX)).LE.SMAX) GO TO 5
         GLAMAX = I
         SMAX = GLABS1(ZX(IX))
    5    CONTINUE
         IX = IX + INCX
   10 CONTINUE
      GOTO 9999
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 CONTINUE
      SMAX = GLABS1(ZX(1))
      DO 30 I = 2,N
         IF(GLABS1(ZX(I)).LE.SMAX) GO TO 30
         GLAMAX = I
         SMAX = GLABS1(ZX(I))
   30 CONTINUE
 9999 CONTINUE
      END
