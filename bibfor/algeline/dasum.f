      REAL*8 FUNCTION DASUM(N,DX,INCX)
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/02/2000   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) LINPACK
C ======================================================================
C 
C     SUBROUTINE LINPACK CALCULANT UNE SOMME DE VALEUR ABSOLUE.
C-----------------------------------------------------------------------
C     TAKES THE SUM OF THE ABSOLUTE VALUES.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED TO CORRECT PROBLEM WITH NEGATIVE INCREMENT, 8/21/90.
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
C            REMPLACEMENT DE DABS PAR ABS, 
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            ABS.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
      
      REAL*8 DX(1),DTEMP
      INTEGER I,INCX,IX,M,MP1,N
C
      DASUM = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0) GOTO 1000
      IF(INCX.EQ.1) GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + ABS(DX(IX))
        IX = IX + INCX
   10 CONTINUE
      DASUM = DTEMP
      GOTO 1000
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
C DUE TO CRP_11
   20 CONTINUE
      M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + ABS(DX(I))
   30 CONTINUE
      IF( N .LT. 6 ) GO TO 60
C DUE TO CRP_11
   40 CONTINUE
      MP1 = M + 1
      DO 50 I = MP1,N,6
        DTEMP = DTEMP + ABS(DX(I)) + ABS(DX(I + 1)) + ABS(DX(I + 2))
     *  + ABS(DX(I + 3)) + ABS(DX(I + 4)) + ABS(DX(I + 5))
   50 CONTINUE
C DUE TO CRP_11
   60 CONTINUE
      DASUM = DTEMP
   
 1000 CONTINUE
      END
