      SUBROUTINE  GDSCAL(N,DA,ZX,INCX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C     SCALES A VECTOR BY A CONSTANT.
C     JACK DONGARRA, 3/11/78.
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16 ZX(*)
      REAL*8     DA
      INTEGER I,INCX,IX,N
C
      IF( N.LE.0 .OR. INCX.LE.0 )GOTO 1000
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      DO 10 I = 1,N
        ZX(IX) = DCMPLX(DA,0.0D0)*ZX(IX)
        IX = IX + INCX
   10 CONTINUE
      GOTO 1000
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 CONTINUE
      DO 30 I = 1,N
        ZX(I) = DCMPLX(DA,0.0D0)*ZX(I)
   30 CONTINUE
 1000 CONTINUE
      END
