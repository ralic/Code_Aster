      COMPLEX*16 FUNCTION GLDOTU(N,ZX,INCX,ZY,INCY)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     JACK DONGARRA, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C ======================================================================
C REMPLACE LA BLAS ZDOTU SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
      COMPLEX*16 ZX(*),ZY(*),ZTEMP
      INTEGER I,INCX,INCY,IX,IY,N
      ZTEMP = (0.0D0,0.0D0)
      GLDOTU = (0.0D0,0.0D0)
      IF(N.LE.0)GOTO 9999
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        ZTEMP = ZTEMP + ZX(IX)*ZY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      GLDOTU = ZTEMP
      GOTO 9999
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 CONTINUE
      DO 30 I = 1,N
        ZTEMP = ZTEMP + ZX(I)*ZY(I)
   30 CONTINUE
      GLDOTU = ZTEMP
 9999 CONTINUE
      END
