      SUBROUTINE  BLSWAP (N,DX,INCX,DY,INCY)
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) LINPACK
C ======================================================================
C
C     SUBROUTINE BLAS 1 PERMUTANT DEUX VECTEURS DX ET DY.
C-----------------------------------------------------------------------
C     INTERCHANGES TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C   ------------------------------------------------------------------
C     PARAMETRES D'APPELS:
C
C IN      N     : IS : TAILLE DES VECTEURS DX ET DY
C IN/OUT  DX    : R8 : DX(1..N) VECTEUR 1
C IN      INCX  : IS : INCREMENT SUR LES COMPOSANTES DE DX
C IN/OUT  DY    : R8 : DY(1..N) VECTEUR 2
C IN      INCY  : IS : INCREMENT SUR LES COMPOSANTES DE DY
C   ------------------------------------------------------------------
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            MODIFICATION DU NOM DE LA ROUTINE DSWAP -> BLSWAP,
C            REMPLACEMENT DE 3 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

      REAL*8 DX(1),DY(1),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0) GOTO 1000
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DX(IX)
        DX(IX) = DY(IY)
        DY(IY) = DTEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      GOTO 1000
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP
C
   20 CONTINUE
      M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP
   30 CONTINUE
      IF( N .LT. 3 ) GOTO 1000
   40 CONTINUE
      MP1 = M + 1
      DO 50 I = MP1,N,3
        DTEMP = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP
        DTEMP = DX(I + 1)
        DX(I + 1) = DY(I + 1)
        DY(I + 1) = DTEMP
        DTEMP = DX(I + 2)
        DX(I + 2) = DY(I + 2)
        DY(I + 2) = DTEMP
   50 CONTINUE
 1000 CONTINUE
      END
