      SUBROUTINE  BLSROT (N,DX,INCX,DY,INCY,C,S)
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) LINPACK
C ======================================================================
C
C     SUBROUTINE BLAS 1 EFFECTUANT UNE ROTATION PLANE SUR LES VECTEURS
C     DX ET DY.
C-----------------------------------------------------------------------
C     APPLIES A PLANE ROTATION.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     EFFECTUE LA ROTATION PLANE D'ANGLE ALPHA SUR LE VECTEUR
C                 (X(I-1+INCX), Y(I-1+INCY)) POUR I=1...N
C   ------------------------------------------------------------------
C     PARAMETRES D'APPELS:
C
C IN      N     : IS : TAILLE DU VECTEUR DX
C IN/OUT  DX    : R8 : DX(1..N) VECTEUR 1
C IN      INCX  : IS : INCREMENT SUR LES COMPOSANTES DE DX
C IN/OUT  DY    : R8 : DY(1..N) VECTEUR 2
C IN      INCY  : IS : INCREMENT SUR LES COMPOSANTES DE DY
C IN      C     : R8 : COS(ALPHA) DE LA ROTATION PLANE
C IN      S     : R8 : SIN(ALPHA) DE LA ROTATION PLANE
C   ------------------------------------------------------------------
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            MODIFICATION DU NOM DE LA ROUTINE DROT -> BLSROT,
C            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

      REAL*8 DX(1),DY(1),DTEMP,C,S
      INTEGER I,INCX,INCY,IX,IY,N
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
        DTEMP = C*DX(IX) + S*DY(IY)
        DY(IY) = C*DY(IY) - S*DX(IX)
        DX(IX) = DTEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      GOTO 1000
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 CONTINUE
      DO 30 I = 1,N
        DTEMP = C*DX(I) + S*DY(I)
        DY(I) = C*DY(I) - S*DX(I)
        DX(I) = DTEMP
   30 CONTINUE
 1000 CONTINUE
      END
