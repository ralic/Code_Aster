      SUBROUTINE R8ROT (N, DX, INCX, DY, INCY, C, S)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, INCX, INCY
      REAL *8 C, S, DX(*), DY(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C-----------------------------------------------------------------------
C         ROTATION PLANE (METHODE DE GIVENS).
C-----------------------------------------------------------------------
C IN  : N    : DIMENSION DES VECTEURS X ET Y.
C I/O : DX   : VECTEUR DE LONGUEUR  MAX(N*IABS(INCX),1).
C              R8ROT REMPLACE X(I) AVEC DC*X(I) + DS*Y(I) POUR I=1,..,N.
C              X(I) ET Y(I) SE REFERE AUX ELEMENTS DE DX ET DY.
C IN  : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE DX.
C              X(I) EST DEFINIE PAR
C                 DX(1+(I-1)*INCX) SI INCX.GE.0  OU
C                 DX(1+(I-N)*INCX) SI INCX.LT.0.
C I/O : DX   : VECTEUR DE LONGUEUR  MAX(N*IABS(INCY),1).
C              R8ROT REMPLACE Y(I) AVEC -DS*X(I) + DC*Y(I) POUR I=1.N
C              X(I) ET Y(I) SE REFERE AUX ELEMENTS DE DX ET DY.
C IN  : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE DX.
C              Y(I) EST DEFINIE PAR
C                 DY(1+(I-1)*INCY) SI INCY.GE.0  OU
C                 DY(1+(I-N)*INCY) SI INCY.LT.0.
C IN  : C    : COEFFICIENT DE ROTATION.
C     : S    : COEFFICIENT DE ROTATION.
C-----------------------------------------------------------------------
      INTEGER    I, IX, IY
      REAL *8 DTEMP
C
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C
C                 INCREMENTS NON EGAUX A 1
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               DTEMP = C*DX(IX) + S*DY(IY)
               DY(IY) = C*DY(IY) - S*DX(IX)
               DX(IX) = DTEMP
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                   INCREMENTS EGAUX A 1
            DO 20  I=1, N
               DTEMP = C*DX(I) + S*DY(I)
               DY(I) = C*DY(I) - S*DX(I)
               DX(I) = DTEMP
   20       CONTINUE
         END IF
      END IF
 9999 CONTINUE
      END
