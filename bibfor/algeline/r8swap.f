      SUBROUTINE R8SWAP (N, DX, INCX, DY, INCY)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, INCX, INCY
      REAL *8 DX(*), DY(*)
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
C            COPIE LE VECTEUR X DANS LE VECTEUR Y ET Y DANS X.
C-----------------------------------------------------------------------
C IN  : N    : LONGUEUR DES VECTEURS X EY Y
C I/O : DX   : VECTEUR DE LONGUEUR  MAX(N*IABS(INCX),1).
C IN  : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE DX.
C              X(I) EST DEFINI PAR
C                 DX(1+(I-1)*INCX) SI INCX.GE.0  OU
C                 DX(1+(I-N)*INCX) SI INCX.LT.0.
C I/O : DY   : VECTEUR DE LONGUEUR MAX(N*IABS(INCY),1).
C     INCY   - DEPLACEMENT ENTRE LES ELEMENTS DE DY.
C              Y(I) EST DEFINI PAR
C                 DY(1+(I-1)*INCY) SI INCY.GE.0  OU
C                 DY(1+(I-N)*INCY) SI INCY.LT.0.
C-----------------------------------------------------------------------
      INTEGER    I, IX, IY, M, MP1
      REAL *8 DTEMP
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                   POUR DES INCREMENTS NON EGAUX OU POUR DES
C                   INCREMENTS EGAUX MAIS NON EGAUX A 1
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               DTEMP = DX(IX)
               DX(IX) = DY(IY)
               DY(IY) = DTEMP
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                   INCREMENTS EGAUX A 1
            M = MOD(N,3)
C
            DO 30  I=1, M
               DTEMP = DX(I)
               DX(I) = DY(I)
               DY(I) = DTEMP
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 3
               DTEMP = DX(I)
               DX(I) = DY(I)
               DY(I) = DTEMP
               DTEMP = DX(I+1)
               DX(I+1) = DY(I+1)
               DY(I+1) = DTEMP
               DTEMP = DX(I+2)
               DX(I+2) = DY(I+2)
               DY(I+2) = DTEMP
   40       CONTINUE
         END IF
      END IF
 9999 CONTINUE
      END
