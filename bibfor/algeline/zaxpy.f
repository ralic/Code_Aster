      SUBROUTINE ZAXPY (N, ZA, ZX, INCX, ZY, INCY)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, INCX, INCY
      COMPLEX    *16 ZA, ZX(*), ZY(*)
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
C               CALCUL DE   Y = AX + Y
C-----------------------------------------------------------------------
C IN  : N    : LONGUEUR DES VECTEURS X ET Y.
C     : ZA   : SCALAIRE.
C     : ZX   : VECTEUR DE LONGUEUR MAX(N*IABS(INCX),1).
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE ZX.
C              X(I) EST DEFINI PAR
C                 ZX(1+(I-1)*INCX) SI INCX.GE.0  OU
C                 ZX(1+(I-N)*INCX) SI INCX.LT.0.
C I/O : ZY   : VECTEUR DE LONGUEUR MAX(N*IABS(INCY),1).
C              ZAXPY REMPLACE Y(I) AVEC ZA*X(I) + Y(I) POUR I = 1,..N.
C              X(I) ET Y(I) SE REFERENT AUX ELEMENTS DE ZX ET ZY.
C IN  : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE ZY.
C              Y(I) EST DEFINI PAR
C                 ZY(1+(I-1)*INCY) SI INCY.GE.0  OU
C                 ZY(1+(I-N)*INCY) SI INCY.LT.0.
C-----------------------------------------------------------------------
      INTEGER    I, IX, IY
      REAL *8 ZCABS
C
      IF (N .GT. 0) THEN
         ZCABS = ABS(DBLE(ZA)) + ABS(DIMAG(ZA))
         IF (ZCABS .NE. 0.0D0) THEN
            IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C             POUR DES INCREMENTS NON EGAUX OU DES INCREMENTS EGAUX
C             MAIS NON EGAUX A 1
               IX = 1
               IY = 1
               IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
               IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
               DO 10  I=1, N
                  ZY(IY) = ZY(IY) + ZA*ZX(IX)
                  IX = IX + INCX
                  IY = IY + INCY
   10          CONTINUE
            ELSE
C                                   INCREMENTS EGAUX A 1
               DO 20  I=1, N
                  ZY(I) = ZY(I) + ZA*ZX(I)
   20          CONTINUE
            END IF
         END IF
      END IF
 9999 CONTINUE
      END
