      INTEGER FUNCTION IZAMAX (N, ZX, INCX)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, INCX
      COMPLEX    *16 ZX(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C   RECHERCHE DE L'INDICE DU NOMBRE COMPLEXE DE PLUS GRAND MODULE DANS
C                          LE VECTEUR.
C-----------------------------------------------------------------------
C IN  : N    : LONGUEUR DU VECTEUR X.
C     : ZX   : VECTEUR DE LONGUEUR N*INCX.
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE ZX.
C              X(I) EST DEFINI PARE ZX(1+(I-1)*INCX). INCX DOIT ETRE
C              SUPERIEURE A ZERO.
C OUT :IZAMAX: INDICE DU NOMBRE DE PLUS GRAND MODULE
C-----------------------------------------------------------------------
      INTEGER    I, IX
      REAL *8 SMAX
      COMPLEX    *16 ZDUM
      REAL *8 CABS1
C
      CABS1(ZDUM) = ABS(DBLE(ZDUM)) + ABS(DIMAG(ZDUM))
C
      IZAMAX = 0
      IF (N .GE. 1) THEN
         IZAMAX = 1
         IF (N .NE. 1) THEN
            IF (INCX .NE. 1) THEN
C                                   INCREMENTS NON EGAUX A 1
               IX = 1
               IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
               SMAX = CABS1(ZX(IX))
               IX = IX + INCX
               DO 10  I=2, N
                  IF (CABS1(ZX(IX)) .GT. SMAX) THEN
                     IZAMAX = I
                     SMAX = CABS1(ZX(IX))
                  END IF
                  IX = IX + INCX
   10          CONTINUE
            ELSE
C                                   INCREMENTS EGAUX A 1
               SMAX = CABS1(ZX(1))
               DO 20  I=2, N
                  IF (CABS1(ZX(I)) .GT. SMAX) THEN
                     IZAMAX = I
                     SMAX = CABS1(ZX(I))
                  END IF
   20          CONTINUE
            END IF
         END IF
      END IF
 9999 CONTINUE
      END
