      FUNCTION ZDOTC (N, ZX, INCX, ZY, INCY)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, INCX, INCY
      COMPLEX    *16 ZX(*), ZY(*),ZDOTC
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/01/96   AUTEUR VABHHTS J.PELLET 
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
C                  CALCUL DU PRODUIT CONJG(X)*Y
C-----------------------------------------------------------------------
C IN  : N    : LONGUEUR DES VECTEURS X ET Y.
C     : ZX   : VECTEUR DE LONGUEUR MAX(N*IABS(INCX),1).
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE ZX.
C              X(I) EST DEFINI PAR
C                 ZX(1+(I-1)*INCX) SI INCX.GE.0  OU
C                 ZX(1+(I-N)*INCX) SI INCX.LT.0.
C     : ZY   : VECTEUR DE LONGUEUR MAX(N*IABS(INCY),1).
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE ZY.
C              Y(I) EST DEFINI PAR
C                 ZY(1+(I-1)*INCY) SI INCY.GE.0  OU
C                 ZY(1+(I-N)*INCY) SI INCY.LT.0.
C OUT : ZDOTC: SOMME POUR I=1 A N DE CONJ(X(I))*Y(I).
C-----------------------------------------------------------------------
      INTEGER    I, IX, IY
      COMPLEX    *16 CONJG
C
      ZDOTC = (0.0D0,0.0D0)
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C
C
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               ZDOTC = ZDOTC + DCONJG(ZX(IX))*ZY(IY)
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C
            DO 20  I=1, N
               ZDOTC = ZDOTC + DCONJG(ZX(I))*ZY(I)
   20       CONTINUE
         END IF
      END IF
 9999 CONTINUE
      END
