      INTEGER FUNCTION IDAMAX (N, DX, INCX)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, INCX
      REAL *8 DX(*)
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
C      RECHERCHE DE L'INDICE DE LA VALEUR ABSOLUE MAX. DANS LE VECTEUR.
C-----------------------------------------------------------------------
C IN  : N    : LONGUEUR DU VECTEUR X.
C     : DX   : VECTEUR DE LONGUEUR N*INCX.
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE DX.
C              X(I) EST DEFINI PAR DX(1+(I-1)*INCX). INCX DOIT ETRE
C              SUPERIEURE A ZERO.
C OUT :IDAMAX: INDICE DE LA PLUS GRANDE VALEUR ABSOLUE.
C-----------------------------------------------------------------------
      INTEGER    I, II, NS
      REAL *8 DMAX, XMAG
C
      IDAMAX = 0
      IF (N .GE. 1) THEN
         IDAMAX = 1
         IF (N .GT. 1) THEN
            IF (INCX .NE. 1) THEN
C
               DMAX = ABS(DX(1))
               NS = N*INCX
               II = 1
               DO 10  I=1, NS, INCX
                  XMAG = ABS(DX(I))
                  IF (XMAG .GT. DMAX) THEN
                     IDAMAX = II
                     DMAX = XMAG
                  END IF
                  II = II + 1
   10          CONTINUE
            ELSE
C
               DMAX = ABS(DX(1))
               DO 20  I=2, N
                  XMAG = ABS(DX(I))
                  IF (XMAG .GT. DMAX) THEN
                     IDAMAX = I
                     DMAX = XMAG
                  END IF
   20          CONTINUE
            END IF
         END IF
      END IF
 9999 CONTINUE
      END
