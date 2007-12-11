      SUBROUTINE ENCADR (FONCT,X1,X2,F1,F2,NITER,SUCCES)
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 FONCT,X1,X2
      INTEGER NITER,SUCCES
      PARAMETER (XMULT = 1.6D0)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C
C     DETERMINATION D'UN ENCADREMENT DU ZERO D'UNE FONCTION.
C ----------------------------------------------------------------------
C
      SUCCES = 1
      IF (X1.EQ.X2) THEN
         SUCCES = 0
         GO TO 9999
      END IF
C
      F1 = FONCT(X1)
      F2 = FONCT(X2)
      DO 10 ITER = 1,NITER
         IF (F1*F2.LT.0.D0) THEN
            GO TO 9999
         END IF
         IF (ABS(F1).LT.ABS(F2)) THEN
            X1 = X1 + XMULT*(X1-X2)
            F1 = FONCT(X1)
         ELSE
            X2 = X2 + XMULT*(X2-X1)
            F2 = FONCT(X2)
         END IF
   10 CONTINUE
      SUCCES = 0
C
C ----------------------------------------------------------------------
C
 9999 CONTINUE
      END
