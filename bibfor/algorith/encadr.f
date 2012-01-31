      SUBROUTINE ENCADR (F,X1,X2,F1,F2,NITER,XMULT,IRET)
      IMPLICIT NONE
C
      REAL*8  F,X1,X2,F1,F2,XMULT
      INTEGER NITER,IRET
C
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/01/2012   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C IN      F       : FONCTION F
C IN      PREC    : PRECISION ABSOLUE :
C                   LA SOLUTION EST TELLE QUE F(X)<PREC
C IN      NITER   : NOMBRE D'ITERATIONS MAXIMUM 
C IN/OUT  X1      : BORNE A GAUCHE TROUVEE
C IN/OUT  X2      : BORNE A DROITE TROUVEE
C OUT     F1      : VALEUR DE F EN X1
C OUT     F2      : VALEUR DE F EN X2
C OUT     IRET    : CODE RETOUR : IRET = 0 : OK
C                                 IRET = 1 : PB
C
C ----------------------------------------------------------------------
C
      INTEGER I
C
      IRET = 1

      IF (X1.EQ.X2) GO TO 9999
C
      F1 = F(X1)
      F2 = F(X2)
      DO 10 I = 1,NITER
         IF (F1*F2.LT.0.D0) THEN
            IRET = 0
            GO TO 9999
         END IF
         IF (ABS(F1).LT.ABS(F2)) THEN
            X1 = X1 + XMULT*(X1-X2)
            F1 = F(X1)
         ELSE
            X2 = X2 + XMULT*(X2-X1)
            F2 = F(X2)
         END IF
   10 CONTINUE
C
C ----------------------------------------------------------------------
C
 9999 CONTINUE
      END
