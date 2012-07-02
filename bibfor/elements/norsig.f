      FUNCTION NORSIG(SIGMA,NBSIG)
      IMPLICIT NONE
      REAL*8 NORSIG
      REAL*8 SIGMA(NBSIG)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C     BUT           : CALCUL DE LA NORME DU TENSEUR DE CONTRAINTES
C                     SIGMA AU SENS SUIVANT :
C                     NORSIG = SIGMA(I,J)*SIGMA(I,J)
C IN  SIGMA(NBSIG)  : VECTEUR DES COMPOSANTES DU TENSEUR DE CONTRAINTES
C IN  NBSIG         : NOMBRE DE CONTRAINTES POUR UN TYPE D'ELEMENT
C                     DONNE
C-----------------------------------------------------------------------
      REAL*8 NORSI2
C
C-----------------------------------------------------------------------
      INTEGER I ,NBSIG 
      REAL*8 DEUX ,ZERO 
C-----------------------------------------------------------------------
      ZERO   = 0.0D0
      DEUX   = 2.0D0
      NORSIG = ZERO
      NORSI2 = ZERO
C
      DO 10 I = 1, 3
         NORSI2 = NORSI2 + SIGMA(I)*SIGMA(I)
 10   CONTINUE
C
      DO 20 I = 4, NBSIG
         NORSI2 = NORSI2 + DEUX*SIGMA(I)*SIGMA(I)
 20   CONTINUE
C
      NORSIG = SQRT(NORSI2)
C
      END
