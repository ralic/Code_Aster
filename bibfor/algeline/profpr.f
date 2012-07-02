      SUBROUTINE PROFPR(ICOQ,RKI,R1,R2,COEPR1,COEPR2,WPR)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
C CALCUL DE COEFFICIENTS PONDERATEURS QUAND ON PREND EN COMPTE UN PROFIL
C RADIAL NON UNIFORME POUR LA PRESSION
C APPELANT : BIJSOM, PBFLUI
C-----------------------------------------------------------------------
C  IN : ICOQ   : INDICE CARACTERISANT LA COQUE EN MOUVEMENT POUR LE MODE
C                CONSIDERE (ICOQ=1 COQUE INTERNE  ICOQ=2 COQUE EXTERNE)
C  IN : RKI    : ORDRE DE COQUE DU MODE CONSIDERE
C  IN : R1     : RAYON REPERANT LA SURFACE DE LA STRUCTURE INTERNE
C  IN : R2     : RAYON REPERANT LA SURFACE DE LA STRUCTURE EXTERNE
C OUT : COEPR1 : COEFFICIENT PONDERATEUR POUR LA PRESSION EN R1
C OUT : COEPR2 : COEFFICIENT PONDERATEUR POUR LA PRESSION EN R2
C OUT : WPR    : VALEUR MOYENNE DU PROFIL DE PRESSION
C-----------------------------------------------------------------------
      INTEGER      ICOQ
      REAL*8       RKI,R1,R2,COEPR1,COEPR2,WPR
C-----------------------------------------------------------------------
C
C --- 1.INITIALISATIONS
C
C-----------------------------------------------------------------------
      INTEGER KI 
      REAL*8 A ,H0 ,R0 ,RC ,T ,X ,Y 
      REAL*8 Z ,Z1 ,Z2 
C-----------------------------------------------------------------------
      R0 = (R1+R2)/2.D0
      H0 = R2 - R1
C
      RC = R2
      IF (ICOQ.EQ.2) RC = R1
C
      KI = INT(RKI)
C
C --- 2.CALCUL DES COEFFICIENTS PONDERATEURS
C
      IF (KI.EQ.1) THEN
        X = R0 + RC*RC*DBLE(LOG(R2/R1))/H0
        A = 1.D0/X
      ELSE
        X = R2**(RKI+1.D0) - R1**(RKI+1.D0)
        X = X/(RKI+1.D0)
        Y = RC**(RKI)
        Y = Y*Y/(RKI-1.D0)
        Z1= R1**(RKI-1.D0)
        Z2= R2**(RKI-1.D0)
        Z = 1.D0/Z1 - 1.D0/Z2
        A = H0/(X+Y*Z)
      ENDIF
C
      IF (ICOQ.EQ.1) THEN
        T = (R2/R1)**(RKI)
        COEPR1 = A*(1.D0+T*T)*R1**(RKI)
        COEPR2 = 2.D0*A*R2**(RKI)
      ELSE
        T = (R1/R2)**(RKI)
        COEPR1 = 2.D0*A*R1**(RKI)
        COEPR2 = A*(1.D0+T*T)*R2**(RKI)
      ENDIF
C
C --- 3.CALCUL DE LA VALEUR MOYENNE
C
      IF (KI.EQ.2) THEN
        X = (R1*R1+R2*R2)/2.D0
        Y = RC*RC
        Y = Y*Y*DBLE(LOG(R2/R1))/(R0*H0)
        WPR = A*(X+Y)
      ELSE
        X = R2**(RKI+2.D0) - R1**(RKI+2.D0)
        X = X/(RKI+2.D0)
        Y = RC**(RKI)
        Y = Y*Y/(RKI-2.D0)
        Z1= R1**(RKI-2.D0)
        Z2= R2**(RKI-2.D0)
        Z = 1.D0/Z1 - 1.D0/Z2
        WPR = A*(X+Y*Z)/(R0*H0)
      ENDIF
C
      END
