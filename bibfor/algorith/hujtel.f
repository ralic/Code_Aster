      SUBROUTINE HUJTEL (MOD, MATER, SIG, HOOK)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/05/2008   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C  ---------------------------------------------------------------
C  CALCUL DE LA MATRICE DE RIGIDITE ELASTIQUE DE LA LOI HUJEUX
C  IN   MOD   :  MODELISATION
C       MATER :  COEFFICIENTS MATERIAU
C       SIG   :  CONTRAINTES
C  OUT  HOOK  :  OPERATEUR RIGIDITE ELASTIQUE
C  ---------------------------------------------------------------
      INTEGER  NDT, NDI, I, J
      REAL*8   SIG(6), HOOK(6,6), MATER(22,2), I1, COEF
      REAL*8   E, NU, AL, DEMU, LA
      REAL*8   UN, D13, ZERO, DEUX
      REAL*8   E1,E2,E3,NU12,NU13,NU23,G1,G2,G3,NU21,NU31,NU32,DELTA
      REAL*8   PISO
      CHARACTER*8 MOD

      COMMON /TDIM/     NDT, NDI

      DATA   D13   / .333333333334D0 /
      DATA   UN    / 1.D0 /
      DATA   ZERO  / 0.D0 /
      DATA   DEUX  / 2.D0 /
      
      PISO  = 1.5D0*MATER(21,2)


C ----------------------------------------------------------------
C --->   CALCUL PREMIER INVARIANT DES CONTRAINTES
      I1 = ZERO
      DO 10 I = 1, NDI
        I1 = I1 + D13*SIG(I)
  10  CONTINUE


C --->   CALCUL DES COEF. UTILES
      IF (I1 .EQ. ZERO) THEN
         COEF = UN
      ELSE
         COEF = ((I1 -PISO)/MATER(8,2)) **MATER(1,2)
      ENDIF      
      
      IF (MOD(1:2) .EQ. '3D'     .OR.
     &    MOD(1:6) .EQ. 'D_PLAN' .OR.
     &    MOD(1:4) .EQ. 'AXIS')  THEN
     
        IF (MATER(17,1).EQ.UN) THEN
      
          E    = MATER(1,1)*COEF
          NU   = MATER(2,1)
          AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
          DEMU = E     /(UN+NU)
          LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

          DO 30 I = 1, NDI
            DO 30 J = 1, NDI
              IF (I.EQ.J) HOOK(I,J) = AL
              IF (I.NE.J) HOOK(I,J) = LA
 30           CONTINUE
          DO 35 I = NDI+1, NDT
            HOOK(I,I) = DEMU
 35         CONTINUE
 
        ELSEIF (MATER(17,1).EQ.DEUX) THEN
      
          E1   = MATER(1,1)*COEF
          E2   = MATER(2,1)*COEF
          E3   = MATER(3,1)*COEF
          NU12 = MATER(4,1)
          NU13 = MATER(5,1)
          NU23 = MATER(6,1)
          G1   = MATER(7,1)*COEF
          G2   = MATER(8,1)*COEF
          G3   = MATER(9,1)*COEF
          NU21 = MATER(13,1)
          NU31 = MATER(14,1)
          NU32 = MATER(15,1)
          DELTA= MATER(16,1)
         
          HOOK(1,1) = (UN - NU23*NU32)*E1/DELTA
          HOOK(1,2) = (NU21 + NU31*NU23)*E1/DELTA
          HOOK(1,3) = (NU31 + NU21*NU32)*E1/DELTA
          HOOK(2,2) = (UN - NU13*NU31)*E2/DELTA
          HOOK(2,3) = (NU32 + NU31*NU12)*E2/DELTA
          HOOK(3,3) = (UN - NU21*NU12)*E3/DELTA
          HOOK(2,1) = HOOK(1,2)
          HOOK(3,1) = HOOK(1,3)
          HOOK(3,2) = HOOK(2,3)
          HOOK(4,4) = G1
          HOOK(5,5) = G2
          HOOK(6,6) = G3
        
        ELSE
          CALL U2MESS('F', 'COMPOR1_36')
        ENDIF

      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN
     
        CALL U2MESS('F', 'COMPOR1_4')
     
      ENDIF

      END
