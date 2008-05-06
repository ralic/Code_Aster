        SUBROUTINE HUJELA (MOD, CRIT, MATER, DEPS, SIGD, SIGF, 
     &                     EPSD, IRET)
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
C       ---------------------------------------------------------------
C       INTEGRATION ELASTIQUE NON LINEAIRE DE LA LOI DE HUJEUX
C       IN  MOD    :  MODELISATION
C           CRIT   :  CRITERES DE CONVERGENCE
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           SIGD   :  CONTRAINTE  A T
C           DEPS   :  INCREMENT DE DEFORMATION
C           EPSD   :  DEFORMATION TOTALE A T
C       OUT SIGF   :  CONTRAINTE A T+DT
C           IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
C                         IRET=0 => PAS DE PROBLEME
C                         IRET=1 => ECHEC 
C       ---------------------------------------------------------------
        INTEGER   NDT, NDI, IRET, I, J
        REAL*8    COEF, E, NU, AL, DEMU, I1, N, PREF
        REAL*8    DEPS(6), DSIG(6), SIGD(6), SIGF(6)
        REAL*8    HOOK(6,6), MATER(22,2), CRIT(*), EPSD(6)
        REAL*8    ZERO, UN, D13, DEUX, LA, EPSV, I1E
        REAL*8    E1,E2,E3,NU12,NU13,NU23,G1,G2,G3,NU21,NU31,NU32,DELTA
        REAL*8    PISO, TOLE, R8PREM, C11, C12, C13, C22, C23, C33
        CHARACTER*8 MOD
        LOGICAL     TRACT

        COMMON /TDIM/     NDT, NDI

        DATA      ZERO  / 0.D0 /
        DATA      UN    / 1.D0 /
        DATA      D13   / 0.33333333333334D0 /
        DATA      DEUX  / 2.D0 /
        DATA      TOLE  / 1.D-6 /

C       ---------------------------------------------------------------
        PREF = MATER(8,2)
        PISO = 1.5D0*MATER(21,2)
        N    = MATER(1,2)
        I1E  = D13*(SIGD(1)+SIGD(2)+ SIGD(3))
        EPSV = DEPS(1)+DEPS(2)+DEPS(3)
        
        IF (ABS(N).LT.R8PREM()) THEN
                  
          IF (MATER(17,1).EQ.UN) THEN
          
            I1 = I1E + D13*MATER(1,1)/(UN-DEUX*MATER(2,1))*EPSV

          ELSEIF (MATER(17,1).EQ.DEUX) THEN
      
            E1   = MATER(1,1)
            E2   = MATER(2,1)
            E3   = MATER(3,1)
            NU12 = MATER(4,1)
            NU13 = MATER(5,1)
            NU23 = MATER(6,1)
            NU21 = MATER(10,1)
            NU31 = MATER(11,1)
            NU32 = MATER(12,1)
            DELTA= MATER(13,1)
            
            C11 = (UN - NU23*NU32)*E1/DELTA
            C12 = (NU21 + NU31*NU23)*E1/DELTA
            C13 = (NU31 + NU21*NU32)*E1/DELTA
            C22 = (UN - NU13*NU31)*E2/DELTA
            C23 = (NU32 + NU31*NU12)*E2/DELTA
            C33 = (UN - NU21*NU12)*E3/DELTA
            
            I1 = (C11+C12+C13)*DEPS(1) + (C12+C22+C23)*DEPS(2)
     &           + (C13+C23+C33)*DEPS(3)
            I1 = I1E + D13*I1
        
          ELSE
            CALL U2MESS('F', 'COMPOR1_40')
          ENDIF

          IF ((I1 -PISO)/PREF .LT. TOLE) THEN
            TRACT=.TRUE.
            GOTO 5
          ENDIF
          GOTO 30
          
        ENDIF


C--->  CALCUL DE I1=TR(SIG) A T+DT PAR METHODE DE LA SECANTE
C      OU EXPLICITEMENT SI NIVEAU HUJEUX
        CALL HUJCI1 (CRIT, MATER, DEPS, SIGD, I1, TRACT, IRET)
        IF (IRET.EQ.1) GOTO 9999
                
        IF (MATER(17,1).EQ.UN) THEN
          
          I1E  = I1E + D13*MATER(1,1)/(UN-DEUX*MATER(2,1))*EPSV*
     &                 ((I1E -PISO)/PREF)**N

        ELSEIF (MATER(17,1).EQ.DEUX) THEN
      
          E1   = MATER(1,1)*((I1E -PISO)/PREF)**N
          E2   = MATER(2,1)*((I1E -PISO)/PREF)**N
          E3   = MATER(3,1)*((I1E -PISO)/PREF)**N
          NU12 = MATER(4,1)
          NU13 = MATER(5,1)
          NU23 = MATER(6,1)
          NU21 = MATER(13,1)
          NU31 = MATER(14,1)
          NU32 = MATER(15,1)
          DELTA= MATER(16,1)
          
          C11 = (UN - NU23*NU32)*E1/DELTA
          C12 = (NU21 + NU31*NU23)*E1/DELTA
          C13 = (NU31 + NU21*NU32)*E1/DELTA
          C22 = (UN - NU13*NU31)*E2/DELTA
          C23 = (NU32 + NU31*NU12)*E2/DELTA
          C33 = (UN - NU21*NU12)*E3/DELTA
          
          COEF= (C11+C12+C13)*DEPS(1) + (C12+C22+C23)*DEPS(2)
     &          + (C13+C23+C33)*DEPS(3)
          I1E = I1E + D13*COEF
        
        ELSE
          CALL U2MESS('F', 'COMPOR1_40')
        ENDIF
     
        IF ((I1E -PISO)/PREF .LT. TOLE) I1E = I1
        I1 = (I1+I1E) /DEUX
        IF ((I1  -PISO)/PREF .LT. TOLE) TRACT=.TRUE.

C--->  EN CAS D'ENTREE EN TRACTION, LES CONTRAINTES SONT
C      RAMENEES SUR L'AXE HYDROSTATIQUE A DES VALEURS FAIBLES
C      ( EGALES A PA/100.0 SOIT -1 KPA )
C

CKH A REVOIR....
  5     CONTINUE
        IF (TRACT) THEN
          DO 10 I = 1, NDI
            SIGF(I) = PREF*TOLE
 10         CONTINUE
          DO 20 I = NDI+1, NDT
            SIGF(I) = ZERO
 20         CONTINUE
          GOTO 9999
        ENDIF
 30     CONTINUE
 
 
C---> CALCUL DU COEF  (-----------)**N ET MODULE_YOUNG A T+DT
        CALL LCINMA (ZERO, HOOK)
        COEF = ((I1 -PISO)/PREF)**N
        
        IF (MOD(1:2) .EQ. '3D'     .OR.
     &    MOD(1:6) .EQ. 'D_PLAN' .OR.
     &    MOD(1:4) .EQ. 'AXIS')  THEN
     
        IF (MATER(17,1).EQ.UN) THEN
      
          E    = MATER(1,1)*COEF
          NU   = MATER(2,1)
          AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
          DEMU = E     /(UN+NU)
          LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

          DO 32 I = 1, NDI
            DO 32 J = 1, NDI
              IF (I.EQ.J) HOOK(I,J) = AL
              IF (I.NE.J) HOOK(I,J) = LA
 32           CONTINUE
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
          CALL U2MESS('F', 'COMPOR1_40')
        ENDIF

      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN
     
        CALL U2MESS('F', 'COMPOR1_4')
     
      ENDIF


C--->   INCREMENTATION DES CONTRAINTES  SIGF = SIGD + HOOK DEPS
        CALL LCPRMV (HOOK, DEPS, DSIG)
        CALL LCSOVE (SIGD, DSIG, SIGF)

 9999   CONTINUE
        END
