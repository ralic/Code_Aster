      SUBROUTINE VEOBST ( ARETE, RCARTE, ANGDEB, ANGFIN, ANGVA, ANGARE,
     &                    ANGMAX, PROFON, VOLUME, EPAIS )
      IMPLICIT   NONE
      REAL*8              ARETE, RCARTE, ANGDEB, ANGFIN, ANGVA, ANGARE,
     &                    ANGMAX, PROFON, VOLUME, EPAIS
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/10/2000   AUTEUR CIBHHLV L.VIVAN 
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
      REAL*8   DELTA, TAU, ANG1, ANG2
      REAL*8   THETA1, THETA2, THETA3, FONC1, FONC2, FONC3
      REAL*8   A1, A2, A3, A, D
      REAL*8   RAD, R8DGRD, DEG, R8RDDG
C-----------------------------------------------------------------------
C
      RAD = R8DGRD( )
      DEG = R8RDDG( )
C
      D = RCARTE * SIN(ARETE*RAD)
      TAU    = 0.5D0
      THETA1 = 25.D0
      THETA2 = ARETE-1.D-5
      ANGMAX = ANGARE
      PROFON = VOLUME*(1.D0+TAU)/EPAIS*ANGVA/0.7D-3*RAD        
      DELTA  = PROFON / ( RCARTE*TAN(PROFON*ANGVA/0.7D-3*RAD) )
      ANG2   = ARETE + DELTA*DEG
C*********************************************************
C      RESOLUTION DE L'EQUATION S*E=V_USE
C      ON PROCEDE PAR DICHOTOMIE
C
C      L'EQUATION DE LA DROITE EST R(THETA)=A*(THETA-ANG2)+RCARTE
C*********************************************************
      A1 = 1.D0 / ( (THETA1-ANG2)*RAD)*(D/SIN(THETA1*RAD)-RCARTE)
      FONC1 = 1.D0 / (6*A1)*RCARTE**3-((ANG2-ARETE)*RAD)/2
     &               *RCARTE**2 + D**2/2
     &               *( 1.D0/TAN(ARETE*RAD)-1.D0/TAN(THETA1*RAD) )
     &               -1.D0/(6*A1)*(A1*(THETA1-ANG2)*RAD+RCARTE)**3
     &               -VOLUME/EPAIS
C
      A2 = 1.D0 / ((THETA2-ANG2)*RAD)*(D/SIN(THETA2*RAD)-RCARTE)
      FONC2 = 1.D0 / (6*A2)*RCARTE**3-((ANG2-ARETE)*RAD)/2
     &               *RCARTE**2 - D**2/2
     &               *( 1.D0/TAN(ARETE*RAD)-1.D0/TAN(THETA2*RAD) )
     &               -1.D0/(6*A2)*(A2*(THETA2-ANG2)*RAD+RCARTE)**3
     &               -VOLUME/EPAIS
C
      IF ( FONC1 .EQ. 0.D0 ) THEN
         ANG1 = THETA1
         GOTO 20
      ENDIF
C
      IF ( FONC2 .EQ. 0.D0 ) THEN
         ANG1 = THETA2
         GOTO 20
      ENDIF
C
      IF ( FONC2*FONC1 .GT. 0.D0 ) THEN
         ANG1 = ARETE
         GOTO 20
      ENDIF
C
      IF ( FONC2*FONC1 .LT. 0.D0 ) THEN
10       CONTINUE
         IF ( (THETA2-THETA1) .LT. 1.D-5 ) THEN
            ANG1 = THETA2
            GOTO 20
         ENDIF
         THETA3 = ( THETA1 + THETA2 ) / 2
         A3 = 1.D0 / ( (THETA3-ANG2)*RAD)
     &             * ( D / SIN(THETA3*RAD) - RCARTE )        
         FONC3 = 1.D0 / (6.D0*A3)*RCARTE**3-((ANG2-ARETE)*RAD)/2
     &               *RCARTE**2 - D**2/2
     &               *( 1.D0/TAN(ARETE*RAD)-1.D0/TAN(THETA3*RAD) )
     &               -1.D0/(6.D0*A3)*(A3*(THETA3-ANG2)*RAD+RCARTE)**3
     &               -VOLUME/EPAIS
C
         IF ( FONC3 .EQ. 0.D0 ) THEN
            ANG1 = THETA3
            GOTO 20
         ENDIF
         IF ( FONC1*FONC3 .LT. 0.D0 ) THEN
            THETA2 = THETA3
            FONC2  = FONC3
            GOTO 10
         ENDIF
         IF ( FONC2*FONC3 .LT. 0.D0 ) THEN
            THETA1 = THETA3
            FONC1  = FONC3
            GOTO 10
         ENDIF
      ENDIF
20    CONTINUE
C
      IF ( ANGMAX .LT. 90.D0 ) THEN
         ANGFIN = ANG2
         ANGDEB = ANG1
      ENDIF
      IF ( ANGMAX .GT. 270.D0 ) THEN
         ANGDEB = 360.D0 - ANG2
         ANGFIN = 360.D0 - ANG1
      ENDIF
      IF ( (ANGMAX.GT.90.D0) .AND. (ANGMAX.LT.180.D0) ) THEN
         ANGDEB = 180.D0 - ANG2
         ANGFIN = 180.D0 - ANG1
      ENDIF
      IF ( (ANGMAX.GT.180.D0) .AND. (ANGMAX.LT.270.D0) ) THEN
         ANGFIN = 180.D0 + ANG2
         ANGDEB = 180.D0 + ANG1
      ENDIF
C
      A = 1.D0 / ((ANG1-ANG2)*RAD)*(D/SIN(ANG1*RAD)-RCARTE)
      PROFON = ABS( A*(ANG2-ARETE)*RAD )
C
      END
