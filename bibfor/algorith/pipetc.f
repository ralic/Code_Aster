      SUBROUTINE PIPETC(MAT,SUP,SUD,MUP,MUD,VIM,TAU,COPILO)
            
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/05/2010   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      INTEGER  MAT
      REAL*8   SUP(3),SUD(3),MUP(3),MUD(3),TAU,VIM(*),COPILO(5)
      
C ----------------------------------------------------------------------
C     PILOTAGE PRED_ELAS POUR LA LOI D'INTERFACE CZM_TAC_MIX
C
C IN  MAT    : MATERIAU
C IN  SUP    : SAUT DU AUX CHARGES FIXES
C IN  SUD    : SAUT DU AUX CHARGES PILOTEES
C IN  MUP    : MULTIPLICATEUR DU AUX CHARGES FIXES
C IN  MUD    : MULTIPLICATEUR DU AUX CHARGES PILOTEES
C IN  VIM    : VARIABLES INTERNES EN T-
C IN  TAU    : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
C OUT COPILO : COEFFICIENTS DU TIR ELASTIQUE LINEARISE AUTOUR DES SOL.
C                FEL = COPILO(1) + COPILO(2)*ETA
C                FEL = COPILO(3) + COPILO(4)*ETA
C                COPILO(5) <> R8VIDE => PAS DE SOLUTION
C ----------------------------------------------------------------------
      INTEGER NRAC,I
      REAL*8  SC,GC,DC,H,R,KA,GA,SK,VAL(3),TMP
      REAL*8  TT2,TP(3),TD(3),TPN,TDN,C0,C1,C2,N0,N1,N2,RAC(4),ETA,PENTE
      REAL*8  R8VIDE
      CHARACTER*2 COD(3)
      CHARACTER*8 NOM(3)
      
      DATA NOM /'GC','SIGM_C','PENA_LAGR'/
C ----------------------------------------------------------------------

C -- SEUIL EN ENERGIE DISSIPE
      GA = VIM(4) + TAU

C -- CAS DE L'ENDOMMAGEMENT SATURE
      IF (GA .GT. 1.D0) GOTO 9999

C -- RECUPERATION DES PARAMETRES PHYSIQUES

      CALL RCVALA(MAT,' ','RUPT_FRAG',0,' ',0.D0,3,NOM,VAL,COD,'F ')

      GC   = VAL(1)      
      SC   = VAL(2)  
      DC   = 2.D0*GC/SC
      H    = SC/DC
      R    = H * VAL(3)
      
C    CALCUL DE KAPPA : KA = DC*(1-SQRT(1-GA)) : SEUIL EN SAUT
      TMP = SQRT(MAX(0.D0,1.D0-GA))
      TMP = DC*(1.D0-TMP)
      TMP = MAX(0.D0,TMP)
      TMP = MIN(DC,TMP)
      KA  = TMP
      SK  = MAX(0.D0,SC - H*KA)

C   CALCUL DU SEUIL (EN CONTRAINTE)
      TT2 = (R*KA + SK)**2.D0
     
C -- CALCUL DU SECOND MEMBRE 
       
C    FORCE COHESIVE AUGMENTEE       
      TP(1) = MUP(1) + R*SUP(1)
      TP(2) = MUP(2) + R*SUP(2)
      TP(3) = MUP(3) + R*SUP(3)

C    FORCE COHESIVE AUGMENTEE       
      TD(1) = MUD(1) + R*SUD(1)
      TD(2) = MUD(2) + R*SUD(2)
      TD(3) = MUD(3) + R*SUD(3)

      TPN = TP(1)
      TDN = TD(1)


C -- CALCUL DES FORMES QUADRATIQUES 

C    CISAILLEMENT : C0 + 2.C1 ETA + C2 ETA**2
C    OUVERTURE    : N0 + 2.N1 ETA + N2 ETA**2

      C0 = TP(2)*TP(2) + TP(3)*TP(3)
      C1 = TP(2)*TD(2) + TP(3)*TD(3)
      C2 = TD(2)*TD(2) + TD(3)*TD(3)

      N0 = TPN*TPN
      N1 = TPN*TDN
      N2 = TDN*TDN

C    SI LE POINT N'EST PAS PILOTABLE :
      IF (C2+N2.EQ.0.D0) GOTO 9999
          
  
C -- RESOLUTION DES EQUATIONS

C             C(ETA) = TT2
C    N(ETA) + C(ETA) = TT2        

C    INITIALISATION
      CALL R8INIR(4,R8VIDE(),RAC,1)
      
C    EQUATION SUR LE CISAILLEMENT SEUL
      IF (C2.NE.0.D0) CALL ZEROP2(2.D0*C1/C2, (C0-TT2)/C2, RAC(1), NRAC)

C    EQUATION SUR LE CISAILLEMENT + L'OUVERTURE
      CALL ZEROP2(2.D0*(C1+N1)/(C2+N2), (C0+N0-TT2)/(C2+N2),RAC(3),NRAC)


C -- SELECTION DES SOLUTIONS CONFORMES AVEC LE SIGNE SUR L'OUVERTURE

      NRAC = 0
      DO 100 I = 1,4
        IF (RAC(I).NE.R8VIDE()) THEN
          ETA = RAC(I)
          
          IF (I.LE.2) THEN
            IF ( (TPN + ETA*TDN).LT.0.D0 ) THEN
              PENTE = 2.D0*(C2*ETA + C1)
              COPILO(NRAC+1) = TAU - PENTE*ETA
              COPILO(NRAC+2) = PENTE
              NRAC = NRAC + 2
            END IF
          ELSE
            IF ( (TPN + ETA*TDN).GE.0.D0 ) THEN
              PENTE = 2.D0*((C2+N2)*ETA + C1+N1)
              COPILO(NRAC+1) = TAU - PENTE*ETA
              COPILO(NRAC+2) = PENTE
              NRAC = NRAC + 2
            END IF
          END IF
          
        END IF
 100  CONTINUE  
      IF (NRAC.EQ.0) COPILO(5) = 0.D0
  
 9999 CONTINUE
      END
