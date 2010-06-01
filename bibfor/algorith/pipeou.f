      SUBROUTINE PIPEOU(MAT,SUP,SUD,MUP,MUD,VIM,TAU,COPILO)
            
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
C     PILOTAGE PRED_ELAS POUR LA LOI D'INTERFACE OUVERTURE 
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
      REAL*8  SC,GC,DC,H,R,KA,GA,SK,VAL(3),TMP
      REAL*8  TT,TPN,TDN,TAUREF
      CHARACTER*2 COD(3)
      CHARACTER*8 NOM(3)
      
      DATA NOM /'GC','SIGM_C','PENA_LAGR'/
C ----------------------------------------------------------------------


C -- CAS DE L'ENDOMMAGEMENT SATURE

      GA = VIM(4) + TAU
      IF (GA .GT. 1.D0) GOTO 9999

C -- RECUPERATION DES PARAMETRES PHYSIQUES

      CALL RCVALA(MAT,' ','RUPT_FRAG',0,' ',0.D0,3,NOM,VAL,COD,'F ')
      GC   = VAL(1)      
      SC   = VAL(2)  
      DC   = 2.D0*GC/SC
      H    = SC/DC
      R    = H * VAL(3)
      
C    CALCUL DE KAPPA : KA = DC*(1-SQRT(1-GA))

      TMP = SQRT(MAX(0.D0,1.D0-GA))
      TMP = DC*(1.D0-TMP)
      TMP = MAX(0.D0,TMP)
      TMP = MIN(DC,TMP)
      KA  = TMP
      SK  = MAX(0.D0,SC - H*KA)

C   CALCUL DU SEUIL
   
      TT = R*KA + SK
      TAUREF = TAU/TT  
            
C -- CALCUL DU SECOND MEMBRE 
       
      TPN = MUP(1) + R*SUP(1)
      TDN = MUD(1) + R*SUD(1)
      
C -- PILOTAGE DU POINT

      COPILO(1) = TAUREF * TPN
      COPILO(2) = TAUREF * TDN

 9999 CONTINUE
      END
