        SUBROUTINE LCMMFL( TAUS,COEFT,IFA,NMAT,NBCOMM,NECOUL,NECRIS,
     &                     VIS,X,DTIME,DGAMMA,DP )
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3)
        REAL*8 TAUS,COEFT(NMAT),VIS(3),DGAMMA,DP,X,DTIME
        CHARACTER*16 NECOUL,NECRIS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C       IN  TAUS     :  SCISSION REDUITE
C           COEFT   :  PARAMETRES MATERIAU
C           IFA :  NUMERO DE FAMILLE
C           NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NECOU  :  NOM DE LA LOI D'ECOULEMENT
C           NECRIS  :  NOM DE LA LOI D'ECROUISSAGE ISTROPE
C           VIS : VARIABLES INTERNES DU SYSTEME DE GLISSEMENT COURANT
C     OUT:
C           DGAMMA    :  DERIVEES DES VARIABLES INTERNES A T
C           DP
C  INTEGRATION DES LOIS MONOCRISTALLINES PAR UNE METHODE DE RUNGE KUTTA
C
C     ----------------------------------------------------------------
      REAL*8 C,P,R0,Q,H,B,RP,K,N,FTAU,CRIT,B1,B2,Q1,Q2
      INTEGER IFL,IEI
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)
      IEI=NBCOMM(IFA,3)
      P=VIS(3)
      IF (NECRIS.EQ.'ECRO_ISOT1') THEN
         R0=COEFT(IEI-1+1)
         Q=COEFT(IEI-1+2)
         B=COEFT(IEI-1+3)
         H=COEFT(IEI-1+4)
         
         RP=R0+Q*H*(1.D0-EXP(-B*P))
      ELSEIF (NECRIS.EQ.'ECRO_ISOT2') THEN
         R0=COEFT(IEI-1+1)
         Q1=COEFT(IEI-1+2)
         B1=COEFT(IEI-1+3)
         H =COEFT(IEI-1+4)
         Q2=COEFT(IEI-1+5)
         B2=COEFT(IEI-1+6)
         
C         RP=R0+Q1*H*(1.D0-EXP(-B1*P))+Q2*(1.D0-EXP(-B2*P))
         RP=R0+Q1*(1.D0-EXP(-B1*P))+Q2*(1.D0-EXP(-B2*P))
      ENDIF
      IF (NECOUL.EQ.'ECOU_VISC1') THEN
          N=COEFT(IFL-1+1)
          K=COEFT(IFL-1+2)
          C=COEFT(IFL-1+3)
      
          FTAU=TAUS-C*VIS(1)
          
          CRIT=ABS(FTAU)-RP 
          IF (CRIT.GT.0.D0) THEN
C             DP=((CRIT/K)**N)*X/DTIME
             DP=((CRIT/K)**N)
             DGAMMA=DP*FTAU/ABS(FTAU)
          ELSE
             DP=0.D0
             DGAMMA=0.D0
          ENDIF
       ENDIF
      
           
      END
