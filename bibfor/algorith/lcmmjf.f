        SUBROUTINE LCMMJF( TAUS,COEFT,IFA,NMAT,NBCOMM,DT,NECOUL,
     &             TPERD,NUMS,VIS,NVI,RP,DRSDPR,DVDTAU,DDVIS,DDVIR,DP)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3),NUMS,NVI
        REAL*8 TAUS,COEFT(NMAT),VIS(3),DVDTAU(3),RP,DDVIS(3,3),DT
        REAL*8 DDVIR(NVI),DRSDPR(NVI)
        CHARACTER*16 NECOUL,NECRIS,NECRCI
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/08/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C RESPONSABLE JMBHH01 J.M.PROIX
C ======================================================================
C  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
C  POUR LA LOI D'ECOULEMENT
C       IN  TAUS     :  SCISSION REDUITE
C           COEFT   :  PARAMETRES MATERIAU
C           IFA :  NUMERO DE FAMILLE
C           NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NECOU  :  NOM DE LA LOI D'ECOULEMENT
C           VIS : VARIABLES INTERNES DU SYSTEME DE GLISSEMENT COURANT
C     OUT:
C       DVDTAU    :  DERIVEES DES VARIABLES INTERNES PAR RAPPORT A TAU
C       DDVIS    :  DERIVEES DES VARIABLES INTERNES PAR RAPPORT AUX
C           AUTRES
C       DDVIR    :  DERIVEES DES VARIABLES INTERNES PAR RAPPORT AUX
C                   VARIABLES INTERNES DES AUTRES SYSTEMES
C       DP
C     ----------------------------------------------------------------
      REAL*8 C,P,R0,Q,H,B,K,N,FTAU,CRIT,B1,B2,Q1,Q2,A,GAMMA0,D,VAL
      REAL*8 TPERD,TABS,DRDP,ALPHA,GAMMA,DP,DGAMMA,TAUMU,TAUV,GM,PM,CC
      REAL*8 PR,DRDPR,DELTAV,DELTAG
      INTEGER IFL,NS, IS
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P
C     DANS DDVIS : 1,1 = DA/DALPHA, 1,2=DG/DALPHA, 1,3=DP/DALPHA...
C     DANS DDVIS : 2,1 = DA/DGAMMA, 2,2=DG/DGAMMA, 2,3=DP/DGAMMA...
C     DANS DDVIS : 3,1 = DA/DP,     3,2=DG/DP,     3,3=DP/DP...
C     DANS DVDTAU : 1 = DALPHA/DTAU, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)

      ALPHA=VIS(1)

C     NS=NOMBRE TOTAL DE SYSTEMES DE GLISSEMENT         
      NS=(NVI-1-6)/3
      CALL LCINVN(NS, 0.D0, DDVIR)
      CALL LCINVN(3, 0.D0, DVDTAU)
      CALL LCINVN(3*3, 0.D0, DDVIS)      
            
      IF (NECOUL.EQ.'ECOU_VISC1') THEN
          N=COEFT(IFL-1+1)
          K=COEFT(IFL-1+2)
          C=COEFT(IFL-1+3)
      
          FTAU=TAUS-C*ALPHA
          
          CRIT=ABS(FTAU)-RP 
          IF (CRIT.GT.0.D0) THEN
             VAL=DT*(CRIT/K)**(N-1)
             DP=((CRIT/K)**N)*DT
C             DGAMMA=DP*FTAU/ABS(FTAU)
             
             DDVIS(2,1)=0.D0
             DDVIS(2,2)=1.D0
             DDVIS(2,3)=-FTAU/ABS(FTAU)
             
             DDVIS(3,1)= N*C*VAL/K*FTAU/ABS(FTAU)
             DDVIS(3,2)= 0.D0
             DDVIS(3,3)= 1.D0+N*VAL/K*DRSDPR(NUMS)
             
             DVDTAU(3)=-N*VAL/K*FTAU/ABS(FTAU)
             
             DO 11 IS=1,NS
                IF (IS.EQ.NUMS) THEN
                   DDVIR(IS)=1.D0+N*VAL/K*DRSDPR(IS)
                ELSE
                   DDVIR(IS)=N*VAL/K*DRSDPR(IS)
                ENDIF
 11          CONTINUE
             
          ELSE
             DP=0.D0
C             DGAMMA=0.D0
C             DP/DP
             DDVIS(3,3)=1.D0             
             DDVIS(2,2)=1.D0
             DDVIS(2,3)=-FTAU/ABS(FTAU)
             DVDTAU(3)=0.D0
             DDVIR(NUMS)=1.D0
         ENDIF
       ENDIF
       
      IF (NECOUL.EQ.'ECOU_VISC2') THEN
          N=COEFT(IFL-1+1)
          K=COEFT(IFL-1+2)
          C=COEFT(IFL-1+3)
          A=COEFT(IFL-1+4)
          D=COEFT(IFL-1+5)
          
          FTAU=TAUS-C*VIS(1)-A*VIS(2)
          
          CRIT=ABS(FTAU)-RP + (C/2/D)*(C*VIS(1))**2
          IF (CRIT.GT.0.D0) THEN
             VAL=DT*(CRIT/K)**(N-1)
             DP=((CRIT/K)**N)*DT
C             DGAMMA=DP*FTAU/ABS(FTAU)
             
             DDVIS(2,1)=0.D0
             DDVIS(2,2)=1.D0
             DDVIS(2,3)=-FTAU/ABS(FTAU)
             
             DDVIS(3,1)= -N*VAL/K*(-C*FTAU/ABS(FTAU)+C*C*C/D*VIS(1))
             DDVIS(3,2)= N*VAL/K*A*FTAU/ABS(FTAU)
             DDVIS(3,3)= 1.D0+N*VAL/K*DRSDPR(NUMS)
             
             DVDTAU(3)=-N*VAL/K*FTAU/ABS(FTAU)
             
             
             DO 13 IS=1,NS
                IF (IS.EQ.NUMS) THEN
                   DDVIR(IS)=1.D0+N*VAL/K*DRSDPR(IS)
                ELSE
                   DDVIR(IS)=N*VAL/K*DRSDPR(IS)
                ENDIF
 13          CONTINUE
             
          ELSE
             DP=0.D0
C             DGAMMA=0.D0
C             DP/DP
             DDVIS(3,3)=1.D0             
             DDVIS(2,2)=1.D0
             DDVIS(2,3)=-FTAU/ABS(FTAU)
             DVDTAU(3)=0.D0
             DDVIR(NUMS)=1.D0
          ENDIF
       ENDIF
       
      IF (NECOUL.EQ.'ECOU_VISC3') THEN
          K      =COEFT(IFL-1+1)
          TAUMU  =COEFT(IFL-1+2)
          GAMMA0 =COEFT(IFL-1+3)
          DELTAV =COEFT(IFL-1+4)
          DELTAG =COEFT(IFL-1+5)
          
          TAUV=ABS(TAUS)-TAUMU 
          IF (TAUV.GT.0.D0) THEN
             TABS=TPERD+273.5D0

             DDVIS(2,1)=0.D0
             DDVIS(2,2)=1.D0
             DDVIS(2,3)=-TAUS/ABS(TAUS)
             
             DDVIS(3,1)= 0.D0
             DDVIS(3,2)= 0.D0
             DDVIS(3,3)= 1.D0
C             DDVIS(3,3)= 1.D0+GAMMA0*DELTAV/K/TABS*EXP(-DELTAG/K/TABS)
C     &                 *EXP(DELTAV/K/TABS*TAUV)*DRSDPR(NUMS)
             
             DVDTAU(3)=GAMMA0*DELTAV/K/TABS*EXP(-DELTAG/K/TABS)
     &                 *EXP(DELTAV/K/TABS*TAUV)*TAUS/ABS(TAUS)
             DDVIR(NUMS)=0.D0
             
          ELSE
             DDVIS(3,3)=1.D0             
             DDVIS(2,2)=1.D0
             DDVIS(2,3)=-FTAU/ABS(FTAU)
             DVDTAU(3)=0.D0
             DDVIR(NUMS)=0.D0
          ENDIF
       ENDIF
       
      IF (NECOUL.EQ.'ECOU_PLAS1') THEN
          C=COEFT(IFL-1+1)
          CRIT=ABS(FTAU)-RP 
      
          IF (CRIT.GT.0.D0) THEN
             CALL UTMESS('F','LCMMFL','ECOU_PLAS1 NON DISPONIBLE')
          ELSE
             DP=0.D0
C             DGAMMA=0.D0
          ENDIF
       ENDIF  
       
           
      END
