      SUBROUTINE IRRJAC ( FAMI,KPG,KSP,MOD, NMAT, MATERF,
     1                    YF,  DY,  NMOD, DRDY)

      IMPLICIT NONE
      CHARACTER*(*) FAMI
      CHARACTER*8   MOD
      INTEGER       NMAT,NMOD,KPG,KSP
      REAL*8        MATERF(NMAT,2),YF(*),DY(*),DRDY(NMOD,NMOD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/02/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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


C     ------------------------------------------------------------------
C  IRRAD3M    : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C               DY    = ( DSIG  DP  DETA   DPI   DG  (DEPS3) )
C               Y     = ( SIG   P   ETA    PI    G   (DEPS3) )
C               DRDY  = ( DRSDS  DRSDP  DRSDE  DRSDI DRSDG (DRSDE3) )
C                       ( DRPDS  DRPDP  DRPDE  DRPDI DRPDG (DRPDE3) )
C                       ( DREDS  DREDP  DREDE  DREDI DREDG (DREDE3) )
C                       ( DRIDS  DRIDP  DRIDE  DRIDI DRIDG (DRIDE3) )
C                       ( DRGDS  DRGDP  DRGDE  DRGDI DRGDG (DRGDE3) )
C                       ((DQDS) (DQDP) (DQDE) (DQDI) (DQDG) (DQDE3) )
C  IN  FAMI   :  FAMILLE DES POINTS DE GAUSS
C      KPG    :  NUMERO DU POINT DE GAUSS
C      KSP    :  NUMERO DU SOUS POINT DE GAUSS
C      MOD    :  TYPE DE MODELISATION
C      NMAT   :  DIMENSION MATER
C      MATERF :  COEFFICIENTS MATERIAU A T+DT
C      YF     :  VARIABLES A T + DT
C                ( SIGF  PF   ETAF   PIF   GF  (EPS3F) )
C      DY     :  SOLUTION
C                ( DSIG  DP   DETA   DPI   SG  (DEPS3) )
C      NMOD   :  DIMENSION DECLAREE DRDY
C  OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C  ----------------------------------------------------------------
      REAL*8          FKOOH(6,6),ID(6),K,N,P0,AI0,IRRAD,IRRAF,SIGF(6)
      REAL*8          ETAIS,PF,DP,DPI,DPHI,S,DEV(6),DFDS(6)
      REAL*8          DRSDS(6,6),DRSDP(6),DRSDE(6),DRSDI(6),DRSDG(6)
      REAL*8          DRPDS(6),DRPDP,DRPDE,DRPDI,DRPDG
      REAL*8          DREDS(6),DREDP,DREDE,DREDI,DREDG
      REAL*8          DRIDS(6),DRIDP,DRIDE,DRIDI,DRIDG
      REAL*8          DRGDS(6),DRGDP,DRGDE,DRGDI,DRGDG
      REAL*8          DRSDE3(6),DRPDE3,DREDE3,DRIDE3,DRGDE3
      REAL*8          DQDS(4),DQDP,DQDE,DQDI,DQDG,DQDE3,R8PREM
      REAL*8          LCNRTS,SR,DDFDDS(6,6),ETAIF,DEDE3(6),HOOKF(6,6)
      INTEGER         NDT,NDI,IRET,I,J
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
      DATA   ID       / 1.D0,  1.D0,  1.D0,  0.D0,  0.D0,  0.D0/
      DATA DEDE3      / 0.D0  , 0.D0  , -1.D0  , 0.D0 , 0.D0 , 0.D0/

      CALL LCOPIL ( 'ISOTROPE' , MOD , MATERF(1,1) , FKOOH )
      CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOKF )
      CALL RCVARC ('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
      CALL RCVARC ('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)

      CALL LCEQVN ( NDT , YF(1), SIGF )
      DP=DY(NDT+1)
      PF=YF(NDT+1)
      ETAIF=YF(NDT+2)
      DPI=DY(NDT+3)
      DPHI=IRRAF-IRRAD
      K=MATERF(1,2)
      N=MATERF(2,2)
      P0=MATERF(3,2)
      AI0=MATERF(4,2)
      ETAIS=MATERF(5,2)
      CALL LCDEVI ( SIGF , DEV )
      S =  LCNRTS ( DEV )
      IF ( S.EQ.0.D0) THEN
        CALL LCINVE (0.D0,DFDS)
      ELSE
        CALL LCPRSV ( 1.5D0 / S , DEV , DFDS )
      ENDIF
   
      IF ( ((PF+P0).EQ.0.D0).AND.(N.EQ.0.D0)) THEN
        SR = K
      ELSE
        SR = K*(PF+P0)**N
      ENDIF
C - DRSDS
        
      CALL IRRFSS  (SIGF,DDFDDS)
      CALL LCPRSM ((DP+DPI),DDFDDS,DRSDS)
      CALL LCSOMA (FKOOH,DRSDS,DRSDS)


C - DRSDP
      
      CALL LCEQVN(NDT,DFDS,DRSDP)

C - DRSDE
      
      CALL LCINVE(0.D0,DRSDE)
      
C - DRSDI
      IF ( ETAIF.GT.ETAIS) THEN
        CALL LCEQVN(NDT,DFDS,DRSDI)
      ELSE
        CALL LCINVE(0.D0,DRSDI)
      ENDIF
C - DRSDG

      CALL LCEQVN(NDT,ID,DRSDG)

C - DRPDS

      IF (((S.GE.SR).AND.(DP.GE.0.D0)).OR.(DP.GT.R8PREM())) THEN
        CALL LCEQVN(NDT,DFDS,DRPDS)
        CALL LCPRSV((1.D0/HOOKF(1,1)),DFDS,DRPDS)
      ELSE
        CALL LCINVE(0.D0,DRPDS)
      ENDIF

C - DRPDP
      IF (((S.GE.SR).AND.(DP.GE.0.D0)).OR.(DP.GT.R8PREM())) THEN
        IF ( N.EQ.0.D0) THEN
          DRPDP=0.D0
        ELSE
          DRPDP=(-N*K*(PF+P0)**(N-1.D0))/HOOKF(1,1)
        ENDIF
      ELSE 
        DRPDP=1.D0
      ENDIF

C - DRPDE
      
      DRPDE=0.D0

C - DRPDI
      
      DRPDI=0.D0
      
C - DRPDG

      DRPDG=0.D0

C - DREDS

      CALL LCPRSV ((-DPHI/HOOKF(1,1)),DFDS,DREDS)

C - DREDP

      DREDP=0.D0

C - DREDE

      DREDE=1.D0/HOOKF(1,1)

C - DREDI

      DREDI=0.D0

C - DREDG

      DREDG=0.D0

C - DRIDS

      IF (ETAIF.GT.ETAIS) THEN
        CALL LCPRSV((-DPHI*AI0),DFDS,DRIDS)
      ELSE
        CALL LCINVE(0.D0,DRIDS)
      ENDIF

C - DRIDP
      DRIDP=0.D0

C - DRIDE
      DRIDE=0.D0

C - DRIDI
      DRIDI=1.D0
      
C - DRIDG
      DRIDG=0.D0

C - DRGDS
      CALL LCINVE(0.D0,DRGDS)

C - DRGDP
      DRGDP=0.D0

C - DRGDE
      DRGDE=0.D0

C - DRGDI
      DRGDI=0.D0

C - DRGDG
      DRGDG=1.D0

C - CONTRAINTES PLANES

      IF( MOD(1:6).EQ.'C_PLAN' ) THEN

C - DRSDE3

        CALL LCEQVN(NDT,DEDE3,DRSDE3)

C - DRPDE3
        
        DRPDE3=0.D0

C - DREDE3

        DREDE3=0.D0

C - DRIDE3

        DRIDE3=0.D0

C - DRGDE3

        DRGDE3=0.D0

C - DQDS

        DQDS(1)= (-(DP+DPI)*(HOOKF(3,3)*DDFDDS(3,1) + 
     &                HOOKF(3,1)*DDFDDS(1,1) +
     &                HOOKF(3,2)*DDFDDS(2,1) +
     &                HOOKF(3,4)*DDFDDS(4,1)))/HOOKF(1,1)
        DQDS(2)= (-(DP+DPI)*(HOOKF(3,3)*DDFDDS(3,2) +
     &                HOOKF(3,1)*DDFDDS(1,2) +
     &                HOOKF(3,2)*DDFDDS(2,2) + 
     &                HOOKF(3,4)*DDFDDS(4,2)))/HOOKF(1,1)
        DQDS(3)= (-(DP+DPI)*(HOOKF(3,3)*DDFDDS(3,3) +
     &                HOOKF(3,1)*DDFDDS(1,3) +
     &                HOOKF(3,2)*DDFDDS(2,3) +
     &                HOOKF(3,4)*DDFDDS(4,3)))/HOOKF(1,1)
        DQDS(4)= (-(DP+DPI)*(HOOKF(3,3)*DDFDDS(3,4) +
     &                HOOKF(3,1)*DDFDDS(1,4) +
     &                HOOKF(3,2)*DDFDDS(2,4) +
     &                HOOKF(3,4)*DDFDDS(4,4)))/HOOKF(1,1)

C - DQDP
        DQDP = (- HOOKF(3,1)*DFDS(1) - HOOKF(3,2)*DFDS(2)
     &        - HOOKF(3,3)*DFDS(3) - HOOKF(3,4)*DFDS(4))/HOOKF(1,1)

C - DQDE
      
        DQDE=0.D0

C - DQDI
      
        DQDI = (- HOOKF(3,1)*DFDS(1) - HOOKF(3,2)*DFDS(2)
     &         - HOOKF(3,3)*DFDS(3) - HOOKF(3,4)*DFDS(4))/HOOKF(1,1)


C - DQDG
      
        DQDG=-HOOKF(3,3)/HOOKF(1,1)

C - DQDE3
      
        DQDE3=HOOKF(3,3)/HOOKF(1,1)

      ENDIF
C - ASSEMBLAGE

C - DRDY(T+DT)  =  DRSDS  DRSDP  DRSDE  DRSDI DRSDG (DRSDE3) 
C                  DRPDS  DRPDP  DRPDE  DRPDI DRPDG (DRPDE3) 
C                  DREDS  DREDP  DREDE  DREDI DREDG (DREDE3) 
C                  DRIDS  DRIDP  DRIDE  DRIDI DRIDG (DRIDE3) 
C                  DRGDS  DRGDP  DRGDE  DRGDI DRGDG (DRGDE3) 
C                 (DQDS) (DQDP) (DQDE) (DQDI) (DQDG) (DQDE3) 

      
      CALL LCICMA(DRSDS,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
      CALL LCICMA(DRSDP,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+1)
      CALL LCICMA(DRSDE,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+2)
      CALL LCICMA(DRSDI,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+3)
      CALL LCICMA(DRSDG,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+4)

      CALL LCICMA(DRPDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
      CALL LCICMA(DREDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+2,1)
      CALL LCICMA(DRIDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+3,1)
      CALL LCICMA(DRGDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+4,1)

      DRDY(NDT+1,NDT+1)=DRPDP
      DRDY(NDT+1,NDT+2)=DRPDE
      DRDY(NDT+1,NDT+3)=DRPDI
      DRDY(NDT+1,NDT+4)=DRPDG

      DRDY(NDT+2,NDT+1)=DREDP
      DRDY(NDT+2,NDT+2)=DREDE
      DRDY(NDT+2,NDT+3)=DREDI
      DRDY(NDT+2,NDT+4)=DREDG
      
      DRDY(NDT+3,NDT+1)=DRIDP
      DRDY(NDT+3,NDT+2)=DRIDE
      DRDY(NDT+3,NDT+3)=DRIDI
      DRDY(NDT+3,NDT+4)=DRIDG

      DRDY(NDT+4,NDT+1)=DRGDP
      DRDY(NDT+4,NDT+2)=DRGDE
      DRDY(NDT+4,NDT+3)=DRGDI
      DRDY(NDT+4,NDT+4)=DRGDG

      IF( MOD(1:6).EQ.'C_PLAN' ) THEN
      
        CALL LCICMA (DRSDE3,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+5)
        CALL LCICMA (DQDS  ,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+5,1)
        DRDY(NDT+1,NDT+5)=DRPDE3
        DRDY(NDT+2,NDT+5)=DREDE3
        DRDY(NDT+3,NDT+5)=DRIDE3
        DRDY(NDT+4,NDT+5)=DRGDE3
        DRDY(NDT+5,NDT+1)=DQDP
        DRDY(NDT+5,NDT+2)=DQDE
        DRDY(NDT+5,NDT+3)=DQDI
        DRDY(NDT+5,NDT+4)=DQDG
        DRDY(NDT+5,NDT+5)=DQDE3
      
      ENDIF
      END
