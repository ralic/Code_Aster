        SUBROUTINE LCMMJF( TAUS,COEFT,MATERF,IFA,NMAT,
     &    NBCOMM,DT,NECOUL,IS,IR,NBSYS,VIND,DY,HSR,RP,ALPHAP,DALPHA,
     &    GAMMAP,DGAMMA,SGNR,DGDTAU,DGDAL,DFDR,IRET)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3),NUMHSR
        REAL*8 TAUS,COEFT(NMAT),RP,DT,ALPHAP,DALPHA,GAMMAP,DGAMMA
        REAL*8 DGDTAU,DGDAL,DFDR,HSR(5,24,24),DY(*),VIND(*),MATERF(NMAT)
        CHARACTER*16 NECOUL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C TOLE CRP_21
C MODIF ALGORITH  DATE 24/03/2009   AUTEUR REZETTE C.REZETTE 
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
C       IN  TAUS    :  SCISSION REDUITE
C           COEFT   :  PARAMETRES MATERIAU
C           IFA     :  NUMERO DE FAMILLE
C           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NECOUL  :  NOM DE LA LOI D'ECOULEMENT
C           RP      :  ECROUISSAGE
C           ALPHAP  :  ALPHA (CINEMATIQUE) A T+DT
C           DALPHA  :  DALPHA ENTRE T ET T+DT ITERATION COURANTE
C           GAMMAP  :  GAMMA (GLISSEMENT) A T+DT
C           DALPHA  :  DGAMMA ENTRE T ET T+DT ITERATION COURANTE
C     OUT:
C           DGDTAU  :  dF/dTau
C           DGDAL   :  dF/dAlpha  (particulier pour KR)
C           DFDR    :  dF/dR (ou dhs/dalphar pour KR)
C     ----------------------------------------------------------------
      REAL*8 C,P,R0,Q,H,B,K,N,FTAU,CRIT,B1,B2,Q1,Q2,A,GAMMA0,D
      REAL*8 TPERD,TABS,DRDP,ALPHA,GAMMA,DP,TAUMU,TAUV,GM,PM,CC
      REAL*8 SGNR,PR,DRDPR,DELTAV,DELTG0,SGNS,R8MIEM,AUX,ALPHAS
      REAL*8 TAUR,TAU0,TAUEF,BSD,GCB,KDCS,R,INT1,INT2,INT3,DFDTMU
      REAL*8 DTMUDR,SOM,DRDGAM,CISA2, RR,DELTGG,TERME,PETITG,CS
      REAL*8 DTEDTO,DGGDTO,DTEDAL,DGGDAL,DELTSR,DHDAL,PETITH

      INTEGER IFL,NS, IS, NBSYS,IU,IR,IRET,NUECOU,NULHSR,IRET2
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)
      NUECOU=NINT(COEFT(IFL))
      IRET=0

C      IF (NECOUL.EQ.'ECOU_VISC1') THEN
      IF (NUECOU.EQ.1) THEN

          N=COEFT(IFL+1)
          K=COEFT(IFL+2)
          C=COEFT(IFL+3)

          FTAU=TAUS-C*ALPHAP
          CRIT=ABS(FTAU)-RP

C         dF/dTau

          IF (CRIT.GT.0.D0) THEN
             DGDTAU=(N*DT/(K**N))*(CRIT**(N-1))
          ELSE
             DGDTAU=0.D0
          ENDIF

C         DGDAL
          DGDAL=-C*DGDTAU

C         DFDR
          IF (ABS(FTAU).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=FTAU/ABS(FTAU)
          ENDIF
          DFDR=-SGNS*DGDTAU

C      IF (NECOUL.EQ.'ECOU_VISC2') THEN
      ELSEIF (NUECOU.EQ.2) THEN
      
          N=COEFT(IFL+1)
          K=COEFT(IFL+2)
          C=COEFT(IFL+3)
          A=COEFT(IFL+4)
          D=COEFT(IFL+5)

          FTAU=TAUS-C*ALPHAP-A*GAMMAP

          CRIT=ABS(FTAU)-RP + 0.5D0*C*D*(ALPHAP)**2
          IF (ABS(FTAU).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=FTAU/ABS(FTAU)
          ENDIF

          IF (CRIT.GT.0.D0) THEN
             DGDTAU=(N*DT/(K**N))*(CRIT**(N-1))
          ELSE
             DGDTAU=0.D0
          ENDIF

C         DGDAL
          DGDAL=(-C*SGNS+D*ALPHAP*DALPHA)*DGDTAU*SGNS

C         DFDR
          DFDR=-SGNS*DGDTAU


C      ELSEIF (NECOUL.EQ.'ECOU_VISC3') THEN
      ELSEIF (NUECOU.EQ.3) THEN
      
          K      =COEFT(IFL+1)
          TAUMU  =COEFT(IFL+2)
          GAMMA0 =COEFT(IFL+3)
          DELTAV =COEFT(IFL+4)
          DELTG0 =COEFT(IFL+5)
          TPERD  =COEFT(IFL+6)
          CRIT=ABS(TAUS)-TAUMU
          IF (ABS(TAUS).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=(TAUS)/ABS(TAUS)
          ENDIF
          
          IF (CRIT.LT.0.D0) THEN
             DGDTAU=0.D0
          ELSE
               TABS=TPERD+273.15D0
               DGDTAU=GAMMA0*DELTAV*K*TABS*EXP(-DELTG0/K/TABS)
     &                *EXP(DELTAV/K/TABS*CRIT-1.D0)*(DELTAV/K/TABS*CRIT)
          ENDIF

C         DGDAL
          DGDAL=0.D0

C         DFDR
          DFDR=0.D0


      ELSEIF (NUECOU.EQ.4) THEN
C             MATRICE JACOBIENNE DU SYSTEME :
C  R1 = D-1*SIGMA - (D_M-1*SIGMA_M)-(DEPS-DEPS_TH)+Somme(ms*Gamma_s)=0
C  R2 = dALPHA - g(Taus,alphas)*h(alphas)
C avec Gamma_s=g(Taus,alphas)*sgn(taus)
C
C ON VEUT CALCULER :
C         dg(taus,alphas)/dtaus          
          DGDTAU=0.D0
C         dg(taus,alphas)/dalphar          
          DGDAL=0.D0
C         dh(alphas)/dalphar          
          DHDAL=0.D0
C         DFDR=DHDAL
          DFDR=0.D0
                
          K         =COEFT(IFL+1)
          TAUR      =COEFT(IFL+2)
          TAU0      =COEFT(IFL+3)
          GAMMA0    =COEFT(IFL+4)
          DELTG0    =COEFT(IFL+5)
          BSD       =COEFT(IFL+6)
          GCB       =COEFT(IFL+7)
          KDCS      =COEFT(IFL+8)
          P         =COEFT(IFL+9)
          Q         =COEFT(IFL+10)
          NUMHSR    =NINT(COEFT(IFL+12))
          TPERD     =COEFT(IFL+11)
          IF (MATERF(NMAT).EQ.0) THEN
             CISA2 = (MATERF(1)/2.D0/(1.D0+MATERF(2)))**2
          ELSE
             CISA2 = (MATERF(36)/2.D0)**2
          ENDIF
          TAUV=ABS(TAUS)-TAU0
          IF (ABS(TAUS).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=(TAUS)/ABS(TAUS)
          ENDIF
          IF (TAUV.GT.0.D0) THEN
             SOM    = 0.D0
             TAUMU  = 0.D0
             DGDAL  = 0.D0
             DO 1 IU = 1, NBSYS
               ALPHA = VIND(3*(IU-1)+1)+DY(IU)
C              PARTIE POSITIVE DE ALPHA
               IF (ALPHA.GT.0.D0) THEN
                  TAUMU = TAUMU +  HSR(NUMHSR,IS,IU)*ALPHA
                  IF (IU.NE.IS) SOM = SOM+ALPHA
               ENDIF
 1           CONTINUE
             ALPHAS= VIND(3*(IS-1)+1)+DY(IS)
             SOM=SQRT(SOM)
             TAUMU = CISA2 * TAUMU/TAUV
             TAUEF = TAUV-TAUMU
             IF (TAUEF.GT.0.D0) THEN
                AUX= (1.D0-(TAUEF/TAUR)**P)
                IF (AUX.LE.0.D0)  THEN
C                   print *,'attention, lcmmjf,AUX=',AUX
                   IRET=1
                   GOTO 9999
                ENDIF
               TABS=TPERD+273.15D0
C              PROTECTION DE l'EXPONENTIELLE
               DELTGG=DELTG0*(AUX**Q)
               TERME=-DELTGG/K/TABS
               IF (TERME.GT.10.D0) THEN
                  IRET=1
C                  print *,'attention, lcmmjf,TERME=',TERME
                  GOTO 9999
               ENDIF
C              CALCUL DE dg/dtau
               PETITG=GAMMA0*EXP(TERME)*DT
               PETITH=BSD+SOM/KDCS-GCB*ALPHAS
               IF (PETITH.LT.0.D0) PETITH=0.D0
               CS=-(P*Q*DELTG0/TAUR)
               CS=CS*(AUX**(Q-1.D0))*(TAUEF/TAUR)**(P-1.D0)
               DTEDTO=SGNS*(1.D0+TAUMU/TAUV)
               DGGDTO=CS*DTEDTO
               DGDTAU =-PETITG*SGNS*DGGDTO/K/TABS
               
               IF (IR.NE.0) THEN
C                  CALCUL DE dgs/dalphar
                   DTEDAL=-CISA2/TAUV*HSR(NUMHSR,IS,IR)
                   DGGDAL=CS*DTEDAL
                   DGDAL=-PETITG/K/TABS*DGGDAL
 
C                  CALCUL DE dhs/dalphar
                   DELTSR=0.D0
                   IF (IR.EQ.IS) DELTSR=1.D0
                   
                   IF (PETITH.GT.0.D0) THEN
                      IF (SOM.GT.0.D0) THEN
                         DHDAL=(1-DELTSR)/SOM/2.D0/KDCS-GCB*DELTSR
                      ENDIF
                   ELSE
                      DHDAL=0.D0
                   ENDIF
                   DFDR=DHDAL
               ELSE
                   DFDR=PETITH
               ENDIF
             ENDIF
          ENDIF

      ENDIF
9999  CONTINUE
      END
