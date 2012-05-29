      SUBROUTINE LCDDCC(  TAUS, COEFT,   IFA,  NMAT, NBCOMM,
     &                      IS, NBSYS, NFS,NSG,HSR, VIND, DY,
     &                      DT,    RP,NUECOU,DALPHA,DGAMMA,  DP,
     &                    IRET                              )
      IMPLICIT NONE
      INTEGER IFA,NMAT,NBCOMM(NMAT,3),IRET
      INTEGER IFL,IS,IR,NBSYS,NFS,NSG,NUECOU,IRR
      REAL*8 TAUS,COEFT(NMAT),DGAMMA,DP,VIND(*),DALPHA
      REAL*8 RP,SGNS,HSR(NSG,NSG),DY(12),DT,DEPSDT
      REAL*8 N,GAMMA0,R8MIEM,RMIN,RHOP(12),R8MAEM
      REAL*8 TAUF,RHOM(12),RMAX,HS,GAMPRO,GP1,GP2,R8PI,YS,TAUEFF
      REAL*8 B,H,DELTG0,TAU0,D,BETA,TEMP,DLAT,KF,KSELF,RHOMOB,KBOLTZ
      REAL*8 YAT,MU,LC,RHOTOT,EPSEQ,DG,DELTAG,T1,T2,T3,T4,T5,T6,T7,T8,T9
      REAL*8 RS,D1,LAMBDA,ALPHAT,LS,TAUSLT,TAUSLR,GAMNUC,ASR
      REAL*8 DELTA1,DELTA2,AIRR,XI,RHOIRR,DEPDT,TAUC,T10
      LOGICAL NEW
      COMMON /DEPS6/DEPSDT
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/05/2012   AUTEUR PROIX J-M.PROIX 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ======================================================================
C  COMPORTEMENT MONOCRISTALLIN : ECOULEMENT (VISCO)PLASTIQUE
C  INTEGRATION DE LA LOI MONOCRISTALLINE DD-CC. CALCUL DE DALPHA DGAMMA
C
C ARGUMENTS
C
C       IN  TAUS    :  SCISSION REDUITE
C           COEFT   :  PARAMETRES MATERIAU
C           IFA     :  NUMERO DE FAMILLE
C           CISA2   :  COEF DE CISAILLEMENT MU
C           NMAT    :  NOMBRE MAXI DE MATERIAUX
C           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NBSYS   :  NOMBRE DE SYSTEMES DE GLISSEMENT
C           HSR     :  Hsr
C           VIND    :  tous les variables internes instant precedent
C           DT      :  INTERVALLE DE TEMPS EVENTULLEMENT REDECOUPE
C           YD      :
C           DY      :
C     OUT:
C           DALPHA  :  VARIABLE densite de dislocation
C           DGAMMA  :  GLISSEMENT PLASTIQUE DU SYSTEME IS
C           DP      :  ABS(DGAMMA)
C           IRET    :  CODE RETOUR
C ======================================================================

      IFL=NBCOMM(IFA,1)
      RMIN=R8MIEM()
      RMAX=SQRT(R8MAEM())
      
      B=     COEFT(IFL+1)
      H=     COEFT(IFL+2)
      DELTG0=COEFT(IFL+3)
      TAU0 = COEFT(IFL+4)
      D=     COEFT(IFL+5)
      GAMMA0=COEFT(IFL+6)
      N     =COEFT(IFL+7)
      BETA=  COEFT(IFL+8)
      YAT   =COEFT(IFL+9)
      DLAT=  COEFT(IFL+10)
      KF=    COEFT(IFL+11)
      KSELF= COEFT(IFL+12)
      TAUF=  COEFT(IFL+13)
      RHOMOB=COEFT(IFL+14)
      KBOLTZ=COEFT(IFL+15)
      DELTA1=COEFT(IFL+16)
      DELTA2=COEFT(IFL+17)
      DEPDT =COEFT(IFL+18)
      TEMP=  COEFT(IFL+19)
      MU    =COEFT(IFL+20)
      IRR   =NINT(COEFT(IFL+21))
      IF (IRR.GT.0) THEN
         AIRR  =COEFT(IFL+22)
C         XI    =COEFT(IFL+23)
      ENDIF

C     DELTA1=1 par defaut : nouvelle formulation. Sinon, ancienne
      NEW=(ABS(DELTA1-1.D0).LT.1.D-6)
      
C initialisation des arguments en sortie
      DGAMMA=0.D0
      DALPHA=0.D0
      DP=0.D0
      IRET=0
      
      LC=500.D0*B*(TEMP/300.D0)**2

      DO 55 IR=1,NBSYS
         RHOM(IR)=VIND(3*(IR-1)+1)
         RHOP(IR)=RHOM(IR)+DY(IR)
 55   CONTINUE
      
C     on resout en alpha=rho

C 1.  CALCUL de DeltaG approximatif
      RHOTOT=0.D0
      IF (NEW) THEN
C rho tot represente rho_f (foret)      
         DO 11 IR=1,12
            IF (IR.EQ.IS) GOTO 11
            RHOTOT=RHOTOT+RHOP(IR)
 11      CONTINUE
      ELSE
         DO 10 IR=1,12
            RHOTOT=RHOTOT+RHOP(IR)
 10      CONTINUE
      ENDIF
      IF (RHOTOT.LT.RMIN) THEN
         IRET=1
         GOTO 9999
      ENDIF
      IF (IRR.GT.0) THEN
         RHOIRR=VIND(3*NBSYS+IS)
         RHOTOT=RHOTOT+RHOIRR
      ENDIF

      IF (DEPDT.GT.RMIN) THEN
C        DEPDST FOURNI PAR L'UTILISATEUR
         DG=KBOLTZ*TEMP*LOG(RHOMOB*B*H/SQRT(RHOTOT)/DEPDT)
         DELTAG=MIN(DELTG0,DG)
      ELSEIF (DEPSDT.GT.RMIN) THEN
C        DEPSDT DU POINT DE GAUSS
         DG=KBOLTZ*TEMP*LOG(RHOMOB*B*H/SQRT(RHOTOT)/DEPSDT)
         DELTAG=MIN(DELTG0,DG)
      ELSE
         DELTAG=DELTG0
      ENDIF
      
C 2.  Calcul de Rs
      T1=1.D0-DELTAG/DELTG0
      IF (T1.LT.0.D0) THEN
         CALL ASSERT(.FALSE.)
      ELSEIF (T1.LT.RMIN) THEN
         RS=RMAX
      ELSE
         RS=MU*B/(2.D0*TAU0*T1*T1)
      ENDIF

C 3.  calcul de lambda
      D1=(D+2.D0*RS)*RHOTOT
      T2=MIN(SQRT(RHOTOT),D1)
      LAMBDA=1.D0/T2 - D
      
C 4.  calcul de Alpha-s_AT et Ls      
      ALPHAT=0.D0
      IF (NEW) THEN
         DO 21 IR=1,12
            IF (IR.EQ.IS) GOTO 21
            ALPHAT=ALPHAT+RHOP(IR)*HSR(IS,IR)
 21      CONTINUE
      ELSE
         DO 20 IR=1,12
            ALPHAT=ALPHAT+RHOP(IR)*HSR(IS,IR)
 20      CONTINUE
      ENDIF
      IF (ALPHAT.LT.RMIN) THEN
         IRET=1
         GOTO 9999
      ENDIF
      IF (IRR.EQ.1) THEN
         IF (ABS(AIRR).GT.RMIN) THEN
            ALPHAT=ALPHAT+AIRR*RHOIRR
         ENDIF
      ENDIF
      ALPHAT=SQRT(ALPHAT/RHOTOT)
      LS=MAX((LAMBDA-2.D0*ALPHAT*RS),LC)

C 5.  calcul de Taus_LT
      T3 = 2.D0*ALPHAT*RS+LC
      T4=1.D0/LAMBDA-1.D0/T3
      IF (NEW) THEN
         TAUSLT=MAX(0.D0,(ALPHAT*MU*B*T4))
      ELSE
         TAUSLT=MAX(0.D0,((1.D0-BETA)*ALPHAT*MU*B*T4))
      ENDIF

C 6.  calcul de Taus_LR
      IF (NEW) THEN
         TAUSLR=MU*B*SQRT(RHOP(IS)*HSR(IS,IS))
      ELSE
         TAUSLR=BETA*ALPHAT*MU*B*SQRT(RHOTOT)
      ENDIF
      
C 7.  calcul de Taus_eff
      IF (NEW) THEN
         TAUC=TAUF + SQRT( TAUSLT**2+TAUSLR**2)
         TAUEFF=ABS(TAUS)-TAUC
      ELSE
         TAUEFF=ABS(TAUS)-TAUF - TAUSLT - TAUSLR
      ENDIF
      
      IF (ABS(TAUS).GT.RMIN) THEN
         SGNS=TAUS/ABS(TAUS)
      ELSE
         SGNS=0.D0
      ENDIF
      
C 8.  calcul de gamma_nuc
      IF (TAUEFF.GT.TAU0) THEN
         IRET=1
         GOTO 9999
      ELSEIF (TAUEFF.LT.RMIN) THEN
         T5=0.D0
      ELSE 
         T5=SQRT(TAUEFF/TAU0)
      ENDIF
      GAMNUC=RHOMOB*B*H*LS*EXP(-DELTG0*(1.D0-T5)/KBOLTZ/TEMP)
      GAMNUC=GAMNUC*SGNS

C 9.  calcul de gamma_prob
      IF (NEW) THEN
         GAMPRO=GAMMA0*(ABS(TAUS)/TAUC)**N
      ELSE
         T6=TAUF+TAUSLR+(1.D0-BETA)*ALPHAT*MU*B/LAMBDA
         GAMPRO=GAMMA0*(ABS(TAUS)/T6)**N
      ENDIF
      GAMPRO=GAMPRO*SGNS

C 10. ECOULEMENT CALCUL DE DGAMMA,DP
      IF (ABS(GAMPRO).GT.RMIN) THEN
         GP1=1.D0/GAMPRO
      ELSE
         GP1=0.D0
      ENDIF
      IF (ABS(GAMNUC).GT.RMIN) THEN
         GP2=1.D0/GAMNUC
      ELSE
         GP2=0.D0
      ENDIF
      IF (ABS(GP1+GP2).GT.RMIN) THEN
         DGAMMA=1.D0/(GP1+GP2)*DT
         DP=ABS(DGAMMA)
      ENDIF

      IF (NEW) THEN
         T10=1.D0
         IF (TAUEFF.GT.RMIN) T10=(1.D0-DELTA1*TAUEFF/TAU0)
      ENDIF
      
C 11. CALCUL DE RHO_POINT RENOMME DALPHA
      IF (RHOP(IS).GT.RMIN) THEN
         IF (NEW) THEN
            T7= SQRT(HSR(IS,IS)*RHOP(IS))*T10
         ELSE
            T7= SQRT(HSR(IS,IS)*RHOP(IS))
         ENDIF
      ELSE
         T7=0.D0
C        ou bien IRET=1, a voir  
      ENDIF
      
      IF (NEW) THEN
         T8=ALPHAT*RHOTOT*LAMBDA*T10      
      ELSE
         T8=0.D0
         DO 30 IR=1,12
            IF (IR.EQ.IS) GOTO 30
            IF (TAUEFF.GT.RMIN) THEN
               ASR=HSR(IS,IR)*(1.D0-DELTA2*TAUEFF/TAU0)
            ELSE
               ASR=HSR(IS,IR)
            ENDIF
            IF (RHOP(IR).GT.RMIN) THEN
               T8=T8+SQRT(ASR*RHOP(IR))
            ENDIF
 30      CONTINUE
      ENDIF
      
      IF (TAUEFF.GT.RMIN) THEN
         T9=1.D0/YAT+DELTA2*2.D0*R8PI()*TAUEFF/MU/B
      ELSE
         T9=1.D0/YAT
      ENDIF
      YS=1.D0/T9
      HS=1.D0/DLAT+T7/KSELF+T8/KF-YS*RHOP(IS)
      DALPHA=HS*DP/B

 9999 CONTINUE
C 12. irradiation mise ajout dans LCDPEC / LCDPEQ      
      END
