        SUBROUTINE LCMMJF( TAUS,COEFT,MATERF,IFA,NMAT,
     &    NBCOMM,DT,NECOUL,IS,IR,NBSYS,VIND,DY,NFS,NSG,HSR,RP,ALPHAP,
     &    DALPHA,GAMMAP,DGAMMA,SGNS,DGDTAU,DGDAL,DFDR,PETITH,IRET)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3), NFS,NSG
        REAL*8 TAUS,COEFT(NMAT),RP,DT,ALPHAP,DALPHA,GAMMAP,DGAMMA,DGDTAU
        REAL*8 DGDAL,DFDR,HSR(NSG,NSG),DY(*),VIND(*),MATERF(NMAT)
        CHARACTER*16 NECOUL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C TOLE CRP_21
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE PROIX J.M.PROIX
C ======================================================================
C  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
C  POUR LA LOI D'ECOULEMENT
C       IN  TAUS    :  SCISSION REDUITE
C           COEFT   :  PARAMETRES MATERIAU
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           IFA     :  NUMERO DE FAMILLE
C           NMAT   :  DIMENSION MATER
C           NBCOMM :  INCIDES DES COEF MATERIAU
C           DT     :  ACCROISSEMENT INSTANT ACTUEL
C           NECOUL  :  NOM DE LA LOI D'ECOULEMENT
C           IS     :  NUMERO DU SYST. GLIS. S
C           IR     :  NUMERO DU SYST. GLIS. R
C           NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT FAMILLE IFA
C           HSR    :  MATRICE D'INTERACTION
C           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
C           RP     :  ECROUISSAGE
C           ALPHAP :  ECR. CINEMATIQUE
C           DALPHA :  ACCR. ECR. CINEMATIQUE
C           GAMMAP :  GLISSEMENT PLASTIQUE INSTANT ACTUEL
C           DGAMMA :  ACCR. GLISSEMENT PLASTIQUE
C     OUT:
C           SGNS=FTAU/ABS(FTAU)
C           DGDTAU  :  dF/dTau
C           DGDAL   :  dF/dAlpha  (particulier pour KR)
C           DFDR    :  dF/dR (ou dhs/dalphar pour KR)
C           PETITH  :  hs pour KR
C       OUT IRET   :  CODE RETOUR
C     ----------------------------------------------------------------
      REAL*8 C,P,Q,K,N,FTAU,CRIT,A,GAMMA0,D
      REAL*8 TPERD,TABS,ALPHA,TAUMU,TAUV
      REAL*8 DELTG0,SGNS,R8MIEM,AUX,ALPHAS
      REAL*8 TAUR,TAU0,TAUEF,BSD,GCB,KDCS
      REAL*8 SOM,CISA2,DELTGG,TERME,PETITG,CS
      REAL*8 DTEDTO,DGGDTO,DTEDAL,DGGDAL,DELTSR,DHDAL,PETITH

      INTEGER IFL, IS, NBSYS,IU,IR,IRET,NUECOU
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)
      NUECOU=NINT(COEFT(IFL))
      IRET=0

C      IF (NECOUL.EQ.'MONO_VISC1') THEN
      IF (NUECOU.EQ.1) THEN

          N=COEFT(IFL+1)
          K=COEFT(IFL+2)
          C=COEFT(IFL+3)

          FTAU=TAUS-C*ALPHAP
          CRIT=ABS(FTAU)-RP

C         dF/dTau

          IF (CRIT.GT.0.D0) THEN
             DGDTAU=(N*DT/(K**N))*(CRIT**(N-1))
             DGDAL=-C*DGDTAU
             DFDR=-SGNS*DGDTAU
          ELSE
             DGDTAU=0.D0
             DGDAL=0.D0
             DFDR=0.D0
          ENDIF

          IF (ABS(FTAU).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=FTAU/ABS(FTAU)
          ENDIF

C      IF (NECOUL.EQ.'MONO_VISC2') THEN
      ELSEIF (NUECOU.EQ.2) THEN

          N=COEFT(IFL+1)
          K=COEFT(IFL+2)
          C=COEFT(IFL+3)
          A=COEFT(IFL+4)
          D=COEFT(IFL+5)

          FTAU=TAUS-C*ALPHAP-A*GAMMAP

          CRIT=ABS(FTAU)-RP + 0.5D0*C*D*ALPHAP**2
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

      ELSEIF (NUECOU.EQ.4) THEN
C             MATRICE JACOBIENNE DU SYSTEME :
C  R1 = D-1*SIGMA - (D_M-1*SIGMA_M)-(DEPS-DEPS_TH)+Somme(ms*Gamma_s)=0
C  R2 = dALPHA - g(Taus,alphas)*h(alphas)
C avec Gamma_s=g(Taus,alphas)*sgn(taus)
C
C ON VEUT CALCULER :
C        dg(taus,alphas)/dtaus
         DGDTAU=0.D0
C        dg(taus,alphas)/dalphar
         DGDAL=0.D0
C        dh(alphas)/dalphar
         DHDAL=0.D0
C        DFDR=DHDAL
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
            SGNS=TAUS/ABS(TAUS)
         ENDIF
         IF (TAUV.GT.0.D0) THEN
            SOM    = 0.D0
            TAUMU  = 0.D0
            DGDAL  = 0.D0
            DO 1 IU = 1, NBSYS
              ALPHA = VIND(3*(IU-1)+1)+DY(IU)
C             PARTIE POSITIVE DE ALPHA
              IF (ALPHA.GT.0.D0) THEN
                 TAUMU = TAUMU +  HSR(IS,IU)*ALPHA
                 IF (IU.NE.IS) SOM = SOM+ALPHA
              ENDIF
 1          CONTINUE
            ALPHAS= VIND(3*(IS-1)+1)+DY(IS)
            SOM=SQRT(SOM)
            TAUMU = CISA2 * TAUMU/TAUV
            TAUEF = TAUV-TAUMU
            IF (TAUEF.GT.0.D0) THEN
               AUX= (1.D0-(TAUEF/TAUR)**P)
               IF (AUX.LE.0.D0)  THEN
                  IRET=1
                  GOTO 9999
               ENDIF
               TABS=TPERD+273.15D0
C              PROTECTION DE l'EXPONENTIELLE
               DELTGG=DELTG0*(AUX**Q)
               TERME=-DELTGG/K/TABS
               IF (TERME.GT.10.D0) THEN
                  IRET=1
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

C              CALCUL DE dgs/dalphar
               DTEDAL=-CISA2/TAUV*HSR(IS,IR)
               DGGDAL=CS*DTEDAL
               DGDAL=-PETITG/K/TABS*DGGDAL

C              CALCUL DE dhs/dalphar
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
            ENDIF
         ENDIF
      ENDIF
9999  CONTINUE
      END
