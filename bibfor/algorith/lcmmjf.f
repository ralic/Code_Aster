        SUBROUTINE LCMMJF( FAMI,KPG,KSP,TAUS,COEFT,MATERF,IFA,NMAT,
     &    NBCOMM,DT,NECOUL,IS,IR,NBSYS,VIND,DY,HSR,RP,ALPHAP,DALPHA,
     &    GAMMAP,DGAMMA,SGNR,DFDTAU,DFDAL,DFDR,IRET)
        IMPLICIT NONE
        INTEGER KPG,KSP,IFA,NMAT,NBCOMM(NMAT,3),NUMHSR
        REAL*8 TAUS,COEFT(NMAT),RP,DT,ALPHAP,DALPHA,GAMMAP,DGAMMA
        REAL*8 DFDTAU,DFDAL,DFDR,HSR(5,24,24),DY(*),VIND(*),MATERF(NMAT)
        CHARACTER*(*) FAMI
        CHARACTER*16 NECOUL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C TOLE CRP_21
C MODIF ALGORITH  DATE 22/10/2007   AUTEUR PROIX J-M.PROIX 
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
C       IN  FAMI    :  FAMILLE DU POINT DE GAUSS
C           KPG     :  POINT DE GAUSS
C           KSP     :  SOUS-POINT DE GAUSS
C           TAUS    :  SCISSION REDUITE
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
C           DFDTAU  :  dF/dTau
C           DFDAL   :  dF/dAlpha
C           DFDR    :  dF/dR
C     ----------------------------------------------------------------
      REAL*8 C,P,R0,Q,H,B,K,N,FTAU,CRIT,B1,B2,Q1,Q2,A,GAMMA0,D
      REAL*8 TPERD,TABS,DRDP,ALPHA,GAMMA,DP,TAUMU,TAUV,GM,PM,CC
      REAL*8 SGNR,PR,DRDPR,DELTAV,DELTAG,SGNS,R8MIEM,AUX
      REAL*8 TAUR,TAU0,TAUEF,BSD,GCB,KDCS,R,INT1,INT2,INT3,DFDTMU
      REAL*8 DTMUDR,SOM,DRDGAM,CISA2, RR

      INTEGER IFL,NS, IS, NBSYS,IU,IR,IRET,NUECOU,NULHSR,IRET2
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)
      NUECOU=COEFT(IFL)
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
             DFDTAU=(N*DT/(K**N))*(CRIT**(N-1))
          ELSE
             DFDTAU=0.D0
          ENDIF

C         DFDAL
          DFDAL=-C*DFDTAU

C         DFDR
          IF (ABS(FTAU).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=FTAU/ABS(FTAU)
          ENDIF
          DFDR=-SGNS*DFDTAU

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
             DFDTAU=(N*DT/(K**N))*(CRIT**(N-1))
          ELSE
             DFDTAU=0.D0
          ENDIF

C         DFDAL
          DFDAL=(-C*SGNS+D*ALPHAP*DALPHA)*DFDTAU*SGNS

C         DFDR
          DFDR=-SGNS*DFDTAU


C      ELSEIF (NECOUL.EQ.'ECOU_VISC3') THEN
      ELSEIF (NUECOU.EQ.3) THEN
      
          K      =COEFT(IFL+1)
          TAUMU  =COEFT(IFL+2)
          GAMMA0 =COEFT(IFL+3)
          DELTAV =COEFT(IFL+4)
          DELTAG =COEFT(IFL+5)
          TPERD  =COEFT(IFL+6)
          CRIT=ABS(TAUS)-TAUMU
          IF (ABS(TAUS).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=(TAUS)/ABS(TAUS)
          ENDIF
          
          IF (CRIT.LT.0.D0) THEN
             DFDTAU=0.D0
          ELSE
               TABS=TPERD+273.15D0
               DFDTAU=GAMMA0*DELTAV*K*TABS*EXP(-DELTAG/K/TABS)
     &                *EXP(DELTAV/K/TABS*CRIT-1.D0)*(DELTAV/K/TABS*CRIT)
          ENDIF

C         DFDAL
          DFDAL=0.D0

C         DFDR
          DFDR=0.D0


      ELSEIF (NUECOU.EQ.4) THEN
          
          K         =COEFT(IFL+1)
          TAUR      =COEFT(IFL+2)
          TAU0      =COEFT(IFL+3)
          GAMMA0    =COEFT(IFL+4)
          DELTAG    =COEFT(IFL+5)
          BSD       =COEFT(IFL+6)
          GCB       =COEFT(IFL+7)
          KDCS      =COEFT(IFL+8)
          P         =COEFT(IFL+9)
          Q         =COEFT(IFL+10)
          NUMHSR    =COEFT(IFL+12)
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
             DTMUDR = 0.D0
             DFDAL  = 0.D0

             DO 1 IU = 1, NBSYS
               R     = VIND(3*(IU-1)+1)
               TAUMU = TAUMU +  HSR(NUMHSR,IS,IU)*R
 1           CONTINUE

             TAUMU = CISA2 * TAUMU/TAUV

             TAUEF = TAUV-TAUMU

             IF (TAUEF.GT.0.D0) THEN

                AUX= (1.D0-((TAUV-TAUMU)/TAUR)**P)
                IF (AUX.LE.0.D0)  THEN
                   IRET=1
                   GOTO 9999
                ENDIF

               INT1 = (1.D0-((TAUV-TAUMU)/TAUR)**P)**(Q-1)
               INT2 = ((TAUV-TAUMU)/TAUR)**(P-1)
               INT3 = 1.D0+TAUMU/TAUV

               TABS=TPERD+273.15D0
               DFDTAU = DT*GAMMA0*DELTAG*Q*P/K/TABS/TAUR*
     &         EXP(-DELTAG/K/TABS*(1.D0-(((TAUV-TAUMU)/TAUR)**P))**Q)
     &             *INT1*INT2*INT3

               DFDTMU = -DT*GAMMA0*DELTAG*Q*P/K/TABS/TAUR*
     &         EXP(-DELTAG/K/TABS*(1.D0-(((TAUV-TAUMU)/TAUR)**P))**Q)
     &         *INT1*INT2*SGNS

               IF(IR.NE.0) THEN
                 RR  = VIND(3*(IR-1)+1)
                 DO 2 IU = 1, NBSYS
                    R  = VIND(3*(IU-1)+1)
                    SOM    = SOM  + R
  2              CONTINUE
                 SOM    = SOM  - RR
                 SOM = SQRT(SOM)
C
                 DTMUDR = CISA2 * HSR(NUMHSR,IS,IR)/TAUV

                 DRDGAM = SGNR/(1.D0+GCB*ABS(DGAMMA))**2*
     &                  (BSD+SOM/KDCS-GCB*VIND(3*(IR-1)+1))

                 DFDAL = DFDTMU*DTMUDR*DRDGAM
               ENDIF
             ELSE
                DFDTAU=0.D0
                DFDAL=0.D0
             ENDIF
          ELSE
             DFDTAU=0.D0
             DFDAL=0.D0
          ENDIF

C         DFDR
          DFDR=0.D0
      ENDIF
9999  CONTINUE
      END
