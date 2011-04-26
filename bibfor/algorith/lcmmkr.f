        SUBROUTINE LCMMKR(  TAUS, COEFT, CISA2,   IFA,  NMAT,
     &                    NBCOMM,    IS, NBSYS,   HSR,  VIND,
     &                        DY,    DT,DALPHA,DGAMMA,    DP,
     &                      CRIT,  SGNS,  IRET               )

        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3),IRET,NUMHSR
        REAL*8 TAUS,COEFT(NMAT),DGAMMA,DP,DT,TAUMU,TAUV
        REAL*8 SGNS,HSR(5,24,24),DY(*)
        REAL*8 VIND(*),DALPHA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C ======================================================================
C  COMPORTEMENT MONOCRISTALLIN : ECOULEMENT (VISCO)PLASTIQUE
C  INTEGRATION DE LA LOIS MONOCRISTALLINES KOCKS-RAUCH. CALCUL DE DALPHA
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
C           DALPHA  :  VARIABLE dalpha pour Kocks-Rauch
C           DGAMMA  :  DEF PLAS
C           DP      :  DEF PLAS CUMULEE
C           CRIT    :  CRITERE
C           SGNS    :  SIGNE DE GAMMA
C           IRET    :  CODE RETOUR
C ======================================================================
C             RESOLUTION DU SYSTEME :
C  R1 = D-1*SIGMA - (D_M-1*SIGMA_M)-(DEPS-DEPS_TH)+Somme(ms*Gamma_s)=0
C      avec Gamma_s=g(Taus,alphas)
C  R2 = dALPHA - |g(Taus,alphas)|*h(alphas)
C     ----------------------------------------------------------------
      REAL*8 P,Q,K,CRIT,GAMMA0
      REAL*8 TEMPF,TABS,DELTAG,R8MIEM,PTIT
      REAL*8 TAUR,TAU0,TAUEF,BSD,GCB,ALPHAR,KDCS,SOM,DELTGG
      REAL*8 AUX,CISA2,ALPHAS,TERME,PETITH,PETITG
      INTEGER IFL,IS,IU,NBSYS
C     ----------------------------------------------------------------
C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)
      IRET=0
      PTIT=R8MIEM()
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
      TEMPF     =COEFT(IFL+11)
      NUMHSR    =NINT(COEFT(IFL+12))
      TAUV=ABS(TAUS)-TAU0
      CRIT=TAUV


C      VARIABLE INTERNE PRINCIPALE : ALPHA
      IRET=0
      IF (ABS(TAUS).LE.PTIT) THEN
         SGNS=1.D0
      ELSE
         SGNS=TAUS/ABS(TAUS)
      ENDIF

      IF (TAUV.GT.0.D0) THEN

         SOM=0.D0
         TAUMU=0.D0
         ALPHAS=VIND(3*(IS-1)+1)+DY(IS)
         DO 1 IU = 1, NBSYS
            ALPHAR=VIND(3*(IU-1)+1)+DY(IU)
C           PARTIE POSITIVE DE ALPHA
            IF (ALPHAR.GT.0.D0) THEN
               TAUMU = TAUMU +  HSR(NUMHSR,IS,IU)*ALPHAR
               IF (IU.NE.IS) SOM = SOM+ALPHAR
            ENDIF
  1      CONTINUE

         SOM=SQRT(SOM)
         TAUMU = CISA2 * TAUMU/TAUV
         TAUEF = TAUV-TAUMU
         CRIT=TAUEF
         IF (TAUEF.GT.0.D0) THEN
           AUX= (1.D0-(TAUEF/TAUR)**P)
           IF (AUX.LE.0.D0) THEN
              IRET=1
C              print *,'attention, lcmmkr,AUX=',AUX
              GOTO 9999
           ENDIF
C          PROTECTION DE l'EXPONENTIELLE
           TABS=TEMPF+273.15D0
           DELTGG=DELTAG*(AUX**Q)
           TERME=-DELTGG/K/TABS
           IF (TERME.GT.10.D0) THEN
              IRET=1
C              print *,'attention, lcmmkr,TERME=',TERME
              GOTO 9999
           ENDIF
           PETITG=GAMMA0*EXP(TERME)*DT
           DGAMMA=PETITG*SGNS
           DP=PETITG
           PETITH=BSD+SOM/KDCS-GCB*ALPHAS
C          LES VALEURS NEGATIVES DE PETITH
C          SONT INTERDITES (CELA DEVRAIT ETRE DANS LE MODELE)
           IF (PETITH.LT.0.D0) PETITH=0.D0
           DALPHA=PETITG*PETITH
         ELSE
           DGAMMA=0.D0
           DP=0.D0
           DALPHA=0.D0
         ENDIF
      ELSE
         DGAMMA=0.D0
         DP=0.D0
         DALPHA=0.D0
      ENDIF
 9999 CONTINUE
      END
