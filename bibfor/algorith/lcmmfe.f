        SUBROUTINE LCMMFE( FAMI,KPG,KSP,TAUS,COEFT,MATERF,IFA,NMAT,
     &      NBCOMM,NECOUL,IS,NBSYS,VIND,DY,RP,ALPHAP,GAMMAP,DT,DALPHA,
     &      DGAMMA,DP,CRIT,SGNS,HSR,IRET,DAL)
        IMPLICIT NONE
        INTEGER KPG,KSP,IFA,NMAT,NBCOMM(NMAT,3),IRET,NUMHSR
        REAL*8 TAUS,COEFT(NMAT),ALPHAP,DGAMMA,DP,DT,DTIME,TAUMU,TAUV
        REAL*8 RP,SGNS,HSR(5,24,24),DY(*),VIND(*),MATERF(NMAT),DAL(*)
        CHARACTER*(*) FAMI
        CHARACTER*16 NECOUL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/04/2007   AUTEUR PROIX J-M.PROIX 
C TOLE CRP_21
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
C RESPONSABLE JMBHH01 J.M.PROIX
C ======================================================================
C  COMPORTEMENT MONOCRISTALLIN : ECOULEMENT (VISCO)PLASTIQUE
C  INTEGRATION DES LOIS MONOCRISTALLINES
C       IN  FAMI    :  FAMILLE DU POINT DE GAUSS
C           KPG     :  POINT DE GAUSS
C           KSP     :  SOUS-POINT DE GAUSS
C           TAUS    :  SCISSION REDUITE
C           COEFT   :  PARAMETRES MATERIAU
C           IFA     :  NUMERO DE FAMILLE
C           NMAT    :  NOMBRE MAXI DE MATERIAUX
C           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NECOUL  :  NOM DE LA LOI D'ECOULEMENT
C           RP      :  R(P) FONCTION D'ECROUISSAGE ISTROPE
C           ALPHAP  :  ALPHA A T ACTUEL
C           GAMMAP  :  GAMMA A T ACTUEL
C           DT      :  INTERVALLE DE TEMPS EVENTULLEMENT REDECOUPE
C     OUT:
C           DGAMMA  :  DEF PLAS
C           DALPHA  :  VARIABLE r pour Kocks-Rauch
C           DP      :  DEF PLAS CUMULEE
C           CRIT    :  CRITERE
C           SGNS    :  SIGNE DE GAMMA
C           IRET    :  CODE RETOUR
C    VAR    DAL    :  dr
C ======================================================================

C     ----------------------------------------------------------------
      REAL*8 C,P,R0,Q,H,B,K,N,FTAU,CRIT,B1,B2,Q1,Q2,A,GAMMA0,D
      REAL*8 TEMPF,TABS,PR,DELTAV,DELTAG,GAMMAP,R8MIEM,PTIT
      REAL*8 TAUR,TAU0,TAUEF,BSD,GCB,R,KDCS,SOM,DALPHA
      REAL*8 AUX,CISA2,RACR
      INTEGER IFL,IEI,TNS,NS,IS,IU,NBSYS
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)
      IRET=0
      PTIT=R8MIEM()
      CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TEMPF,IRET)
C-------------------------------------------------------------
C     POUR UN NOUVEAU TYPE D'ECOULEMENT, CREER UN BLOC IF
C------------------------------------------------------------

      IF (NECOUL.EQ.'ECOU_VISC1') THEN
          N=COEFT(IFL-1+1)
          K=COEFT(IFL-1+2)
          C=COEFT(IFL-1+3)

          FTAU=TAUS-C*ALPHAP
          IF (ABS(FTAU).LT.PTIT) THEN
             SGNS=1.D0
          ELSE
             SGNS=FTAU/ABS(FTAU)
          ENDIF
          CRIT=ABS(FTAU)-RP
          IF (CRIT.GT.0.D0) THEN
             DP=((CRIT/K)**N)*DT
             DGAMMA=DP*SGNS
          ELSE
             DP=0.D0
             DGAMMA=0.D0
          ENDIF

      ENDIF

      IF (NECOUL.EQ.'ECOU_VISC2') THEN
          N=COEFT(IFL-1+1)
          K=COEFT(IFL-1+2)
          C=COEFT(IFL-1+3)
          A=COEFT(IFL-1+4)
          D=COEFT(IFL-1+5)

          FTAU=TAUS-C*ALPHAP-A*GAMMAP
          IF (ABS(FTAU).LT.PTIT) THEN
             SGNS=1.D0
          ELSE
             SGNS=FTAU/ABS(FTAU)
          ENDIF

          CRIT=ABS(FTAU)-RP + 0.5D0*D*C*ALPHAP**2
          IF (CRIT.GT.0.D0) THEN
             DP=((CRIT/K)**N)*DT
             DGAMMA=DP*SGNS
          ELSE
             DP=0.D0
             DGAMMA=0.D0
          ENDIF
      ENDIF

      IF (NECOUL.EQ.'ECOU_VISC3') THEN
          K      =COEFT(IFL-1+1)
          TAUMU  =COEFT(IFL-1+2)
          GAMMA0 =COEFT(IFL-1+3)
          DELTAV =COEFT(IFL-1+4)
          DELTAG =COEFT(IFL-1+5)

          TAUV=ABS(TAUS)-TAUMU
          IF (ABS(TAUS).LT.PTIT) THEN
             SGNS=1.D0
          ELSE
             SGNS=TAUS/ABS(TAUS)
          ENDIF
          IF (TAUV.GT.0.D0) THEN
             TABS=TEMPF+273.5D0
             DP=GAMMA0*EXP(-DELTAG/K/TABS)*EXP(DELTAV/K/TABS*TAUV)
             DGAMMA=DP*TAUS/ABS(TAUS)
          ELSE
             DP=0.D0
             DGAMMA=0.D0
          ENDIF
       ENDIF

      IF (NECOUL.EQ.'KOCKS_RAUCH') THEN
          K         =COEFT(IFL-1+1)
          TAUR      =COEFT(IFL-1+2)
          TAU0      =COEFT(IFL-1+3)
          GAMMA0    =COEFT(IFL-1+4)
          DELTAG    =COEFT(IFL-1+5)
          BSD       =COEFT(IFL-1+6)
          GCB       =COEFT(IFL-1+7)
          KDCS      =COEFT(IFL-1+8)
          P         =COEFT(IFL-1+9)
          Q         =COEFT(IFL-1+10)
          NUMHSR=COEFT(IFL-1+11)

          CISA2 = (MATERF(1)/2.D0/(1.D0+MATERF(2)))**2

          TAUV=ABS(TAUS)-TAU0
          CRIT=TAUV
          
          IF (ABS(TAUS).LT.PTIT) THEN
             SGNS=1.D0
          ELSE
             SGNS=TAUS/ABS(TAUS)
          ENDIF

           IF (TAUV.GT.0.D0) THEN

           TABS=TEMPF+273.5D0
           SOM=0.D0
           TAUMU=0.D0

             DO 1 IU = 1, NBSYS
             R=VIND(3*(IU-1)+1)+DAL(IU)
             TAUMU = TAUMU +  HSR(NUMHSR,IS,IU)*R
               IF (IU.NE.IS) THEN
                   RACR=SQRT(R)
               ELSE
                   RACR=0.D0
               ENDIF
             SOM = SOM+RACR
  1          CONTINUE

             TAUMU = CISA2 * TAUMU/TAUV

             TAUEF = TAUV-TAUMU

             CRIT=TAUEF

             IF (TAUEF.GT.0.D0) THEN

               AUX= (1.D0-((TAUV-TAUMU)/TAUR)**P)

               IF (AUX.LE.0.D0) THEN
               IRET=1
               GOTO 9999
               ENDIF

               DGAMMA=GAMMA0*EXP(-DELTAG/K/TABS*
     &        (1.D0-(((TAUV-TAUMU)/TAUR)**P))**Q)*SGNS*DT

               DP=ABS(DGAMMA)

               DALPHA=ABS(DGAMMA)/(1.D0+GCB*ABS(DGAMMA))*
     &               (BSD+SOM/KDCS-GCB*VIND(3*(IS-1)+1))
               DAL(IS)=DALPHA

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
       ENDIF
9999   CONTINUE
      END
