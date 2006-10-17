        SUBROUTINE LCMMJF( TAUS,COEFT,IFA,NMAT,NBCOMM,DT,NECOUL,
     &     TPERD,RP,ALPHAP,DALPHA,GAMMAP,DGAMMA,
     &     DFDTAU,DFDAL,DFDR)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3)
        REAL*8 TAUS,COEFT(NMAT),RP,DT,ALPHAP,DALPHA,GAMMAP,DGAMMA
        REAL*8 DFDTAU,DFDAL,DFDR
        CHARACTER*16 NECOUL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2006   AUTEUR JMBHH01 J.M.PROIX 
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
C           TPERD   :  TEMPERATURE
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
      REAL*8 PR,DRDPR,DELTAV,DELTAG,SGNS,R8MIEM
      INTEGER IFL,NS, IS
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IFL=NBCOMM(IFA,1)

      IF (NECOUL.EQ.'ECOU_VISC1') THEN
          N=COEFT(IFL-1+1)
          K=COEFT(IFL-1+2)
          C=COEFT(IFL-1+3)

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
          
      ELSEIF (NECOUL.EQ.'ECOU_VISC2') THEN
          N=COEFT(IFL-1+1)
          K=COEFT(IFL-1+2)
          C=COEFT(IFL-1+3)
          A=COEFT(IFL-1+4)
          D=COEFT(IFL-1+5)

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
          
          
      ELSEIF (NECOUL.EQ.'ECOU_VISC3') THEN
          K      =COEFT(IFL-1+1)
          TAUMU  =COEFT(IFL-1+2)
          GAMMA0 =COEFT(IFL-1+3)
          DELTAV =COEFT(IFL-1+4)
          DELTAG =COEFT(IFL-1+5)

          CRIT=ABS(TAUS)-TAUMU
          IF (ABS(TAUS).LE.R8MIEM()) THEN
             SGNS=1.D0
          ELSE
             SGNS=(TAUS)/ABS(TAUS)
          ENDIF       
          
          IF (CRIT.LT.0.D0) THEN
             DFDTAU=0.D0
          ELSE
             TABS=TPERD+273.5D0

             DFDTAU=GAMMA0*DELTAV*K*TABS*EXP(-DELTAG/K/TABS)
     &              *EXP(DELTAV/K/TABS*CRIT-1.D0)*(DELTAV/K/TABS*CRIT)
          ENDIF
          
C         DFDAL          
          DFDAL=0.D0
          
C         DFDR
          DFDR=0.D0         

       ENDIF

      END
