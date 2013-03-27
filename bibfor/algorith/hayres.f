      SUBROUTINE HAYRES ( MOD,NMAT,MATERD,MATERF,TIMED,TIMEF,
     &                    YD,YF,DEPS,DY,RES,CRIT,IRET)
      IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2013   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C     HAYHURST :
C            CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = RES(DY)
C                   DY  = ( DEPSILON_EL DP DH1 DH2 DPHI DD )
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YD     :  VARIABLES A T      = ( SIGD  X1D X2D PD RD QD(..))
C           YF     :  VARIABLES A T + DT = ( SIGF  X1F X2F PF RF QF(..))
C           DY     :  SOLUTION ESSAI     = ( DSIG  DX1 DX2 DP DR DQ(..))
C           EPSD   :  DEFORMATION A T
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT RES    :  SYSTEME NL A T + DT
C       ----------------------------------------------------------------
      CHARACTER*8 MOD
      INTEGER IRET,ITENS,NDI,NMAT,NVI,NDT,NDIM
      REAL*8 HOOKF(6,6),RES(10),CRIT(*),THETA,ALPHAD
      REAL*8 MATERD(NMAT,2),MATERF(NMAT,2),TIMED,TIMEF,DEPS(6),DT,DTOT
      REAL*8 YD(*),YF(*),DY(*),SIGF(6),ECROU(2),GH,DMG,DMGMI,EPSED(6)
      REAL*8 DEPSP(6),DEVCUM,DECROU(2),DDMG,EPSEF(6),DEPSEL(6),M13,W(6)
      REAL*8 ZE,TD,SINN,GRJ0,GH1,GH2,EQUI(17),R8MIEM,RMIN,R8PREM,SEQUID
      REAL*8 EPS0,PK,PH1,PH2,DELTA1,DELTA2,H1ST,H2ST,PKC,SIG0,BIGA
      REAL*8 TRSIG,GRJ2V,GRJ1,EPSI,TERME1,SHMAX,SEQUI,DDDMG,DH1,DH2,DP
C     ----------------------------------------------------------------
      PARAMETER(ZE=0.0D0)
      PARAMETER(TD=1.5D0)
C
      COMMON /TDIM/   NDT,    NDI
C-----------------------------------------------------------------------
      THETA=CRIT(4)
      CALL LCEQVN(1,YD(8),GH1)
      CALL LCEQVN(1,YD(9),GH2)
      CALL LCEQVN(1,DY(7),DP)
      CALL LCEQVN(1,DY(8),DH1)
      CALL LCEQVN(1,DY(9),DH2)
      CALL LCEQVN(1,DY(10),DDDMG)
      DO 11 ITENS=1,NDT
        EPSEF(ITENS)=YD(ITENS)+THETA*DY(ITENS)
   11 CONTINUE
      DT=TIMEF-TIMED
      IF (NDT.EQ.6)THEN
         NDIM=3
      ELSEIF (NDT.EQ.4)THEN
         NDIM=2
         SIGF(5)=0.D0
         SIGF(6)=0.D0
         DEPSP(5)=0.D0
         DEPSP(6)=0.D0
      ENDIF
      IRET=0
      RMIN=R8MIEM()
      SHMAX=50.D0
      EPS0   = MATERF(1,2)
      PK     = MATERF(2,2)
      PH1    = MATERF(3,2)
      PH2    = MATERF(4,2)
      DELTA1 = MATERF(5,2)
      DELTA2 = MATERF(6,2)
      H1ST   = MATERF(7,2)
      H2ST   = MATERF(8,2)
      BIGA   = MATERF(9,2)
      SIG0   = MATERF(10,2)
      PKC    = MATERF(11,2)
      ALPHAD = MATERF(12,2)
      SEQUID = MATERF(13,2)
      EPSI=R8PREM()*PK
      GH=GH1+THETA*DH1+GH2+THETA*DH2
      M13=-1.D0/3.D0
      DMGMI=1.D0-(1.D0+PKC*TIMEF)**M13
      DMG=YD(10)+THETA*DDDMG

C----------------------------------------------------------------
      DTOT=(1.D0-DMG)
      CALL LCOPLI('ISOTROPE',MOD,MATERF(1,1),HOOKF)
      CALL LCPRMV ( HOOKF , EPSEF , SIGF)
      CALL LCPRSV ( DTOT, SIGF, SIGF)
C
C------------CALCUL DES INVARIANTS DE CONTRAINTE  -------
C     attention FGEQUI ne prend pas en compte les SQRT(2)
      CALL DSCAL(3,1.D0/SQRT(2.D0),SIGF(4),1)
      CALL FGEQUI(SIGF,'SIGM_DIR',NDIM,EQUI)
C     on retablit le tenseur
      CALL DSCAL(3,SQRT(2.D0),SIGF(4),1)
      TRSIG=EQUI(16)
      GRJ0=MAX(EQUI(3),EQUI(4))
      GRJ0=MAX(GRJ0,EQUI(5))
      GRJ1= TRSIG
      GRJ2V=EQUI(1)
      IF(SEQUID.EQ.0.D0) THEN
         SEQUI=GRJ0
      ELSE
         SEQUI=GRJ1
      ENDIF
C------------ CALCUL DU TENSEUR DEPSPATORIQUE DES CONTRAINTES ---
      DO 10 ITENS=1,NDT
        IF (ITENS.LE.3) SIGF(ITENS)=SIGF(ITENS)-GRJ1/3.D0
   10 CONTINUE
C
C----- EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
C----- CUMULEE
C
      TERME1=(GRJ2V*(1-GH))/(PK*(1-DMGMI)*DTOT)
      IF (GRJ2V .LE. EPSI) THEN
         DEVCUM=ZE
      ELSEIF (ABS(TERME1).LT.SHMAX) THEN
         DEVCUM=EPS0*(SINH(TERME1))*DT
      ELSE
         IRET=1
         GOTO 9999
      ENDIF
C
C----- EQUATION DONNANT LA DERIVEE DE GH
C
      IF (GRJ2V .LE. EPSI) THEN
C       DIVISION PAR ZERO EVITEE
        DECROU(1)=ZE
        DECROU(2)=ZE
      ELSE
        IF (GH1.LE.(H1ST-RMIN)) THEN
          DECROU(1)=(PH1/GRJ2V)*(H1ST-(DELTA1*(GH1+THETA*DH1)))*DP
          DECROU(2)=(PH2/GRJ2V)*(H2ST-(DELTA2*(GH2+THETA*DH2)))*DP
        ELSE
           IRET=1
           GOTO 9999
        ENDIF
      ENDIF
C
C----- EQUATION DONNANT LA DERIVEE DE L ENDOMMAGEMENT
C
      IF (SEQUI .GE. ZE) THEN
        SINN=ALPHAD*SEQUI+((1.D0-ALPHAD)*GRJ2V)
      ELSE
        SINN=(1.D0-ALPHAD)*GRJ2V
      ENDIF
      IF ((SINN/SIG0).LT.SHMAX) THEN
         DDMG=BIGA*SINH(SINN/SIG0)*DT
      ELSE
         IRET=1
         GOTO 9999
      ENDIF
C     
C------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
C
      IF (GRJ2V .LE. EPSI) THEN
         DO 33 ITENS=1,NDT
           DEPSP(ITENS)=ZE
   33    CONTINUE
      ELSE   
         DO 12 ITENS=1,NDT
           DEPSP(ITENS)=TD*DP*SIGF(ITENS)/GRJ2V
   12    CONTINUE
      ENDIF
      CALL LCDIVE ( DEPS  , DEPSP   , DEPSEL )
      CALL LCDIVE ( DEPSEL, DY, RES(1) )
      RES(NDT+1)  = DEVCUM-DP
      RES(NDT+2)  = DECROU(1)-DH1
      RES(NDT+3)  = DECROU(2)-DH2
      RES(NDT+4)  = DDMG-DDDMG
 9999 CONTINUE
      END
