      SUBROUTINE HAYJAC (MOD,NMAT,COEFEL,COEFT,TIMED,TIMEF,
     &                   YF,DEPS,NR,NVI,VIND,VINF,YD,DY,CRIT,DRDY,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/02/2013   AUTEUR PROIX J-M.PROIX 
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
      IMPLICIT   NONE
C     --------------------------------------------------------------
C     CALCUL DU JACOBIEN DE HAYHURST = DRDY(DY)
C     IN  MOD    :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION MATER
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C         YF     :  VARIABLES A T + DT =  ( SIGF DLAMBDA XI_P XI_VP)
C         DEPS   :  INCREMENT DE DEFORMATION
C         TIMED  :  INSTANT  T
C         TIMEF  :  INSTANT  T+DT
C         NR     :  DIMENSION DECLAREE DRDY
C         NVI    :  NOMBRE DE VARIABLES INTERNES
C         VIND   :  VARIABLE INTERNES A T
C         VINF   :  VARIABLE INTERNES A T+DT
C         YD     :  VARIABLES A T  = ( SIGD  0 XI_P XI_VP) A T
C         DY     :  SOLUTION = ( DSIG  DLAMBDA  DXI_P DXI_VP )
C     OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C         IRET   :  CODE RETOUR
C     --------------------------------------------------------------
      INTEGER         NR,NMAT,IRET,NVI,I,J
      REAL*8          DEPS(6),DRDY(NR,NR),YF(NR),DY(NR),YD(NR)
      REAL*8          COEFEL(NMAT),COEFT(NMAT),CRIT(*)
      REAL*8          TIMED,TIMEF,VIND(NVI),VINF(NVI)
      CHARACTER*8     MOD
C
      INTEGER         NDT,NDI,ITENS,NDIM
      REAL*8          R8PREM,FKOOH(6,6),DEV(6),LCNRTS,S,N(6),N1(6)
      REAL*8          KRON(6),EPS0,PK,PH1,PH2,DELTA1,DELTA2,H1ST,H2ST
      REAL*8          BIGA,SIG0,PKC,ALPHAD,SEQUID,DT,THETA,EPSI,COEF
      REAL*8          H1,H2,H,D,DP,COSH1,COSH2,SINH1,SINH2,DMGMI,GRJ0
      REAL*8          SEQ,SEQ0,SEQUI,SHMAX,SINN,TERME1,TRSIG,TRSIG0
      REAL*8          SMX(6),EQUI(16),DM1
      
      REAL*8          DGDS(6,6),I6(6,6),ID(6,6),NXN(6,6),DFSDS(6,6)
      REAL*8          UN  , ZERO , D23 , D13
      PARAMETER       ( UN   =  1.D0      )
      PARAMETER       ( ZERO =  0.D0      )
      PARAMETER       ( D23  =  2.D0/3.D0 )
      PARAMETER       ( D13  = -1.D0/3.D0 )

C     --------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     --------------------------------------------------------------
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA ID         / D23   , D13   , D13   , ZERO , ZERO , ZERO ,
     &                  D13   , D23   , D13   , ZERO , ZERO , ZERO ,
     &                  D13   , D13   , D23   , ZERO , ZERO , ZERO ,
     &                  ZERO  , ZERO  , ZERO  , UN   , ZERO , ZERO ,
     &                  ZERO  , ZERO  , ZERO  , ZERO , UN   , ZERO ,
     &                  ZERO  , ZERO  , ZERO  , ZERO , ZERO , UN /
C
C --    COEFFICIENTS MATERIAU INELASTIQUES
C
      EPS0   = COEFT(1)
      PK     = COEFT(2)
      PH1    = COEFT(3)
      PH2    = COEFT(4)
      DELTA1 = COEFT(5)
      DELTA2 = COEFT(6)
      H1ST   = COEFT(7)
      H2ST   = COEFT(8)
      BIGA   = COEFT(9)
      SIG0   = COEFT(10)
C      PKC    = COEFT(11)
      ALPHAD = COEFT(12)
      SEQUID = COEFT(13)
      DT=TIMEF-TIMED
      THETA=CRIT(4)      
      EPSI=R8PREM()*PK
      
      DMGMI=0.D0
      NDIM=3

      H1=YF(8)
      H2=YF(9)
      H=H1+H2
      D=YF(10)
      DP=DY(7)
      DM1=1.D0-D
      
C  INITIALISATION DE LA MATRICE DRDY
      CALL R8INIR ( NR*NR, 0.D0 , DRDY, 1 )
C     Inverse de la matrice de Hooke
      CALL LCOPIL  ( 'ISOTROPE' , MOD , COEFEL , DGDS )
      DO 1 I=7,10
         DRDY(I,I)=1.D0
   1  CONTINUE

C
C------------CALCUL DES INVARIANTS DE CONTRAINTE  -------
C     attention FGEQUI ne prend pas en compte les SQRT(2)
      CALL DCOPY(NDT,YF,1,SMX,1)      
      CALL DSCAL(3,1.D0/SQRT(2.D0),SMX(4),1)
      CALL FGEQUI(SMX,'SIGM_DIR',NDIM,EQUI)
C     on retablit le tenseur
      CALL DSCAL(3,SQRT(2.D0),SMX(4),1)
      TRSIG=EQUI(16)
      GRJ0=MAX(EQUI(3),EQUI(4))
      GRJ0=MAX(GRJ0,EQUI(5))
      SEQ=EQUI(1)
      SEQ0=SEQ/(1.D0-D)
      TRSIG0=TRSIG/(1.D0-D)
      IF(SEQUID.EQ.0.D0) THEN
         SEQUI=GRJ0
      ELSE
         SEQUI=TRSIG
      ENDIF
C------------ CALCUL DU TENSEUR DEVIATORIQUE DES CONTRAINTES ---
      DO 10 ITENS=1,NDT
        IF (ITENS.LE.3) THEN
           DEV(ITENS)=SMX(ITENS)-TRSIG/3.D0
        ELSE
           DEV(ITENS)=SMX(ITENS)*SQRT(2.0D0)
        ENDIF
   10 CONTINUE
   
      SHMAX=50.D0
C
      TERME1=(SEQ*(1.D0-H))/(PK*(1.D0-DMGMI)*(1.D0-D))
      IF (SEQ .LE. EPSI) THEN
         SINH1=0.D0
      ELSEIF (ABS(TERME1).LT.SHMAX) THEN
         SINH1=SINH(TERME1)
      ELSE
         IRET=1
         GOTO 9999
      ENDIF
      COSH1=SQRT(1.D0+SINH1*SINH1)

C----- EQUATION DONNANT LA DERIVEE DE L ENDOMMAGEMENT
C
      IF (SEQUI .GE. 0.D0) THEN
        SINN=ALPHAD*SEQUI+((1.D0-ALPHAD)*SEQ)
      ELSE
        SINN=(1.D0-ALPHAD)*SEQ
      ENDIF
      IF ((SINN/SIG0).LT.SHMAX) THEN
         SINH2=SINH(SINN/SIG0)
      ELSE
         IRET=1
         GOTO 9999
      ENDIF
      COSH2=SQRT(1.D0+SINH2*SINH2)
      
      IF (DP.GT.0.D0) THEN
C        dFs/dp=n*(1-D)    
C        YF(1a6) CONTIENT SIG
         CALL LCPRSV ( 1.5D0 / SEQ , DEV , N )
C - DGDS(T+DT)
         CALL LCPRTE ( N  , N  , NXN )
         CALL LCPRSM ( 1.5D0   , ID    , DFSDS )
         CALL LCDIMA ( DFSDS,  NXN , DFSDS )
         CALL LCPRSM ( DP*THETA*DM1/SEQ , DFSDS, DFSDS )
         CALL LCSOMA ( DFSDS , DGDS   , DGDS  )

         DO 20 I=1,6
            DRDY(I,7)=N(I)*DM1
   20    CONTINUE
C        dFs/dD=    
C        YF(1a6) CONTIENT SIG
         DO 30 I=1,6
            DRDY(I,10)=THETA*(DEPS(I)-N(I)*DP)
   30    CONTINUE
         
C        dFp/dSig=
         COEF=-EPS0*DT*COSH1*(1.D0-H)/PK
         DO 40 I=1,6
            DRDY(7,I)=COEF*N(I)
   40    CONTINUE
C        dFp/dp=
         DRDY(7,7)=1.D0
C        dFp/dH1=
         DRDY(7,8)=EPS0*DT*COSH1*THETA*SEQ0/PK
         DRDY(7,9)=EPS0*DT*COSH1*THETA*SEQ0/PK
C        dFp/dD=
         DRDY(7,10)=EPS0*DT*COSH1*THETA*SEQ0/PK*(1.D0-H)/(1-D)**2
         
C        dFH1/ds=
         COEF=PH1*DP*(H1ST-DELTA1*H1)/SEQ/SEQ
         DO 50 I=1,6
            DRDY(8,I)=COEF*N(I)
   50    CONTINUE
C        dFH2/ds=
         COEF=PH2*DP*(H2ST-DELTA2*H2)/SEQ/SEQ
         DO 60 I=1,6
            DRDY(9,I)=COEF*N(I)
   60    CONTINUE
C        dFH1/dp=
         DRDY(8,7)=-PH1*(H1ST-DELTA1*H1)/SEQ
         DRDY(9,7)=-PH2*(H2ST-DELTA2*H2)/SEQ
C        dFH1/dH1=
         DRDY(8,8)=1.D0+PH1*DP*DELTA1*THETA/SEQ
         DRDY(9,9)=1.D0+PH2*DP*DELTA2*THETA/SEQ
C        dFH1/dD=
         DRDY(8,10)=PH1*DP*(H1ST-DELTA1*H1)/SEQ*THETA/(1-D)
         DRDY(9,10)=PH2*DP*(H2ST-DELTA2*H2)/SEQ*THETA/(1-D)

C        DFPHI/DPHI non programme
C        DFD/DS
         COEF=-BIGA*DT*COSH2/SIG0*THETA
         DO 70 I=1,6
            DRDY(10,I)=COEF*N(I)*(1.D0-ALPHAD)
   70    CONTINUE

C        DFD/DD
         DRDY(10,10)=1.D0+BIGA*DT*COSH2/SIG0*(1-ALPHAD)*THETA*SEQ0
         IF (TRSIG.GT.EPSI) THEN
            DRDY(10,10)=DRDY(10,10)+
     &      BIGA*DT*COSH2/SIG0*ALPHAD*THETA*TRSIG0
            DO 80 I=1,6
               DRDY(10,I)=DRDY(10,I)+COEF*ALPHAD*KRON(I)
   80       CONTINUE
         ENDIF

      ENDIF
      
      CALL LCICMA(DGDS, 6,6,NDT,NDT,1,1,DRDY,NR,NR,1,1)
      
9999  CONTINUE
      END
