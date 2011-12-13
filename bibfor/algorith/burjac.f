       SUBROUTINE BURJAC ( MOD, NMAT, MATERD, MATERF,NVI,VIND,
     &                     TIMED,TIMEF,YD,YF,DY,NR,DRDY )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C       ----------------------------------------------------------------
C       CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY) 
C       POUR LE MODELE BETON_BURGER_FP
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YF     :  VARIABLES A T + DT =    ( SIGF  VINF  (EPS3F)  )
C           DEPS   :  INCREMENT DE DEFORMATION
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT  T+DT
C           NR     :  DIMENSION DECLAREE DRDY
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           VIND   :  VARIABLE INTERNES A T
C           EPSD   :  DEFORMATION A T
C           YD     :  VARIABLES A T   = ( SIGD  VARD  ) A T
C           DY     :  SOLUTION           =    ( DSIG  DVIN  (DEPS3)  )
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       ----------------------------------------------------------------
      IMPLICIT NONE
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     ----------------------------------------------------------------
      INTEGER         I,NDT,NDI,NMAT,NR,NVI,IRET
      REAL*8          DRDY(NR,NR) , YF(NR), DY(NR),YD(NR)
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2),VIND(NVI) 
      REAL*8          INVELA(6,6),HOOK(6,6),TIMED,TIMEF,DAMP
      REAL*8          ETAI0(6,6),VISCO(6)
      REAL*8          AFR(6),BFR(6,6),CFR(6,6)
      REAL*8          AFD(6),BFD(6,6),CFD(6,6),CFT(6,6)
      REAL*8          TEMP(6,6),DET,DR1DY1(6,6)
      REAL*8          IDENTI(6,6),DR1DY2(6,6),DR1DY3(6)
      REAL*8          DT,HINI,SCAL,DR2DY1(6,6)
      REAL*8          KAPPA,ETAS,ETAD,EPSFIF(6),DEPSFI(6),EPSFID(6)
      REAL*8          NFIF,NFID,COEF,CEPSFI(6),NDFI,NORMAL(6)
      REAL*8          DR2DY2(6,6),DRDYT(6,6),YOUM1
      REAL*8          DR2DY3(6),DR3DY1(6),DR3DY2(6),DR3DY3
      REAL*8          D2FI1(6,6),D2FI2(6,6),MIDENT(6,6)
      CHARACTER*8     MOD

C === =================================================================
C --- INITIALISATION DES VARIABLES
C === =================================================================
      CALL LCINMA(0.D0,INVELA)
      CALL LCINMA(0.D0,IDENTI)
C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
C === =================================================================
      CALL LCOPLI ( 'ISOTROPE', '3D      ', MATERF, HOOK)
      DAMP = MATERF(6,2)
C === =================================================================
C --- INVERSION TENSEUR RIGIDITE ELASTIQUE 
C === =================================================================
      DO 1 I=1,NDT
        INVELA(I,I) = 1.D0
        IDENTI(I,I) = 1.D0
 1    CONTINUE
      CALL MGAUSS('NFVP',HOOK, INVELA, 6, NDT, NDT, DET, IRET)
C === =================================================================
C --- RECUPERATION DU TENSEUR FLUAGE PROPRE REVERSIBLE CFR (ORDRE 4) 
C === =================================================================
      CALL BURAFR(VIND,NVI,MATERD,MATERF,NMAT,TIMED,TIMEF,AFR,BFR,CFR)
C === =================================================================
C --- RECUPERATION DU TENSEUR FLUAGE DESSICATION CFD (ORDRE 4) 
C === =================================================================
      CALL BURAFD(MATERD,MATERF,NMAT,AFD,BFD,CFD)
C === =================================================================
C --- ASSEMBLAGE DES TERMES CFR+CFD (TENSEUR ORDRE 4)
C === =================================================================
      CALL LCSOMA(CFR,CFD,CFT)
C === =================================================================
C --- CALCUL DE DR(1:NDT)/DY(1:NDT)= DR1DY1
C === =================================================================
      CALL LCSOMA(INVELA,CFT,TEMP)
      CALL LCPRSM(-1.D0,TEMP,DR1DY1)

C *********************************************************************
C === =================================================================
C --- CALCUL DE DR(1:NDT)/DY(NDT+1:2*NDT)= DR1DY2
C === =================================================================
      CALL LCPRSM(-1.D0,IDENTI,DR1DY2)

C *********************************************************************
C === =================================================================
C --- CALCUL DE DR(NDT+1:NDT)/DY(1:NDT)= DR2DY1 + MISE A L'ECHELLE
C === =================================================================
      DT   = TIMEF-TIMED
      HINI = MATERD(6,1) 
      DAMP = DAMP/DT
      SCAL = -DT/2.D0*HINI/DAMP
      CALL LCPRSM(SCAL,IDENTI,DR2DY1)

C *********************************************************************
C === =================================================================
C --- CALCUL DE DR(NDT+1:NDT)/DY(NDT+1:2*NDT)= DR2DY2
C === =================================================================
      ETAS = MATERD(3,2)
      ETAD = MATERD(6,2)
      KAPPA = MATERD(7,2)
C === =================================================================
C --- RECUPERATION DES DEFORMATIONS IRREVERSIBLES A T+DT
C === =================================================================
      DO 2 I = 1,NDT
        EPSFIF(I) = YF(NDT+I)
        EPSFID(I) = YD(NDT+I)
        DEPSFI(I) = DY(NDT+I)
 2    CONTINUE
C === =================================================================
C --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T+DT -> NFIF
C === =================================================================
      CALL LCPRSC(EPSFIF,EPSFIF,NFIF)
      NFIF = SQRT(NFIF)
C === =================================================================
C --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T -> NFID
C === =================================================================
      CALL LCPRSC(EPSFID,EPSFID,NFID)
      NFID = SQRT(NFID)
C === =================================================================
C --- CALCUL DE EXP(NFIF/KAPPA)/(2*NFIF)
C === =================================================================
      IF(ABS(NFIF).NE.0.D0)THEN
        COEF = EXP(NFIF/KAPPA)/(NFIF)
      ELSE
        COEF = 0.D0
      ENDIF
C === =================================================================
C --- CALCUL DU PRODUIT SCALAIRE*VECTEUR: COEF*EPSFIF
C === =================================================================
      CALL LCPRSV(COEF,EPSFIF,CEPSFI)
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 4 DE VISCOSITE ETA_I^0
C === =================================================================
      CALL LCINMA(0.D0,ETAI0)
      DO 3 I = 1, NDI 
        ETAI0(I,I) = (ETAS+2.D0*ETAD)/3.D0
        ETAI0(I+NDI,I+NDI) = ETAD
 3    CONTINUE 
      ETAI0(1,2) = (ETAS-ETAD)/3
      ETAI0(2,1) = ETAI0(1,2)
      ETAI0(3,1) = ETAI0(1,2)
      ETAI0(1,3) = ETAI0(1,2)
      ETAI0(2,3) = ETAI0(1,2)
      ETAI0(3,2) = ETAI0(1,2)
C === =================================================================
C --- CALCUL DE LA NORME DES INCREMENTS DE DEFORMATIONS IRREVERSIBLES 
C === =================================================================
      CALL LCPRSC(DEPSFI,DEPSFI,NDFI)
      IF(ABS(NDFI).NE.0.D0)THEN
        NDFI = 1.D0/SQRT(NDFI)
      ELSE
        NDFI = 0.D0
      ENDIF
C === =================================================================
C --- CALCUL DE LA DIRECTION DES INCREMENTS DEFORMATION IRREVERSIBLE 
C === =================================================================
      CALL LCPRSV(NDFI,DEPSFI,NORMAL)
C === =================================================================
C --- CALCUL DE ETAI0(ORDRE4)*NORMAL(ORDRE2)=VISCO
C === =================================================================
      CALL LCPRMV(ETAI0,NORMAL,VISCO)
C === =================================================================
C --- PRODUIT TENSORIEL VISCO(X)CEPSFI
C === =================================================================
      CALL LCPRTE(VISCO,CEPSFI,DRDYT)
C === =================================================================
C --- SUITE - DERIVEE PAR RAPPORT A LA NORMALE D'ECOULEMENT
C === =================================================================
      CALL LCPRTE(DEPSFI,DEPSFI,D2FI1)
      SCAL = -NDFI*NDFI*NDFI
      CALL LCPRSM(SCAL,D2FI1,D2FI2)
      CALL LCPRSM(NDFI,IDENTI,MIDENT)
      CALL LCSOMA(MIDENT,D2FI2,D2FI1)
      SCAL = KAPPA*(EXP(NFIF/KAPPA)-EXP(NFID/KAPPA))
      CALL LCPRSM(SCAL,ETAI0,TEMP)
      CALL LCPRMM(TEMP,D2FI1,D2FI2)
      CALL LCSOMA(D2FI2,DRDYT,D2FI1)
C === =================================================================
C --- MISE A L'ECHELLE DE DR2DY2
C === =================================================================
      YOUM1 = 1.D0/DAMP
      CALL LCPRSM(YOUM1,D2FI1,DR2DY2)

C *********************************************************************
C === =================================================================
C --- REMPLISSAGE DU JACOBIEN
C === =================================================================
      CALL LCICMA (DR1DY1 , 6,6,NDT,NDT,1,1,DRDY,NR,NR, 1,1     )
      CALL LCICMA (DR1DY2 , 6,6,NDT,NDT,1,1,DRDY,NR,NR, 1,NDT+1 )
      CALL LCICMA (DR2DY1 , 6,6,NDT,NDT,1,1,DRDY,NR,NR, NDT+1,1 )
      CALL LCICMA (DR2DY2 , 6,6,NDT,NDT,1,1,DRDY,NR,NR, NDT+1,NDT+1 )

C *********************************************************************
C --- MODELISATION C_PLAN
C *********************************************************************
      IF(MOD(1:6).EQ.'C_PLAN')THEN
C === =================================================================
C --- CALCUL DE DR(1:NDT)/DY(NR)= DR1DY3
C === =================================================================
        CALL LCINVE(0.D0,DR1DY3)
        DR1DY3(3) = -1.D0
C === =================================================================
C --- CALCUL DE DR(NDT+1:2*NDT)/DY(NR)= DR2DY3
C === =================================================================
        CALL LCINVE(0.D0,DR2DY3)
C === =================================================================
C --- CALCUL DE DR(NR)/DY(1:NDT)= DR3DY1
C === =================================================================
        DR3DY1(1) = HOOK(3,1)*CFT(1,1)+HOOK(3,2)*CFT(2,1)+
     &              HOOK(3,3)*CFT(3,1)+HOOK(3,4)*CFT(4,1)
        DR3DY1(2) = HOOK(3,1)*CFT(1,2)+HOOK(3,2)*CFT(2,2)+
     &              HOOK(3,3)*CFT(3,2)+HOOK(3,4)*CFT(4,2)
        DR3DY1(3) = HOOK(3,1)*CFT(1,3)+HOOK(3,2)*CFT(2,3)+
     &              HOOK(3,3)*CFT(3,3)+HOOK(3,4)*CFT(4,3)
        DR3DY1(4) = HOOK(3,1)*CFT(1,4)+HOOK(3,2)*CFT(2,4)+
     &              HOOK(3,3)*CFT(3,4)+HOOK(3,4)*CFT(4,4)
C === =================================================================
C --- CALCUL DE DR(NR)/DY(NDT+1:2*NDT)= DR3DY2=-I
C === =================================================================
        DR3DY2(1) = -HOOK(3,1)
        DR3DY2(2) = -HOOK(3,2)
        DR3DY2(3) = -HOOK(3,3)
        DR3DY2(4) = -HOOK(3,4)
C === =================================================================
C --- CALCUL DE DR(NR)/DY(NR)= HOOK(3,3)
C === =================================================================
        DR3DY3 = HOOK(3,3)

C === =================================================================
C --- COMPLEMENT DU JACOBIEN
C === =================================================================
        CALL LCICMA (DR1DY3,6,1,NDT,1,1,1,DRDY,NR,NR,1,2*NDT+1 )
        CALL LCICMA (DR2DY3,6,1,NDT,1,1,1,DRDY,NR,NR,NDT+1,2*NDT+1 )
        CALL LCICMA (DR3DY1,1,6,1,NDT,1,1,DRDY,NR,NR,NR,1 )
        CALL LCICMA (DR3DY2,1,6,1,NDT,1,1,DRDY,NR,NR,NR,NDT+1 )
        CALL LCICMA (DR3DY3,1,1,1,1,1,1,DRDY,NR,NR,NR,NR )

      ENDIF

      END
