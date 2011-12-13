       SUBROUTINE BURJBC (NMAT,MATERD,MATERF,DT,SIGF,NVI,VIND,VINF,
     &                    DBDED,DBDES,DCDED,DCDES)
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
C       CALCUL DES TERMES INTERVENANT DANS LA DEFINITION DE L'OPERATEUR
C       TANGENT DE BETON_BURGER_FP EN VITESSE A T+DT
C
C       IN   OPTION : 'DEV' OU 'SPH' -> TERMES SPHERIQUES OU DEVIATOIRES
C            NMAT   : DIMENSION MATER
C            MATERD : COEFFICIENTS MATERIAU A T
C            MATERF : COEFFICIENTS MATERIAU A T+DT
C            DT     : INTERVALLE DE TEMPS DT = (T+DT)-T
C            SIGF   : CONTRAINTES A L'INSTANT T+
C            NVI    : NOMBRE DE VARIABLES INTERNES           
C            VIN    : VARIABLES INTERNES A L'INSTANT T+DT            
C
C       OUT  DBDED  : DERIVEE DE B^I_N / EPSD (EPSD = EPS - EPSV*I)
C            DBDES  : DERIVEE DE B^I_N / EPSV (EPSV = TR(EPS))
C            DCDED  : DERIVEE DE C^I_N / EPSD (EPSD = EPS - EPSV*I)
C            DCDES  : DERIVEE DE C^I_N / EPSV (EPSV = TR(EPS))
C       ----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER         NVI,NMAT,NDT,NDI,I
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2),DT,HINI,HFIN
      REAL*8          SIGF(6),VIND(NVI),VINF(NVI)
      REAL*8          DBDED(6),DBDES,DCDED(6),DCDES
      REAL*8          ETAS,ETAD,KAPPA
      REAL*8          EPSIF(6),EPSIM(6),NEIF,NEIM,DEPSIR(6),NDEIR
      REAL*8          FBD,FCD,GBD,GCD
      REAL*8          FPBD(6),FPCD(6),GPBD(6),GPCD(6)
      REAL*8          FBS,FCS,GBS,GCS,FPBS,FPCS,GPBS,GPCS
      COMMON /TDIM/   NDT,NDI

C === =================================================================
C --- RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
C === =================================================================
      ETAS  = MATERD(3,2)
      ETAD  = MATERD(6,2)
      KAPPA = MATERD(7,2)
      HINI  = MATERD(6,1)
      HFIN  = MATERF(6,1)
C === =================================================================
C --- CONSTRUCTION TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES T+DT
C === =================================================================
      DO 1 I=1,NDI 
        EPSIF(I) = VINF(2)+VINF(2*I+2)
 1    CONTINUE
      EPSIF(4) = VINF(13)
      EPSIF(5) = VINF(15)
      EPSIF(6) = VINF(17)
C === =================================================================
C --- CALCUL NORME TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES T+DT
C === =================================================================
      CALL LCPRSC(EPSIF,EPSIF,NEIF)
      NEIF = SQRT(NEIF)
C === =================================================================
C --- CONSTRUCTION TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES T
C === =================================================================
      DO 2 I=1,NDI 
        EPSIM(I) = VIND(2)+VIND(2*I+2)
 2    CONTINUE
      EPSIM(4) = VIND(13)
      EPSIM(5) = VIND(15)
      EPSIM(6) = VIND(17)
C === =================================================================
C --- CALCUL NORME TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES T
C === =================================================================
      CALL LCPRSC(EPSIM,EPSIM,NEIM)
      NEIM = SQRT(NEIM)
C === =================================================================
C --- CONSTRUCTION VARIATION TENSEUR DEFORMATIONS FLUAGE IRREVERSIBLES 
C === =================================================================
      CALL LCDIVE(EPSIF,EPSIM,DEPSIR)
C === =================================================================
C --- CALCUL NORME VARIATION TENSEUR DES DEFORMATIONS FLUAGE IRR.
C === =================================================================
      CALL LCPRSC(DEPSIR,DEPSIR,NDEIR)
      NDEIR = SQRT(NDEIR)
C *********************************************************************
C === =================================================================
C --- CALCUL FBD, FCD, GBD, GCD
C === =================================================================
      FBD = NDEIR*DT*HFIN
      FCD = NDEIR*DT*HINI
      GBD = 2.D0*ETAD*KAPPA*(EXP(NEIF/KAPPA)-EXP(NEIM/KAPPA))
      GCD = GBD
C === =================================================================
C --- CALCUL FPBD, FPCD, GPBD, GPCD
C === =================================================================
      DO 3 I = 1, NDT
        IF(ABS(NEIF).NE.0.D0)THEN
          FPBD(I) = DT*HFIN*3.D0/4.D0*EPSIF(I)/NEIF
          FPCD(I) = DT*HINI*3.D0/4.D0*EPSIF(I)/NEIF
          GPBD(I) = 3.D0/2.D0*ETAD*EXP(NEIF/KAPPA)*EPSIF(I)/NEIF
          GPCD(I) = GPBD(I)
        ELSE
          FPBD(I) = 0.D0
          FPCD(I) = 0.D0
          GPBD(I) = 0.D0
          GPCD(I) = 0.D0
        ENDIF
 3    CONTINUE       
C === =================================================================
C --- CALCUL DE DBDED, DCDED
C === =================================================================
      DO 4 I = 1, NDT
        IF(ABS(GBD).NE.0.D0)THEN
          DBDED(I) = (FPBD(I)*GBD - FBD*GPBD(I))/(GBD**2)
        ELSE
          DBDED(I) = 0.D0
        ENDIF
        IF(ABS(GCD).NE.0.D0)THEN
          DCDED(I) = (FPCD(I)*GCD - FCD*GPCD(I))/(GCD**2)
        ELSE
          DCDED(I) = 0.D0
        ENDIF
 4    CONTINUE
C *********************************************************************
C === =================================================================
C --- CALCUL FBS, FCS, GBS, GCS
C === =================================================================
      FBS = FBD
      FCS = FCD
      GBS = 2.D0*ETAS*KAPPA*(EXP(NEIF/KAPPA)-EXP(NEIM/KAPPA))
      GCS = GBD
C === =================================================================
C --- CALCUL FPBS, FPCS, GPBS, GPCS
C === =================================================================
      IF(ABS(NEIF).NE.0.D0)THEN
        FPBS = DT*HFIN*(EPSIF(1)+EPSIF(2)+EPSIF(3))/(2.D0*NEIF)
        FPCS = DT*HINI*(EPSIF(1)+EPSIF(2)+EPSIF(3))/(2.D0*NEIF)
        GPBS = ETAS*EXP(NEIF/KAPPA)*(EPSIF(1)+EPSIF(2)+EPSIF(3))/NEIF
        GPCS = GPBS
      ELSE
        FPBS = 0.D0
        FPCS = 0.D0
        GPBS = 0.D0
        GPCS = 0.D0
      ENDIF
C === =================================================================
C --- CALCUL DE DBDES, DCDES
C === =================================================================
      IF(ABS(GBS).NE.0.D0)THEN
        DBDES = (FPBS*GBS - FBS*GPBS)/(GBS**2)
      ELSE
        DBDES = 0.D0
      ENDIF
      IF(ABS(GCS).NE.0.D0)THEN
        DCDES = (FPCS*GCS - FCS*GPCS)/(GCS**2)
      ELSE
        DCDES = 0.D0
      ENDIF

      END
