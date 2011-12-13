        SUBROUTINE BURRES(TYPMOD,NMAT,MATERD,MATERF,TIMED,TIMEF,
     &                    NVI,VIN,YD,YF,DEPS,DY,NR,R)
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
C=====================================================================
C    BETON_BURGER_FP : CALCUL RESIDUS DU SYSTEME NL A RESOUDRE = R(DY)
C                    CF. R7.01.34
C                    DY =  DSIG (DBETA PAR SYSTEME)
C                    Y  =  SIG   (BETA  PAR SYSTEME)
C                    R  = ( R1  R2  (R3) )
C                    ATTENTION IL FAUT CALCULER -R
C
C     IN  TYPMOD :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION MATER
C         MATERD :  COEFFICIENTS MATERIAU A T
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C         TIMED  :  ISTANT PRECEDENT
C         TIMEF  :  INSTANT ACTUEL
C         NVI    :  NOMBRE DE VARIABLES INTERNES
C         VIN    :  VECTEUR VARIABLES INTERNES
C         YD     :  VARIABLES A T       = ( SIGD BETAD )
C         YF     :  VARIABLES A T + DT  = ( SIGF BETAF )
C         DEPS   :  INCREMENT DE DEFORMATION
C         DY     :  SOLUTION  =  ( DSIG DBETA )
C         NR     :  DIMENSION VECTEUR INCONNUES
C     OUT R      :  RESIDU DU SYSTEME NL A T + DT
C=====================================================================
      IMPLICIT NONE
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT ,NDI
C     ----------------------------------------------------------------
      INTEGER         I,J,NDT,NDI,NMAT,NVI,NR,IRET
      REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2),TIMED,TIMEF,DT
      REAL*8          VIN(NVI),YD(NR),YF(NR),DY(NR),R(NR)
      REAL*8          DEPS(6),EPSEM(6),EPSEF(6),HOOKM(6,6)
      REAL*8          HOOK(6,6),DET,INVELA(6,6),DEPST(6),EPSEP(6)
      REAL*8          AFR(6),BFR(6,6),CFR(6,6),IDENT(6,6)
      REAL*8          AFD(6),BFD(6,6),CFD(6,6)
      REAL*8          BFRYD(6),CFRYF(6),TEMP(6),DEPSFR(6)
      REAL*8          BFDYD(6),CFDYF(6),DEPSFD(6)
      REAL*8          HINI,HFIN,KAPPA,ETAS,ETAD,ETAI0(6,6)
      REAL*8          PSIG(6),EPSFID(6),EPSFIF(6),DEPSFI(6)
      REAL*8          DIFEXP,NFID,NFIF,NDFI,KETAI0(6,6),NORMAL(6)
      REAL*8          VISCO(6),DAMP,R8PREM
      REAL*8          MAXI,MINI
      CHARACTER*8     TYPMOD

C === =================================================================
C --- INITIALISATION DES VARIABLES
C === =================================================================
      CALL LCINMA(0.D0,INVELA)
C === =================================================================
C --- EQUATIONS D'ETAT MECANIQUE : DSIG = E*DEPSE (6 EQUATIONS)
C === =================================================================
C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
C === =================================================================
      CALL LCOPLI ( 'ISOTROPE', '3D      ', MATERF, HOOK)
C === =================================================================
C --- INVERSION TENSEUR RIGIDITE ELASTIQUE A T+DT
C === =================================================================
      DO 1 I=1,NDT
        INVELA(I,I) = 1.D0
 1    CONTINUE
      CALL MGAUSS('NFVP',HOOK, INVELA, 6, NDT, 6, DET, IRET)
C === =================================================================
C --- CONSTRUCTION EPSEP = (E)^-1 * SIGF
C === =================================================================
      CALL LCPRMV(INVELA,YF,EPSEP)
C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T
C === =================================================================
      CALL LCOPLI ( 'ISOTROPE', '3D      ', MATERD, HOOKM)
C === =================================================================
C --- INVERSION TENSEUR RIGIDITE ELASTIQUE A T
C === =================================================================
      CALL LCINMA(0.D0,IDENT)
      DO 2 I=1,NDT
        IDENT(I,I) = 1.D0
 2    CONTINUE
      CALL MGAUSS('NFVP',HOOKM, IDENT, 6, NDT, 6, DET, IRET)
C === =================================================================
C --- CONSTRUCTION EPSEM = (E)^-1 * SIGD
C === =================================================================
      CALL LCPRMV(IDENT,YD,EPSEM)
C === =================================================================
C --- CONSTRUCTION DEPST = EPSEP - EPSEM
C === =================================================================
      CALL LCDIVE(EPSEP,EPSEM,DEPST)
C === =================================================================
C --- CONSTRUCTION DES DEFORMATIONS FLUAGE PROPRE REVERSIBLES
C === =================================================================
      CALL BURAFR(VIN,NVI,MATERD,MATERF,NMAT,TIMED,TIMEF,AFR,BFR,CFR)
      CALL LCPRMV(BFR,YD,BFRYD)
      CALL LCPRMV(CFR,YF,CFRYF)
      CALL LCSOVE(BFRYD,CFRYF,TEMP)
      CALL LCSOVE(TEMP,AFR,DEPSFR)
C === =================================================================
C --- CONSTRUCTION DES DEFORMATIONS FLUAGE DESSICATION
C === =================================================================
      CALL BURAFD(MATERD,MATERF,NMAT,AFD,BFD,CFD)
      CALL LCPRMV(BFD,YD,BFDYD)
      CALL LCPRMV(CFD,YF,CFDYF)
      CALL LCSOVE(BFDYD,CFDYF,DEPSFD)
C === =================================================================
C --- RECUPERATION DES DEFORMATIONS IRREVERSIBLES A T ET T+DT
C === =================================================================
      DO 4 I = 1,NDT
        EPSFID(I) = YD(NDT+I)
        EPSFIF(I) = YF(NDT+I)
        DEPSFI(I) = DY(NDT+I)
 4    CONTINUE
C === =================================================================
C --- EVALUATION DE -R(YD+DY) SUR LES (NDT) 1ERES COMPOSANTES
C === =================================================================
      DO 5 I = 1, NDT
        R(I) = -DEPS(I)+DEPST(I)+DEPSFR(I)+DEPSFD(I)+DEPSFI(I)
 5    CONTINUE

C**********************************************************************
C**********************************************************************
C === =================================================================
C --- EQUATIONS LIEES AUX DEFORMATIONS FLUAGE IRREVERSIBLE
C === =================================================================
C === =================================================================
C --- RECUPERATION TERME HYDRATATION A T ET T+DT
C === =================================================================
      HINI = MATERD(6,1)
      HFIN = MATERF(6,1)
C === =================================================================
C --- RECUPERATION PROPRIETE VISQUEUSE IRREVERSIBLE
C === =================================================================
      ETAS = MATERD(3,2)
      ETAD = MATERD(6,2)
      KAPPA = MATERD(7,2)
C === =================================================================
C --- CALCUL DE (SIGF*HINI + SIGD*HFIN)*(DT/2)
C === =================================================================
      DT = TIMEF - TIMED
      DO 6 I = 1, NDT
        PSIG(I) = DT/2.D0*(HINI*YF(I)+HFIN*YD(I))
 6    CONTINUE
C === =================================================================
C --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T -> NFID
C === =================================================================
      CALL LCPRSC(EPSFID,EPSFID,NFID)
      NFID = SQRT(NFID)
C === =================================================================
C --- CALCUL DE LA NORME DES DEFORMATIONS IRREVERSIBLES A T+DT -> NFIF
C === =================================================================
      CALL LCPRSC(EPSFIF,EPSFIF,NFIF)
      NFIF = SQRT(NFIF)
C === =================================================================
C --- CALCUL DE (EXP(NFIF/KAPPA)-EXP(NFID/KAPPA))
C === =================================================================
      IF(((NFIF/KAPPA).LT.1.D2).AND.((NFID/KAPPA).LT.1.D2))THEN
        DIFEXP = EXP(NFIF/KAPPA)-EXP(NFID/KAPPA)
      ELSE
        DIFEXP = 0.D0
      ENDIF
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 4 DE VISCOSITE KAPPA*(ETA_I^0)*DIFEXP
C === =================================================================
      CALL LCINMA(0.D0,ETAI0)
      DO 7 I = 1, NDI 
        ETAI0(I,I) = (ETAS+2.D0*ETAD)/3.D0
        ETAI0(I+NDI,I+NDI) = ETAD
 7    CONTINUE 
      ETAI0(1,2) = (ETAS-ETAD)/3.D0
      ETAI0(2,1) = ETAI0(1,2)
      ETAI0(3,1) = ETAI0(1,2)
      ETAI0(1,3) = ETAI0(1,2)
      ETAI0(2,3) = ETAI0(1,2)
      ETAI0(3,2) = ETAI0(1,2)
      CALL LCPRSM(KAPPA*DIFEXP,ETAI0,KETAI0)
C === =================================================================
C --- CALCUL DE LA NORME DES INCREMENTS DE DEFORMATIONS IRREVERSIBLES 
C === =================================================================
      CALL LCPRSC(DEPSFI,DEPSFI,NDFI)
C --- ON S'ASSURE QUE TOUTE DIVISION PAR ZERO EST IMPOSSIBLE
      IF(NDFI.NE.0.D0)THEN
        NDFI = 1.D0/SQRT(NDFI)
      ELSE
        NDFI = 0.D0
      ENDIF
C === =================================================================
C --- CALCUL DE LA DIRECTION DES INCREMENTS DEFORMATION IRREVERSIBLE 
C === =================================================================
      CALL LCPRSV(NDFI,DEPSFI,NORMAL)
C === =================================================================
C --- CALCUL DE KETAI0(ORDRE4)*NORMAL(ORDRE2)=VISCO
C === =================================================================
      CALL LCPRMV(KETAI0,NORMAL,VISCO)
C === =================================================================
C --- EVALUATION DE -R(YD+DY) SUR LES (NDT) DERNIERES COMPOSANTES
C === =================================================================
      DO 8 I = NDT+1, 2*NDT
        R(I) = PSIG(I-NDT)-VISCO(I-NDT)
 8    CONTINUE
C === =================================================================
C --- MISE A L'ECHELLE DU SYSTEME D'EQUATIONS NL
C === =================================================================
      DAMP = MATERF(6,2)/DT
      DO 9 I = NDT+1, 2*NDT
        R(I) = R(I)/DAMP
 9    CONTINUE
C**********************************************************************
C**********************************************************************
C === =================================================================
C --- MODELISATION C_PLAN: EQ(NR) SIG3 = 0
C === =================================================================
        IF ( TYPMOD(1:6) .EQ. 'C_PLAN' ) THEN
          CALL LCPRMV(INVELA,YD,EPSEM)
          DO 10 I = 1, NDT
            EPSEF(I) = EPSEM(I)+DEPS(I)-DEPSFD(I)-DEPSFR(I)-DY(NDT+I)
 10       CONTINUE
          R(NR) = -HOOK(3,3)*EPSEF(3)-HOOK(3,1)*EPSEF(1)
     &            -HOOK(3,2)*EPSEF(2)-HOOK(3,4)*EPSEF(4)
        ENDIF
C === =================================================================
C --- TRAITEMENT DU BRUIT NUMERIQUE PAR R8PREM
C === =================================================================
      MAXI = 0.D0
      DO 11 I = 1, NR
        IF(ABS(R(I)).GT.MAXI)MAXI = ABS(R(I))
 11   CONTINUE
      MINI = R8PREM() * MAXI
      DO 12 I = 1, NR
        IF(ABS(R(I)).LT.MINI)R(I) = 0.D0
 12   CONTINUE

      END
