      SUBROUTINE BURLNF(NVI,VIND,NMAT,MATERD,MATERF,DT,
     &                  NR,YD,YF,VINF,SIGF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/04/2012   AUTEUR HAELEWYN J.HAELEWYN 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C ----------------------------------------------------------------
C   POST-TRAITEMENTS SPECIFIQUES AU MODELE BETON_BURGER_FP
C
C   CORRESPONDANCE ENTRE LES VARIABLES INTERNES ET LES EQUATIONS
C          DU SYSTEME DIFFERENTIEL APRES INTEGRATION
C
C ----------------------------------------------------------------
C  IN
C     NVI    :  NOMBRE DE VARIABLES INTERNES
C     VIND   :  VARIABLE INTERNES A T
C     NMAT   :  DIMENSION MATERD ET MATERF
C     MATERD :  COEF MATERIAU A T
C     MATERF :  COEF MATERIAU A T+DT
C     DT     :  INCREMENT DE TEMPS
C     NR     :  DIMENSION DU SYSTEME NL A RESOUDRE
C     YD     :  INCONNUES DES EQUATIONS NL A T
C     YF     :  INCONNUES DES EQUATIONS NL A T+DT
C  OUT
C     VINF   :  VARIABLES INTERNES A T+DT
C     SIGF   :  VECTEUR CONTRAINTES A T+DT
C ----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER         I,NDT,NDI,NMAT,NVI,NR
      REAL*8          VIND(NVI),MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8          DT,YD(NR),YF(NR),VINF(NVI),ESPH,EDEV(6)
      REAL*8          AFD(6),BFD(6,6),CFD(6,6),DEPSFD(6)
      REAL*8          BFDSID(6),CFDSIF(6)
      REAL*8          AFR(6),BFR(6,6),CFR(6,6),DEPSFR(6)
      REAL*8          BFRSID(6),CFRSIF(6),SIGF(6),EPSFIF(6),NFIF
      COMMON /TDIM/   NDT  , NDI

C *********************************************************************
C === =================================================================
C --- EXTRACTION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS IRREV.
C === =================================================================
      CALL LCINVE(0.D0,EDEV)
      ESPH = 0.D0
      DO 1 I=1,NDI
        ESPH = ESPH + YF(NDT+I)/3.D0
 1    CONTINUE
      DO 2 I=1,NDI
        EDEV(I) = YF(NDT+I)-ESPH
 2    CONTINUE
      DO 3 I=NDI+1,NDT
        EDEV(I) = YF(NDT+I)
 3    CONTINUE
C === =================================================================
C --- AFFECTATION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS IRREV.
C === =================================================================
      VINF(2) = ESPH
      DO 5 I = 1, NDI
        VINF(2+2*I) = EDEV(I)
 5    CONTINUE
      DO 6 I = NDI+1, NDT
        VINF(5+2*I) = EDEV(I)
 6    CONTINUE     

C *********************************************************************
C === =================================================================
C --- CALCUL DES DEFORMATIONS DE DESSICATION: DEPSFD=AN+BN*SIGD+CN*SIGF
C === =================================================================
      CALL BURAFD(MATERD,MATERF,NMAT,AFD,BFD,CFD)
      CALL LCPRMV(BFD,YD,BFDSID)
      CALL LCPRMV(CFD,YF,CFDSIF)
      DO 7 I=1,NDT
        DEPSFD(I) = AFD(I)+BFDSID(I)+CFDSIF(I)
 7    CONTINUE
C === =================================================================
C --- AFFECTATION DES DEFORMATIONS DESSICATION
C === =================================================================
      DO 10 I = 1, NDI
        VINF(8+I) = DEPSFD(I)+VIND(8+I)
 10   CONTINUE
      DO 11 I = NDI+1, NDT
        VINF(14+I) = DEPSFD(I)+VIND(14+I)
 11   CONTINUE

C *********************************************************************
C === =================================================================
C --- CALCUL DES DEFORMATIONS DE FLUAGE PROPRE REVERSIBLE
C === =================================================================
C === =================================================================
C     CALCUL DES DEFORMATIONS REVERSIBLES 
C          DE FLUAGE PROPRE SPHERIQUE INCREMENTALES
C === =================================================================
      CALL BURAFR(VIND,NVI,MATERD,MATERF,NMAT,0.D0,DT,AFR,BFR,CFR)
      CALL LCPRMV(BFR,YD,BFRSID)
      CALL LCPRMV(CFR,YF,CFRSIF)
      DO 12 I=1,NDT
        DEPSFR(I) = AFR(I)+BFRSID(I)+CFRSIF(I)
 12   CONTINUE
C === =================================================================
C --- EXTRACTION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS REV.
C === =================================================================
      CALL LCINVE(0.D0,EDEV)
      ESPH = 0.D0
      DO 13 I=1,NDI
        ESPH = ESPH + DEPSFR(I)/3.D0
 13    CONTINUE
      DO 14 I=1,NDI
        EDEV(I) = DEPSFR(I)-ESPH
 14   CONTINUE
      DO 15 I=NDI+1,NDT
        EDEV(I) = DEPSFR(I)
 15   CONTINUE
C === =================================================================
C --- AFFECTATION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS REV.
C === =================================================================
      VINF(1) = VIND(1)+ESPH
      DO 16 I = 1, NDI
        VINF(1+2*I) = VIND(1+2*I)+EDEV(I)
 16   CONTINUE
      DO 17 I = NDI+1, NDT
        VINF(4+2*I) = VIND(4+2*I)+EDEV(I)
 17   CONTINUE

C === =================================================================
C --- AFFECTATION CONTRAINTES REDUITES PAR LE FLUAGE
C === =================================================================
      CALL LCEQVE(YF,SIGF)

C === =================================================================
C --- CALCUL NORME DES DEFORMATIONS IRREVERSIBLES DE FLUAGE
C === =================================================================
      DO 18 I = 1, NDT
        EPSFIF(I) = YF(NDT+I)
 18   CONTINUE
      CALL LCPRSC(EPSFIF,EPSFIF,NFIF)
      NFIF = SQRT(NFIF)      

      IF(NFIF.GT.VINF(21))VINF(21) = NFIF

      END
