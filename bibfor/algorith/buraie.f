        SUBROUTINE BURAIE(VIND,VINF,NVI,MATERD,MATERF,NMAT,
     &                    DT,BID,CID,BIS,CIS) 
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
C=======================================================================
C
C ROUTINE QUI CALCULE LES MATRICES DE DEFORMATION DE FLUAGE PROPRE 
C  IRREVERSIBLE D APRES LE MODELE BETON_BURGER_FP
C
C IN  VIND     : VARIABLES INTERNES A T
C     VINF     : VARIABLES INTERNES A T+DT
C     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
C     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
C     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
C     NMAT     : DIMENSION DE CMAT
C     DT       : INCREMENT DE TEMPS (TIMEF-TIMED)
C OUT BID,BIS  : SCALAIRES LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE  
C     CID,CIS  : SCALAIRES LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE  
C=======================================================================
      IMPLICIT NONE
      INTEGER         NVI,NMAT,NDT,NDI,I
      REAL*8          VIND(NVI),VINF(NVI)
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8          DT
      REAL*8          BID,CID,BIS,CIS
      REAL*8          ETAS,ETAD,KAPPA,HINI,HFIN
      REAL*8          EPSIF(6),NEIF,EPSIM(6),NEIM,DEPSIR(6),NDEIR
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
C --- CONSTRUCTION TENSEURS DES DEFORMATIONS FLUAGE IRREVERSIBLES
C === =================================================================
      DO 1 I=1,NDI 
        EPSIM(I) = VIND(2)+VIND(2*I+2)
        EPSIF(I) = VINF(2)+VINF(2*I+2)
 1    CONTINUE
      EPSIM(4) = VIND(13)
      EPSIM(5) = VIND(15)
      EPSIM(6) = VIND(17)
      EPSIF(4) = VINF(13)
      EPSIF(5) = VINF(15)
      EPSIF(6) = VINF(17)
C === =================================================================
C --- CALCUL NORME TENSEURS DES DEFORMATIONS FLUAGE IRREVERSIBLES
C === =================================================================
      CALL LCPRSC(EPSIM,EPSIM,NEIM)
      NEIM = SQRT(NEIM)
      CALL LCPRSC(EPSIF,EPSIF,NEIF)
      NEIF = SQRT(NEIF)
C === =================================================================
C --- CONSTRUCTION VARIATION TENSEUR DEFORMATIONS FLUAGE IRREVERSIBLES 
C === =================================================================
      CALL LCDIVE(EPSIF,EPSIM,DEPSIR)
C === =================================================================
C --- CALCUL NORME VARIATION TENSEUR DES DEFORMATIONS FLUAGE IRR.
C === =================================================================
      CALL LCPRSC(DEPSIR,DEPSIR,NDEIR)
      NDEIR = SQRT(NDEIR)
C === =================================================================
C --- CONSTRUCTION BID,CID,BIS,CIS
C === =================================================================
      IF(ABS(NEIF-NEIM).NE.0.D0)THEN
        BID = NDEIR*DT*HFIN/(2.D0*ETAD*KAPPA*
     &      (EXP(NEIF/KAPPA)-EXP(NEIM/KAPPA)))
        BIS = NDEIR*DT*HFIN/(2.D0*ETAS*KAPPA*
     &      (EXP(NEIF/KAPPA)-EXP(NEIM/KAPPA)))
        CID = NDEIR*DT*HINI/(2.D0*ETAD*KAPPA*
     &      (EXP(NEIF/KAPPA)-EXP(NEIM/KAPPA)))
        CIS = NDEIR*DT*HINI/(2.D0*ETAS*KAPPA*
     &      (EXP(NEIF/KAPPA)-EXP(NEIM/KAPPA)))
      ELSE
        BID = 0.D0
        BIS = 0.D0
        CID = 0.D0
        CIS = 0.D0
      ENDIF
      
      END
