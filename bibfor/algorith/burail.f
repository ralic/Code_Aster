        SUBROUTINE BURAIL(VIN,NVI,MATERD,MATERF,NMAT,
     &                    TIMED,TIMEF,PART,BN,CN) 
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
C ROUTINE QUI CALCULE LES MATRICES LINEARISEES DE DEFORMATION  
C  DE FLUAGE PROPRE IRREVERSIBLE D APRES LE MODELE BETON_BURGER_FP
C
C IN  VIN      : VARIABLES INTERNES INITIALES
C     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
C     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
C     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
C     NMAT     : DIMENSION DE CMAT
C     TIMED    : INSTANT T
C     TIMEF    : INSTANT T+DT
C     PART     : PARTIE DEVIATORIQUE ('DEV') OU SPHERIQUE ('SPH')
C OUT BN       : SCALAIRE LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE SPH.
C     CN       : SCALAIRE LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE SPH.
C=======================================================================
      IMPLICIT NONE
      INTEGER         NVI,NMAT,NDT,NDI,I
      REAL*8          VIN(NVI)
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8          TIMED,TIMEF,TDT
      REAL*8          BN,CN
      REAL*8          ETA,KAPPA,HINI,HFIN
      REAL*8          EPSIM(6),NEIR
      CHARACTER*3     PART
      COMMON /TDIM/   NDT,NDI

C === =================================================================
C --- RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
C === =================================================================
      IF(PART(1:3).EQ.'SPH')THEN
        ETA = MATERD(3,2)
      ELSEIF(PART(1:3).EQ.'DEV')THEN
        ETA = MATERD(6,2)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      KAPPA = MATERD(7,2)

      HINI = MATERD(6,1)
      HFIN = MATERF(6,1)
C === =================================================================
C --- CALCUL VARIATION DU TEMPS
C === =================================================================
      TDT = TIMEF - TIMED
C === =================================================================
C --- CONSTRUCTION TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES
C === =================================================================
      DO 1 I=1,NDI 
        EPSIM(I) = VIN(2)+VIN(2*I+2)
 1    CONTINUE
      EPSIM(4) = VIN(13)
      EPSIM(5) = VIN(15)
      EPSIM(6) = VIN(17)
C === =================================================================
C --- CALCUL NORME TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES
C === =================================================================
      CALL LCPRSC(EPSIM,EPSIM,NEIR)
      NEIR = SQRT(NEIR)
C === =================================================================
C --- CONSTRUCTION DE LA MATRICE IRREVERSIBLE
C === =================================================================
      BN = 1.D0/ETA*EXP(-NEIR/KAPPA)*TDT/2.D0*HFIN
      CN = 1.D0/ETA*EXP(-NEIR/KAPPA)*TDT/2.D0*HINI

      END
