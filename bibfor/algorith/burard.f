        SUBROUTINE BURARD(VIN,NVI,MATERD,MATERF,NMAT,
     &                    TIMED,TIMEF,AN,BN,CN) 
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
C   DEVIATOIRE REVERSIBLE D APRES LE MODELE BETON_BURGER_FP
C
C IN  VIN      : VARIABLES INTERNES INITIALES
C     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
C     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
C     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
C     NMAT     : DIMENSION DE CMAT
C     TIMED    : INSTANT T
C     TIMEF    : INSTANT T+DT
C OUT AN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE DEV.
C     BN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE DEV. 
C     CN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE DEV. 
C=======================================================================
      IMPLICIT NONE
      INTEGER         NVI,NMAT,I
      REAL*8          VIN(NVI)
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8          TIMED,TIMEF,TDT
      REAL*8          AN(6),BN,CN
      REAL*8          KRD,ETARD,HINI,HFIN
      REAL*8          ERD(6)
      REAL*8          TDEV,TDEXP

C === =================================================================
C --- RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
C === =================================================================
      KRD   = MATERD(4,2)
      ETARD = MATERD(5,2)

      HINI = MATERD(6,1)
      HFIN = MATERF(6,1)

C === =================================================================
C --- CALCUL VARIATION DU TEMPS
C === =================================================================
      TDT = TIMEF - TIMED
C === =================================================================
C --- INITIALISATION VARIABLES DE SORTIE
C === =================================================================
      DO 10 I=1,6
        AN(I) = 0.D0
 10   CONTINUE
      BN = 0.D0
      CN = 0.D0
C === =================================================================
C --- RECUPERATION DEFORMATION REVERSIBLE DEVIATOIRE
C === =================================================================
      ERD(1) = VIN(3)
      ERD(2) = VIN(5)
      ERD(3) = VIN(7)
      ERD(4) = VIN(12)
      ERD(5) = VIN(14)
      ERD(6) = VIN(16)
C === =================================================================
C --- CONSTRUCTION DE LA MATRICE DEVIATOIRE REVERSIBLE
C === =================================================================
      TDEV = ETARD / KRD
      TDEXP = EXP(-TDT/TDEV)
      DO 20 I=1,6
        AN(I) = (TDEXP - 1.D0) * ERD(I)
 20   CONTINUE 
      BN = 1.D0/KRD*( TDEXP*(-HINI*(2*TDEV/TDT+1.D0)
     &        + HFIN*TDEV/TDT)
     &        + HINI*(2.D0*TDEV/TDT-1.D0) + HFIN*(1.D0-TDEV/TDT ))
      CN = HINI/(TDT*KRD)*( TDEV*TDEXP - TDEV + TDT )

      END
