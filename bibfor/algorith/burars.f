        SUBROUTINE BURARS(VIN,NVI,MATERD,MATERF,NMAT,
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
C  SPHERIQUE REVERSIBLE D APRES LE MODELE BETON_BURGER_FP
C
C IN  VIN      : VARIABLES INTERNES INITIALES
C     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
C     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
C     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
C     NMAT     : DIMENSION DE CMAT
C     TIMED    : INSTANT T
C     TIMEF    : INSTANT T+DT
C OUT AN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE SPH.
C     BN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE SPH. 
C     CN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE SPH. 
C=======================================================================
      IMPLICIT NONE
      INTEGER         NVI,NMAT
      REAL*8          VIN(*)
      REAL*8          MATERD(NMAT,2),MATERF(NMAT,2)
      REAL*8          TIMED,TIMEF,TDT
      REAL*8          AN,BN,CN
      REAL*8          KRS,ETARS,HINI,HFIN
      REAL*8          ERSP
      REAL*8          TSPH,TSEXP

C === =================================================================
C --- RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
C === =================================================================
      KRS   = MATERD(1,2)
      ETARS = MATERD(2,2)

      HINI = MATERD(6,1)
      HFIN = MATERF(6,1)

C === =================================================================
C --- CALCUL VARIATION DE L'HUMIDITE ET DU TEMPS
C === =================================================================
      TDT = TIMEF - TIMED
C === =================================================================
C --- INITIALISATION VARIABLES DE SORTIE
C === =================================================================
      AN = 0.D0
      BN = 0.D0
      CN = 0.D0
C === =================================================================
C --- RECUPERATION DEFORMATION REVERSIBLE SPHERIQUE
C === =================================================================
      ERSP = VIN(1)
C === =================================================================
C --- CONSTRUCTION DE LA MATRICE SPHERIQUE REVERSIBLE
C === =================================================================
      TSPH = ETARS / KRS
      TSEXP = EXP(-TDT/TSPH)
      AN = (TSEXP - 1.D0) * ERSP
      BN = 1.D0/KRS*( TSEXP*(-HINI*(2*TSPH/TDT+1.D0)
     &      + HFIN*TSPH/TDT)
     &      + HINI*(2.D0*TSPH/TDT-1.D0) + HFIN*(1.D0-TSPH/TDT ))
      CN = HINI/(TDT*KRS)*( TSPH*TSEXP - TSPH + TDT )

      END
