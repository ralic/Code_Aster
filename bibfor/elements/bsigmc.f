      SUBROUTINE BSIGMC ( MODELI, NNO, NDIM, NBSIG, NPG, IPOIDS,
     +                    IVF, IDFDE, XYZ, NHARM, SIGMA, BSIGMA )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      BSIGMC  -- CALCUL DES FORCES INTERNES B*SIGMA AUX NOEUDS 
C                 DE L'ELEMENT DUES AU CHAMP DE CONTRAINTES SIGMA 
C                 DEFINI AUX POINTS D'INTEGRATION  
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELI         IN     K8       MODELISATION (AXI, FOURIER,...)
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    IVF            IN     I        POINTEUR FONCTIONS DE FORME
C    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
C    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    NHARM          IN     R        NUMERO D'HARMONIQUE
C    SIGMA(1)       IN     R        CONTRAINTES AUX POINTS D'INTEGRATION
C    BSIGMA(1)      OUT    R        VECTEUR DES FORCES INTERNES 
C                                   BT*SIGMA AUX NOEUDS DE L'ELEMENT
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*8  MODELI
           REAL*8       XYZ(1), SIGMA(1), BSIGMA(1), NHARM
C -----  VARIABLES LOCALES
           REAL*8       B(486), JACGAU
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      NBINCO = NDIM*NNO
C
      DO 10 I = 1, NBINCO
         BSIGMA(I) = ZERO
 10   CONTINUE
C
C --- CALCUL DE SOMME_ELEMENT(BT_SIGMA) : 
C ---  BOUCLE SUR LES POINTS D'INTEGRATION 
C      -----------------------------------
      DO 20 IGAU = 1, NPG 
C
C  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU 
C  --      PREMIER ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION 
C  --      COURANT : (EPS_1) = (B)*(UN)
C          ----------------------------
          CALL BMATMC ( IGAU, NBSIG, MODELI, XYZ, IPOIDS, IVF, IDFDE,
     +                  NNO, NHARM, JACGAU, B)
C
C  --      CALCUL DU PRODUIT (BT)*(SIGMA)*JACOBIEN*POIDS
C          ---------------------------------------------
          CALL BTSIG(NBINCO, NBSIG, JACGAU, B, SIGMA(1+NBSIG*(IGAU-1)),
     +               BSIGMA)
  20  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
