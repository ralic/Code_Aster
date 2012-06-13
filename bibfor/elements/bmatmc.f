      SUBROUTINE  BMATMC ( IGAU, NBSIG, XYZ, IPOIDS, IVF, IDFDE,
     &                     NNO, NHARM, JACOB, B )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      BMATMC  -- CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS
C                 DU PREMIER ORDRE AUX DEPLACEMENTS AU POINT
C                 D'INTEGRATION D'INDICE IGAU
C
C   ARGUMENT        E/S  TYPE         ROLE
C    IGAU           IN     I        INDICE DU POINT D'INTEGRATION
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    IVF            IN     I        POINTEUR FONCTIONS DE FORME
C    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
C    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NHARM          IN     R        NUMERO D'HARMONIQUE
C    JACOB          OUT    R        PRODUIT POIDS*JACOBIEN
C    B(NBSIG,1)     OUT    R        MATRICE (B) RELIANT LES
C                                   DEFORMATIONS DU PREMIER ORDRE
C                                   AUX DEPLACEMENTS AU POINT
C                                   D'INTEGRATION IGAU.
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
           REAL*8       XYZ(1), NHARM, JACOB ,  B(NBSIG,1)
C -----  VARIABLES LOCALES
           REAL*8       DFDX(27),DFDY(27),DFDZ(27), B3J(9), NHARAY
           LOGICAL      LTEATT
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
      ZERO   = 0.0D0
      CALL MATINI(NBSIG,81,ZERO,B)
C
C       -------------
C ----  CAS MASSIF 3D
C       -------------
      IF(LTEATT(' ','DIM_TOPO_MAILLE','3')) THEN
C
          K = 3*(IGAU-1)*NNO
C
C ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
C ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
C         ----------------------------------------------
         CALL DFDM3D ( NNO, IGAU, IPOIDS, IDFDE,
     &                 XYZ, DFDX, DFDY, DFDZ, JACOB )
C
C ----    AFFECTATION DE LA MATRICE (B)
C         -----------------------------
         DO 20 I = 1, NNO
C
            J= 3*(I-1) + 1
C
            B(1,J)   = DFDX(I)
            B(2,J+1) = DFDY(I)
            B(3,J+2) = DFDZ(I)
            B(4,J)   = DFDY(I)
            B(4,J+1) = DFDX(I)
            B(5,J)   = DFDZ(I)
            B(5,J+2) = DFDX(I)
            B(6,J+1) = DFDZ(I)
            B(6,J+2) = DFDY(I)
C
 20      CONTINUE
C
C       -------------------------------------------------------
C ----  CAS MASSIF 2D CONTRAINTES PLANES ET DEFORMATIONS PLANES
C       -------------------------------------------------------
      ELSEIF (LTEATT(' ','C_PLAN','OUI').OR.
     &        LTEATT(' ','D_PLAN','OUI')) THEN
C
          K = (IGAU-1)*NNO + 1
C
C ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
C ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
C         ----------------------------------------------
         CALL DFDM2D ( NNO,IGAU, IPOIDS,IDFDE,XYZ,DFDX,DFDY,JACOB)
C
C ----    AFFECTATION DE LA MATRICE (B)
C         -----------------------------
         DO 30 I = 1, NNO
C
            J= 2*(I-1) + 1
C
            B(1,J)   = DFDX(I)
            B(2,J+1) = DFDY(I)
            B(4,J)   = DFDY(I)
            B(4,J+1) = DFDX(I)
C
 30      CONTINUE
C
C       ------------------------
C ----  CAS MASSIF AXISYMETRIQUE
C       ------------------------
      ELSEIF (LTEATT(' ','AXIS','OUI').AND.
     &        (.NOT.LTEATT(' ','FOURIER','OUI'))) THEN
C
          K     = (IGAU-1)*NNO
          RAYON = ZERO
C
          DO 40 I = 1, NNO
             IDECNO = 2*(I-1)
             RAYON = RAYON + ZR(IVF+I+K-1)*XYZ(1+IDECNO)
  40      CONTINUE
C
C ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
C ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
C         ----------------------------------------------
         CALL DFDM2D ( NNO,IGAU, IPOIDS, IDFDE, XYZ, DFDX,DFDY,JACOB)
C
         JACOB = JACOB*RAYON
C
          IF (RAYON.EQ.ZERO) THEN
              DO 50 I = 1, NNO
                 B3J(I) = DFDX(I)
 50           CONTINUE
          ELSE
              DO 60 I = 1, NNO
                 B3J(I) = ZR(IVF+I+K-1)/RAYON
 60           CONTINUE
          ENDIF
C
C ----    AFFECTATION DE LA MATRICE (B)
C         -----------------------------
         DO 70 I = 1, NNO
C
            J= 2*(I-1) + 1
C
            B(1,J)   = DFDX(I)
            B(2,J+1) = DFDY(I)
            B(3,J)   = B3J(I)
            B(4,J)   = DFDY(I)
            B(4,J+1) = DFDX(I)
C
 70      CONTINUE
C
C       ------------------
C ----  CAS MASSIF FOURIER
C       ------------------
      ELSEIF (LTEATT(' ','FOURIER','OUI')) THEN
C
          K     = (IGAU-1)*NNO
          RAYON = ZERO
C
          DO 80 I = 1, NNO
             IDECNO = 2*(I-1)
             RAYON = RAYON + ZR(IVF+I+K-1)*XYZ(1+IDECNO)
  80      CONTINUE
C
C ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
C ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB)
C         ----------------------------------------------
         CALL DFDM2D ( NNO,IGAU, IPOIDS,IDFDE,XYZ,DFDX,DFDY,JACOB)
C
         JACOB = JACOB*RAYON
         NHARAY = NHARM/RAYON
C
C ----    AFFECTATION DE LA MATRICE (B)
C         -----------------------------
         DO 90 I = 1, NNO
C
            J= 3*(I-1) + 1
C
            B(1,J)   =  DFDX(I)
            B(2,J+1) =  DFDY(I)
            B(3,J)   =  ZR(IVF+I+K-1)/RAYON
            B(3,J+2) = -ZR(IVF+I+K-1)*NHARAY
            B(4,J)   =  DFDY(I)
            B(4,J+1) =  DFDX(I)
            B(5,J)   =  ZR(IVF+I+K-1)*NHARAY
            B(5,J+2) =  DFDX(I) - ZR(IVF+I+K-1)/RAYON
            B(6,J+1) =  ZR(IVF+I+K-1)*NHARAY
            B(6,J+2) =  DFDY(I)
C
 90      CONTINUE
      ELSE
         CALL U2MESS('F','ELEMENTS_11')
      ENDIF
C.============================ FIN DE LA ROUTINE ======================
      END
