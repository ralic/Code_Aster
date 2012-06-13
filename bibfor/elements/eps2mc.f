      SUBROUTINE EPS2MC (NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     &                   XYZ,DEPL,EPS2)
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
C      EPS2MC   -- CALCUL DES  DEFORMATIONS DU SECOND ORDRE AUX
C                  POINTS D'INTEGRATION POUR LES ELEMENTS
C                  ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
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
C    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
C                                   L'ELEMENT
C    EPS2(1)        OUT    R        DEFORMATIONS DU SECOND ORDRE
C                                   AUX POINTS D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
           REAL*8       XYZ(1), DEPL(1), EPS2(1)
C -----  VARIABLES LOCALES
           REAL*8       DFDX(27),DFDY(27),DFDZ(27)
           REAL*8       JACOB
           LOGICAL      LTEATT
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
C
      DO 10 I = 1, NBSIG*NPG
         EPS2(I) = ZERO
 10   CONTINUE
C
C --- CALCUL DES DEFORMATIONS DU SECOND ORDRE AUX POINTS D'INTEGRATION
C ---  BOUCLE SUR LES POINTS D'INTEGRATION :
C      -----------------------------------
      DO 20 IGAU = 1, NPG
C
        DX     = ZERO
        RAYON  = ZERO
        DUDX   = ZERO
        DUDY   = ZERO
        DUDZ   = ZERO
        DVDX   = ZERO
        DVDY   = ZERO
        DVDZ   = ZERO
        DWDX   = ZERO
        DWDY   = ZERO
        DWDZ   = ZERO
C
C       -------------
C ----  CAS MASSIF 3D
C       -------------
      IF (LTEATT(' ','DIM_TOPO_MAILLE','3')) THEN
C
C ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
C ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB) :
C         ----------------------------------------------
         CALL DFDM3D ( NNO, IGAU, IPOIDS, IDFDE,
     &                 XYZ, DFDX, DFDY, DFDZ, JACOB )
C
C ----    CALCUL DES DERIVEES DES DEPLACEMENTS :
C         ------------------------------------
         DO 30 I = 1, NNO
C
            DUDX = DUDX + DFDX(I)*DEPL((I-1)*NDIM+1)
            DUDY = DUDY + DFDY(I)*DEPL((I-1)*NDIM+1)
            DUDZ = DUDZ + DFDZ(I)*DEPL((I-1)*NDIM+1)
C
            DVDX = DVDX + DFDX(I)*DEPL((I-1)*NDIM+2)
            DVDY = DVDY + DFDY(I)*DEPL((I-1)*NDIM+2)
            DVDZ = DVDZ + DFDZ(I)*DEPL((I-1)*NDIM+2)
C
            DWDX = DWDX + DFDX(I)*DEPL((I-1)*NDIM+3)
            DWDY = DWDY + DFDY(I)*DEPL((I-1)*NDIM+3)
            DWDZ = DWDZ + DFDZ(I)*DEPL((I-1)*NDIM+3)
C
 30      CONTINUE
C
C ----    DEFORMATIONS DU SECOND ORDRE :
C         ----------------------------
         EPS2(NBSIG*(IGAU-1)+1) = UNDEMI*(  DUDX*DUDX + DVDX*DVDX
     &                                    + DWDX*DWDX)
         EPS2(NBSIG*(IGAU-1)+2) = UNDEMI*(  DUDY*DUDY + DVDY*DVDY
     &                                    + DWDY*DWDY)
         EPS2(NBSIG*(IGAU-1)+3) = UNDEMI*(  DUDZ*DUDZ + DVDZ*DVDZ
     &                                    + DWDZ*DWDZ)
C
         EPS2(NBSIG*(IGAU-1)+4) = UNDEMI*(DUDX*DUDY + DVDX*DVDY +
     &                                    DWDX*DWDY)
         EPS2(NBSIG*(IGAU-1)+5) = UNDEMI*(DUDX*DUDZ + DVDX*DVDZ +
     &                                    DWDX*DWDZ)
         EPS2(NBSIG*(IGAU-1)+6) = UNDEMI*(DUDY*DUDZ + DVDY*DVDZ +
     &                                    DWDY*DWDZ)
C
C       ------------------------------------------------------------
C ----  CAS MASSIF 2D CONTRAINTES PLANES, DEFORMATIONS PLANES ET AXI
C       ------------------------------------------------------------
      ELSEIF (LTEATT(' ','C_PLAN','OUI').OR.
     &        LTEATT(' ','D_PLAN','OUI').OR.
     &        LTEATT(' ','AXIS','OUI')) THEN
C
          K = (IGAU-1)*NNO
C
C ----    CALCUL DES DERIVEES DES FONCTIONS DE FORME SUR L'ELEMENT
C ----    REEL ET DU PRODUIT JACOBIEN*POIDS (DANS JACOB) :
C         ----------------------------------------------
         CALL DFDM2D ( NNO,IGAU, IPOIDS,IDFDE,XYZ,DFDX,DFDY,JACOB)
C
C ----    CALCUL DES DERIVEES DES DEPLACEMENTS :
C         ------------------------------------
         DO 40 I = 1, NNO
C
            DUDX = DUDX + DFDX(I)*DEPL((I-1)*NDIM+1)
            DUDY = DUDY + DFDY(I)*DEPL((I-1)*NDIM+1)
C
            DVDX = DVDX + DFDX(I)*DEPL((I-1)*NDIM+2)
            DVDY = DVDY + DFDY(I)*DEPL((I-1)*NDIM+2)
C
            IF (LTEATT(' ','AXIS','OUI')) THEN
              IDECNO = 2*(I-1)
              RAYON = RAYON + ZR(IVF+I+K-1)*XYZ(1+IDECNO)
              DX    = DX    + ZR(IVF+I+K-1)*DEPL(1+IDECNO)
            ENDIF
 40      CONTINUE
C
C ----    DEFORMATIONS DU SECOND ORDRE :
C         ----------------------------
         EPS2(NBSIG*(IGAU-1)+1) = UNDEMI*(  DUDX*DUDX + DVDX*DVDX )
         EPS2(NBSIG*(IGAU-1)+2) = UNDEMI*(  DUDY*DUDY + DVDY*DVDY )
         EPS2(NBSIG*(IGAU-1)+3) = ZERO
C
         IF (LTEATT(' ','AXIS','OUI')) THEN
            EPS2(NBSIG*(IGAU-1)+3) = UNDEMI*DX*DX/RAYON/RAYON
         ENDIF
C
         EPS2(NBSIG*(IGAU-1)+4) = UNDEMI*(DUDX*DUDY + DVDX*DVDY)
      ELSE
         CALL U2MESS('F','ELEMENTS_11')
      ENDIF
C
  20  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
