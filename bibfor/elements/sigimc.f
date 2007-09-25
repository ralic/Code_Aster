      SUBROUTINE SIGIMC (FAMI,NNO,NDIM,NBSIG,NPG,NI,XYZ,INSTAN,
     +                   MATER,REPERE,EPSINI,SIGMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/09/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C      SIGIMC   -- CALCUL DES  CONTRAINTES INITIALES
C                  AUX POINTS D'INTEGRATION
C                  POUR LES ELEMENTS ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    FAMI           IN     K4       FAMILLE DES POINTS DE GAUSS
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    NI(1)          IN     R        FONCTIONS DE FORME
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    MATER          IN     I        MATERIAU
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    EPSINI(1)      IN     R        VECTEUR DES DEFORMATIONS INITIALES
C    SIGMA(1)       OUT    R        CONTRAINTES INITIALES
C                                   AUX POINTS D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*4  FAMI
           REAL*8       NI(1), XYZ(1), REPERE(7), EPSINI(1)
           REAL*8       SIGMA(1), INSTAN
C -----  VARIABLES LOCALES
           REAL*8       D(36), XYZGAU(3)
           CHARACTER*2  K2BID
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      K2BID = '  '
      ZERO   = 0.0D0
C
      DO 10 I = 1, NBSIG*NPG
         SIGMA(I) = ZERO
 10   CONTINUE
C
C --- CALCUL DES CONTRAINTES INITIALES :
C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
      DO 20 IGAU = 1, NPG
C
C  --      COORDONNEES AU POINT D'INTEGRATION
C  --      COURANT
C          -------
          XYZGAU(1) = ZERO
          XYZGAU(2) = ZERO
          XYZGAU(3) = ZERO
C
          DO 30 I = 1, NNO
C
            DO 40 IDIM = 1, NDIM
               XYZGAU(IDIM) = XYZGAU(IDIM) +
     +                        NI(I+NNO*(IGAU-1))*XYZ(IDIM+NDIM*(I-1))
  40         CONTINUE
  30      CONTINUE
C
C  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
C  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C          -------------------------------------------------
          CALL DMATMC(FAMI,K2BID, MATER, INSTAN,'+',IGAU,1,
     +                 REPERE, XYZGAU, NBSIG, D, .FALSE.)
C
C  --      CONTRAINTES INITIALES AU POINT D'INTEGRATION COURANT
C          ------------------------------------------------------
          DO 50 I = 1, NBSIG
             DO 60 J = 1, NBSIG
                SIGMA(I+NBSIG*(IGAU-1)) = SIGMA(I+NBSIG*(IGAU-1)) +
     +                   D(J+(I-1)*NBSIG)*EPSINI(J+NBSIG*(IGAU-1))
  60         CONTINUE
  50      CONTINUE
C
  20  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
