      SUBROUTINE SIGTMC (MODELI,NNO,NDIM,NBSIG,NPG,NI,XYZ,TEMPE,TREF,
     +                   HYDR,SECH,INSTAN,MATER,REPERE,OPTION,SIGMA)
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
C      SIGTMC   -- CALCUL DES  CONTRAINTES THERMIQUES/HYDRIQUE OU DE
C                  SECHAGE AUX POINTS D'INTEGRATION
C                  POUR LES ELEMENTS ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELI         IN     K8       MODELISATION (AXI, FOURIER,...)
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    NI(1)          IN     R        FONCTIONS DE FORME
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    TEMPE(1)       IN     R        TEMPERATURES AUX NOEUDS DE
C                                   L'ELEMENT
C    TREF           IN     R        TEMPERATURE DE REFERENCE
C    HYDR(1)        IN     R        HYDRATATION AUX POINT DE GAUSS DE
C                                   L'ELEMENT
C    SECH(1)        IN     R        SECHAGE AUX NOEUDS DE
C                                   L'ELEMENT
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    MATER          IN     I        MATERIAU
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    OPTION         IN     K16      OPTION DE CALCUL
C    SIGMA(1)       OUT    R        CONTRAINTES THERMIQUES
C                                   AUX POINTS D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*8  MODELI
           CHARACTER*16 OPTION
           REAL*8       NI(1), XYZ(1), TEMPE(1), REPERE(7), SIGMA(1)
           REAL*8       HYDR(1),SECH(1),INSTAN
C -----  VARIABLES LOCALES
           REAL*8       D(36), XYZGAU(3), EPSTH(6)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      NDIM2  = NDIM
      IF (MODELI(1:2).EQ.'FO') THEN
        NDIM2 = 2
      ENDIF
C
      DO 10 I = 1, NBSIG*NPG
         SIGMA(I) = ZERO
 10   CONTINUE
C
C --- CALCUL DES CONTRAINTES D'ORIGINE THERMIQUE/HYDRIQUE/SECHAGE
C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
      DO 20 IGAU = 1, NPG
C
C  --      COORDONNEES ET TEMPERATURE AU POINT D'INTEGRATION
C  --      COURANT
C          -------
          XYZGAU(1) = ZERO
          XYZGAU(2) = ZERO
          XYZGAU(3) = ZERO
          TEMPG     = ZERO
          IF (OPTION(1:16).EQ.'CHAR_MECA_TEMP_R'.OR.
     &        OPTION(1:16).EQ.'CHAR_MECA_HYDR_R'.OR.
     &        OPTION(1:16).EQ.'CHAR_MECA_SECH_R') THEN
              HYDRG     = HYDR(IGAU)
          ELSE
              HYDRG     = ZERO
          ENDIF
          SECHG     = ZERO
C
          DO 30 I = 1, NNO
C
            DO 40 IDIM = 1, NDIM2
               XYZGAU(IDIM) = XYZGAU(IDIM) +
     +                  NI(I+NNO*(IGAU-1))*XYZ(IDIM+NDIM2*(I-1))
  40         CONTINUE
C
             TEMPG     = TEMPG     + NI(I+NNO*(IGAU-1))*TEMPE(I)
          IF (OPTION(1:16).EQ.'CHAR_MECA_TEMP_R'.OR.
     &        OPTION(1:16).EQ.'CHAR_MECA_HYDR_R'.OR.
     &        OPTION(1:16).EQ.'CHAR_MECA_SECH_R') THEN
             SECHG     = SECHG     + NI(I+NNO*(IGAU-1))*SECH(I)
          ELSE
             SECHG     = ZERO
          ENDIF
  30      CONTINUE
C
C  --      CALCUL DES DEFORMATIONS THERMIQUES/HYDRIQUE/DE SECHAGE
C  --      AU POINT D'INTEGRATION COURANT
C          ------------------------------
          CALL EPSTMC(MODELI, TEMPG, TREF, HYDRG, SECHG, INSTAN,
     &                MATER, OPTION, EPSTH)
C
C  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
C  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C          -------------------------------------------------
          CALL DMATMC(MODELI, MATER, TEMPG, HYDRG, SECHG, INSTAN,
     +                REPERE, XYZGAU, NBSIG, D, .FALSE.)
C
C  --      CONTRAINTES THERMIQUES/HYDRIQUE/DE SECHAGE AU POINT
C  --      D'INTEGRATION COURANT
C          ------------------------------------------------------
          DO 50 I = 1, NBSIG
             DO 60 J = 1, NBSIG
                SIGMA(I+NBSIG*(IGAU-1)) = SIGMA(I+NBSIG*(IGAU-1)) +
     +                                    D(J+(I-1)*NBSIG)*EPSTH(J)
  60         CONTINUE
  50      CONTINUE
C
  20  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
