      SUBROUTINE EPSVMC (MODELI,NNO,NDIM,NBSIG,NPG,NI,DNIDX,DNIDY,DNIDZ,
     +                   POIDS,XYZ,DEPL,TEMPE,TREF,HYDR,SECH,INSTAN,
     +                   MATER,REPERE,NHARM,OPTION,EPSM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/02/2002   AUTEUR PABHHHH N.TARDIEU 
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
C TOLE CRP_21
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      EPSVMC   -- CALCUL DES  DEFORMATIONS MECANIQUES
C                  (I.E. EPS_TOTALES - EPS_THERMIQUES - EPS_RETRAIT)
C                  AUX POINTS D'INTEGRATION POUR LES ELEMENTS
C                  ISOPARAMETRIQUES
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
C    DNIDX(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
C                                   / X SUR L'ELEMENT DE REFERENCE
C    DNIDY(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
C                                   / Y SUR L'ELEMENT DE REFERENCE
C    DNIDZ(1)       IN     R        DERIVEES DES FONCTIONS DE FORME
C                                   / Z SUR L'ELEMENT DE REFERENCE
C    POIDS(1)       IN     R        POIDS D'INTEGRATION
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
C                                   L'ELEMENT
C    TEMPE(1)       IN     R        TEMPERATURES AUX NOEUDS DE
C                                   L'ELEMENT
C    TREF           IN     R        TEMPERATURE DE REFERENCE
C    HYDR(1)        IN     R        HYDRATATION AUX POINTS DE GAUSS
C    SECH(1)        IN     R        SECHAGE AUX NOEUDS DE L'ELEMENT
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    MATER          IN     I        MATERIAU
C    NHARM          IN     R        NUMERO D'HARMONIQUE
C    OPTION         IN     K16      OPTION DE CALCUL
C    EPSM(1)        OUT    R        DEFORMATIONS MECANIQUES AUX
C                                   POINTS D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*8  MODELI
           CHARACTER*16 OPTION
           REAL*8       NI(1),   DNIDX(1), DNIDY(1), DNIDZ(1), POIDS(1)
           REAL*8       XYZ(1),  DEPL(1),  TEMPE(1), EPSM(1), REPERE(7)
           REAL*8       HYDR(1), SECH(1),  INSTAN,   NHARM
C -----  VARIABLES LOCALES
           CHARACTER*8  MODEDP
           REAL*8       EPSTH(162), EPS2(162), XYZGAU(3), D(4,4)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      MODEDP = 'DP'
C
      DO 10 I = 1, NBSIG*NPG
         EPSM(I) = ZERO
         EPS2(I) = ZERO
         EPSTH(I)= ZERO
 10   CONTINUE
C
C --- CALCUL DES DEFORMATIONS DU PREMIER ORDRE
C --- AUX POINTS D'INTEGRATION :
C      -----------------------
      CALL EPS1MC(MODELI,NNO,NDIM,NBSIG,NPG,NI,DNIDX,DNIDY,DNIDZ,
     +            POIDS,XYZ,DEPL,NHARM,EPSM)
C
C ---   CALCUL DES DEFORMATIONS DU SECOND ORDRE AUX POINTS
C ---   D'INTEGRATION POUR LES GRANDES TRANSFORMATIONS :
C       ----------------------------------------------
      IF (OPTION(4:4).EQ.'G') THEN
         CALL EPS2MC(MODELI,NNO,NDIM,NBSIG,NPG,NI,DNIDX,DNIDY,DNIDZ,
     +               POIDS,XYZ,DEPL,EPS2)
      ENDIF
C
C --- CALCUL DES DEFORMATIONS THERMIQUES AUX POINTS D'INTEGRATION
C --- AJOUTEES AUX DEFORMATIONS DE RETRAIT ENDOGENE/DESSICCATION:
C      ----------------------------------------------------------
      IF (OPTION(1:4).EQ.'EPME'.OR.OPTION(1:4).EQ.'EPMG'.OR.
     +    OPTION(1:4).EQ.'EPMH') THEN
        CALL EPTHMC(MODELI,NNO,NDIM,NBSIG,NPG,NI,TEMPE,TREF,HYDR,
     +              SECH,INSTAN,MATER,OPTION,EPSTH)
      ENDIF
C
C --- CALCUL DES DEFORMATIONS MECANIQUES AUX POINTS D'INTEGRATION :
C      ----------------------------------------------------------
      DO 20 I = 1, NBSIG*NPG
         EPSM(I) = EPSM(I) + EPS2(I) - EPSTH(I)
 20   CONTINUE
C
C --- CAS DES CONTRAINTES PLANES, ON CALCULE EPSZZ A PARTIR
C --- DE SIGZZ = 0 :
C     ------------
      IF (MODELI(1:2).EQ.'CP') THEN
C
C ---   BOUCLE SUR LES POINTS D'INTEGRATION :
C       -----------------------------------
        DO 30 IGAU = 1, NPG
C
C  --      COORDONNEES ET TEMPERATURE AU POINT D'INTEGRATION
C  --      COURANT
C          -------
          XYZGAU(1) = ZERO
          XYZGAU(2) = ZERO
          XYZGAU(3) = ZERO
          TEMPG     = ZERO
C
          DO 40 I = 1, NNO
             TEMPG     = TEMPG     + NI(I+NNO*(IGAU-1))*TEMPE(I)
  40      CONTINUE
C
C  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
C  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C          -------------------------------------------------
          HYDRG = 0.D0
          SECHG = 0.D0
          CALL DMATMC(MODEDP, MATER, TEMPG, HYDRG, SECHG, INSTAN,
     +                REPERE, XYZGAU, NBSIG, D, .FALSE.)
C
          EPSM(NBSIG*(IGAU-1)+3) = -UN/D(3,3)*
     +                          (  D(3,1)*EPSM(NBSIG*(IGAU-1)+1)
     +                           + D(3,2)*EPSM(NBSIG*(IGAU-1)+2)
     +                           + D(3,4)*EPSM(NBSIG*(IGAU-1)+4)*DEUX)
 30     CONTINUE
C
C --- CAS DES DEFORMATIONS PLANES,  EPSZZ = 0 :
C     ---------------------------------------
      ELSEIF (MODELI(1:2).EQ.'DP') THEN
C
C ---   BOUCLE SUR LES POINTS D'INTEGRATION :
C       -----------------------------------
        DO 50 IGAU = 1, NPG
          EPSM(NBSIG*(IGAU-1)+3) = ZERO
 50     CONTINUE
C
      ENDIF
C
C.============================ FIN DE LA ROUTINE ======================
      END
