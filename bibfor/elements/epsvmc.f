      SUBROUTINE EPSVMC (FAMI,NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,
     &                   IDFDE, XYZ,DEPL,INSTAN,MATER,
     &                   REPERE,NHARM,OPTION,EPSM)
      IMPLICIT NONE
C ----------------------------------------------------------------------
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
C
C      EPSVMC   -- CALCUL DES  DEFORMATIONS MECANIQUES
C                  (I.E. EPS_TOTALES - EPS_THERMIQUES )
C                  AUX POINTS D'INTEGRATION POUR LES ELEMENTS
C                  ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    FAMI           IN     K4       TYPE DE FAMILLE DE POINT DE GAUSS
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
C    IVF            IN     I        POINTEUR FONCTIONS DE FORME
C    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
C                                   L'ELEMENT
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    MATER          IN     I        MATERIAU
C    NHARM          IN     R        NUMERO D'HARMONIQUE
C    OPTION         IN     K16      OPTION DE CALCUL
C    EPSM(1)        OUT    R        DEFORMATIONS MECANIQUES AUX
C                                   POINTS D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION
      CHARACTER*4  FAMI
      REAL*8       XYZ(1),  DEPL(1),  EPSM(1), REPERE(7)
      REAL*8       INSTAN,   NHARM
      INTEGER      IDFDE,IPOIDS,IVF,MATER,NBSIG,NDIM,NNO,NPG
C -----  VARIABLES LOCALES
      CHARACTER*8  PHENOM
      REAL*8       EPSTH(162), EPS2(162), XYZGAU(3), D(4,4)
      REAL*8       ZERO,UN,DEUX
      INTEGER      I,IGAU,ICODRE
      LOGICAL      LTEATT
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      UN     = 1.0D0
      DEUX   = 2.0D0
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
      CALL EPS1MC(NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     &            XYZ,DEPL,NHARM,EPSM)
C
C ---   CALCUL DES DEFORMATIONS DU SECOND ORDRE AUX POINTS
C ---   D'INTEGRATION POUR LES GRANDES TRANSFORMATIONS :
C       ----------------------------------------------
      IF (OPTION(4:4).EQ.'G') THEN
         CALL EPS2MC(NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     &               XYZ,DEPL,EPS2)
      ENDIF
C
C --- CALCUL DES DEFORMATIONS THERMIQUES AUX POINTS D'INTEGRATION
C --- (AJOUTEES AUX DEFORMATIONS DE RETRAIT ENDOGENE/DESSICCATION)
C      ----------------------------------------------------------
      CALL RCCOMA(MATER,'ELAS',PHENOM,ICODRE)
      IF (PHENOM(1:8).NE.'ELAS_MET') THEN
        CALL EPTHMC(FAMI,NNO,NDIM,NBSIG,NPG,ZR(IVF),XYZ,REPERE,
     +         INSTAN,MATER,OPTION,EPSTH)
      ELSE IF (OPTION(1:4).EQ.'EPME'.OR.OPTION(1:4).EQ.'EPMG') THEN
        CALL U2MESK('F','ELEMENTS_15',1,PHENOM)
      ENDIF
C
C --- CALCUL DES DEFORMATIONS MECANIQUES AUX POINTS D'INTEGRATION :
C      ----------------------------------------------------------
      DO 30 I = 1, NBSIG*NPG
         EPSM(I) = EPSM(I) + EPS2(I)
 30   CONTINUE

      IF (OPTION(1:4).EQ.'EPME'.OR.OPTION(1:4).EQ.'EPMG') THEN
        DO 40 I = 1, NBSIG*NPG
           EPSM(I) = EPSM(I) - EPSTH(I)
 40     CONTINUE
      ENDIF
C
C --- CAS DES CONTRAINTES PLANES, ON CALCULE EPSZZ A PARTIR
C --- DE SIGZZ = 0 :
C     ------------
      IF (LTEATT(' ','C_PLAN','OUI')) THEN
C
C ---   BOUCLE SUR LES POINTS D'INTEGRATION :
C       -----------------------------------
        DO 50 IGAU = 1, NPG
C
C  --      COORDONNEES AU POINT D'INTEGRATION
C  --      COURANT
C          -------
          XYZGAU(1) = ZERO
          XYZGAU(2) = ZERO
          XYZGAU(3) = ZERO
C
C  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
C  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C          -------------------------------------------------
          CALL DMATMC(FAMI,'DP',MATER,
     &                INSTAN,'+',IGAU,1,REPERE,XYZGAU,NBSIG,D,.FALSE.)
C
          IF (OPTION(1:4).EQ.'EPME'.OR.OPTION(1:4).EQ.'EPMG') THEN
            EPSM(NBSIG*(IGAU-1)+3) = -UN/D(3,3)*
     &                          (  D(3,1)*EPSM(NBSIG*(IGAU-1)+1)
     &                           + D(3,2)*EPSM(NBSIG*(IGAU-1)+2)
     &                           + D(3,4)*EPSM(NBSIG*(IGAU-1)+4)*DEUX)
          ELSE
            EPSM(NBSIG*(IGAU-1)+3) = -UN/D(3,3)*
     &       (D(3,1)*(EPSM(NBSIG*(IGAU-1)+1)-EPSTH(NBSIG*(IGAU-1)+1))
     &       +D(3,2)*(EPSM(NBSIG*(IGAU-1)+2)-EPSTH(NBSIG*(IGAU-1)+2))
     &       +D(3,4)*(EPSM(NBSIG*(IGAU-1)+4)
     &       -EPSTH(NBSIG*(IGAU-1)+4))*DEUX)+EPSTH(NBSIG*(IGAU-1)+3)
         ENDIF
 50     CONTINUE
C
C --- CAS DES DEFORMATIONS PLANES,  EPSZZ = 0 :
C     ---------------------------------------
      ELSEIF (LTEATT(' ','D_PLAN','OUI')) THEN
C
C ---   BOUCLE SUR LES POINTS D'INTEGRATION :
C       -----------------------------------
        DO 70 IGAU = 1, NPG
          EPSM(NBSIG*(IGAU-1)+3) = ZERO
 70     CONTINUE
C
      ENDIF
C
C.============================ FIN DE LA ROUTINE ======================
      END
