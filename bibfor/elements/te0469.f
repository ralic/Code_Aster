      SUBROUTINE TE0469(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)

C     BUT: CONSTRUCTION DU VECTEUR DES FORCES CORRESPONDANT A UN
C          CHARGEMENT FORCE_ARETE POUR LES ELEMENTS ISOPARAMETRIQUES 3D.
C          (I.E. CALCUL DU VECTEUR DES FORCES NODALES EQUIVALENTES
C                A UNE FORCE LINEIQUE LE LONG D'UNE ARETE POUR
C                LES ELEMENTS 3D).

C          OPTIONS : 'CHAR_MECA_FR1D3D'
C                    'CHAR_MECA_FF1D3D'

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      PARAMETER (NBNOMX=27)
      CHARACTER*8 NOMPAR(4)
      CHARACTER*16 NOMTE,OPTION
      REAL*8 FX(NBNOMX),FY(NBNOMX),FZ(NBNOMX),VALPAR(4)
      REAL*8 XYZGAU(3),FORLIN(3),JACOB
      REAL*8 FXLIN(5),FYLIN(5),FZLIN(5)

C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------


C --- CARACTERISTIQUES DU TYPE D'ELEMENT :
C --- INTEGRATION ET INTERPOLATION
C      ----------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDK,JGANO)

C --- INITIALISATIONS :
C     -----------------
      ZERO = 0.0D0

      DO 10 I = 1,NBNOMX
        FX(I) = ZERO
        FY(I) = ZERO
        FZ(I) = ZERO
   10 CONTINUE

      DO 20 I = 1,NPG
        FXLIN(I) = ZERO
        FYLIN(I) = ZERO
        FZLIN(I) = ZERO
   20 CONTINUE

C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C --- OPTION 'CHAR_MECA_FR1D3D'
C --- CAS OU LES DONNEES DES FORCES LINEIQUES SONT DES REELS :
C     ------------------------------------------------------
      IF (OPTION.EQ.'CHAR_MECA_FR1D3D') THEN

C ---  RECUPERATION DES VALEURS DE LA FORCE LINEIQUE A APPLIQUER
C ---  SUR L'ELEMENT D'ARETE :
C      ---------------------
        CALL JEVECH('PFR1D3D','L',IDFLIN)

C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
        DO 40 IGAU = 1,NPG

          IDECPG = NNO* (IGAU-1)


C ---    CALCUL DE LA FORCE LINEIQUE AUX POINTS D'INTEGRATION :
C        -----------------------------------------------------
          DO 30 INO = 1,NNO

            FXLIN(IGAU) = FXLIN(IGAU) + ZR(IVF+IDECPG+INO-1)*
     &                    ZR(IDFLIN+1-1)
            FYLIN(IGAU) = FYLIN(IGAU) + ZR(IVF+IDECPG+INO-1)*
     &                    ZR(IDFLIN+2-1)
            FZLIN(IGAU) = FZLIN(IGAU) + ZR(IVF+IDECPG+INO-1)*
     &                    ZR(IDFLIN+3-1)
   30     CONTINUE
   40   CONTINUE

C ---    BOUCLE SUR LES POINTS D'INTEGRATION
C        -----------------------------------
        DO 60 IGAU = 1,NPG

          IDECPG = NNO* (IGAU-1)

C ---    CALCUL DU PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
C ---    COURANT :
C        -------
          CALL VFF3D(NNO,ZR(IPOIDS+IGAU-1),ZR(IDFDK+IDECPG),ZR(IGEOM),
     &               JACOB)

C ---    CALCUL DE LA CONTRIBUTION AU VECTEUR DES FORCES NODALES
C ---    DU CHARGEMENT LINEIQUE AU POINT D'INTEGRATION COURANT :
C        -----------------------------------------------------
          DO 50 INO = 1,NNO

            FX(INO) = FX(INO) + ZR(IVF+IDECPG+INO-1)*FXLIN(IGAU)*JACOB
            FY(INO) = FY(INO) + ZR(IVF+IDECPG+INO-1)*FYLIN(IGAU)*JACOB
            FZ(INO) = FZ(INO) + ZR(IVF+IDECPG+INO-1)*FZLIN(IGAU)*JACOB
   50     CONTINUE

   60   CONTINUE

C --- OPTION 'CHAR_MECA_FF1D3D'
C --- CAS OU LES DONNEES DES FORCES LINEIQUES SONT DES FONCTIONS
C --- DES COORDONNEES ET DU TEMPS :
C     ---------------------------
      ELSE IF (OPTION.EQ.'CHAR_MECA_FF1D3D') THEN

C ---  RECUPERATION DES NOMS DES FONCTIONS REPRESENTANT LA FORCE
C ---  LINEIQUE A APPLIQUER SUR L'ELEMENT D'ARETE :
C      ------------------------------------------
        CALL JEVECH('PFF1D3D','L',IDFLIN)

C ---  RECUPERATION DE L'INSTANT D'INTERPOLATION :
C      -----------------------------------------
        CALL JEVECH('PTEMPSR','L',ITEMPS)

C ---  AFFECTATION DES VARIABLES POUR L'INTERPOLATION :
C      ----------------------------------------------
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'

        VALPAR(4) = ZR(ITEMPS)

C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
        DO 100 IGAU = 1,NPG

          IDECPG = NNO* (IGAU-1)

          DO 70 I = 1,3
            XYZGAU(I) = ZERO
            FORLIN(I) = ZERO
   70     CONTINUE

C ---    CALCUL DES COORDONNEES DU POINT D'INTEGRATION COURANT :
C        -----------------------------------------------------
          DO 80 INO = 1,NNO

            IDECNO = 3* (INO-1) - 1

            XYZGAU(1) = XYZGAU(1) + ZR(IVF+IDECPG+INO-1)*
     &                  ZR(IGEOM+1+IDECNO)
            XYZGAU(2) = XYZGAU(2) + ZR(IVF+IDECPG+INO-1)*
     &                  ZR(IGEOM+2+IDECNO)
            XYZGAU(3) = XYZGAU(3) + ZR(IVF+IDECPG+INO-1)*
     &                  ZR(IGEOM+3+IDECNO)
   80     CONTINUE

C ---    INTERPOLATION DES FORCES LINEIQUES EN FONCTION DES
C ---    COORDONNEES ET DU TEMPS :
C        -----------------------
          VALPAR(1) = XYZGAU(1)
          VALPAR(2) = XYZGAU(2)
          VALPAR(3) = XYZGAU(3)

          CALL FOINTE('FM',ZK8(IDFLIN+1-1),4,NOMPAR,VALPAR,FORLIN(1),
     &                IER)
          CALL FOINTE('FM',ZK8(IDFLIN+2-1),4,NOMPAR,VALPAR,FORLIN(2),
     &                IER)
          CALL FOINTE('FM',ZK8(IDFLIN+3-1),4,NOMPAR,VALPAR,FORLIN(3),
     &                IER)

C ---    CALCUL DU PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
C ---    COURANT :
C        -------
          CALL VFF3D(NNO,ZR(IPOIDS+IGAU-1),ZR(IDFDK+IDECPG),ZR(IGEOM),
     &               JACOB)

C ---    CALCUL DE LA CONTRIBUTION AU VECTEUR DES FORCES NODALES
C ---    DU CHARGEMENT LINEIQUE AU POINT D'INTEGRATION COURANT :
C        -----------------------------------------------------
          DO 90 INO = 1,NNO

            FX(INO) = FX(INO) + ZR(IVF+IDECPG+INO-1)*FORLIN(1)*JACOB
            FY(INO) = FY(INO) + ZR(IVF+IDECPG+INO-1)*FORLIN(2)*JACOB
            FZ(INO) = FZ(INO) + ZR(IVF+IDECPG+INO-1)*FORLIN(3)*JACOB
   90     CONTINUE

  100   CONTINUE

      END IF

C ---- RECUPERATION ET AFFECTATION DU VECTEUR FORCES NODALES EN SORTIE :
C      ---------------------------------------------------------------
      CALL JEVECH('PVECTUR','E',IVECTU)

      DO 110 INO = 1,NNO
        ZR(IVECTU+3* (INO-1)+1-1) = FX(INO)
        ZR(IVECTU+3* (INO-1)+2-1) = FY(INO)
        ZR(IVECTU+3* (INO-1)+3-1) = FZ(INO)
  110 CONTINUE

      END
