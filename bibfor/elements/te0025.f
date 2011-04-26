      SUBROUTINE TE0025(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C.......................................................................
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

C     BUT: CALCUL DES DEFORMATIONS AUX POINTS D'INTEGRATION
C          OU AUX NOEUDS DES ELEMENTS ISOPARAMETRIQUES 3D

C          OPTIONS : 'EPSI_ELNO'
C                    'EPSI_ELGA'
C                    'EPSG_ELNO'
C                    'EPSG_ELGA'
C                    'EPME_ELNO  '
C                    'EPME_ELGA  '
C                    'EPMG_ELNO  '
C                    'EPMG_ELGA  '

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER JGANO,NBSIGM,NDIM,NNO,I,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &        NBSIG,IGAU,ISIG,INO,IGEOM,IDEPL,
     &        ITEMPS,IDEFO,IMATE,IRET,IDIM
      REAL*8 EPSM(162),EPSNO(162),REPERE(7),BARY(3)
      REAL*8 NHARM,INSTAN,ZERO,S
C DEB ------------------------------------------------------------------

C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      IF (OPTION(6:9).EQ.'ELGA') THEN
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
C       -- ON AURAIT AIME PRENDRE 'NOGA' MAIS SI IL EXISTE DE
C          L'HYDRATATION, IL FAUT SE SOUMETTRE ...
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF


C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG = NBSIGM()

C --- INITIALISATIONS :
C     -----------------
      ZERO = 0.0D0
      INSTAN = ZERO
      NHARM = ZERO

      DO 20 I = 1,NBSIG*NPG
        EPSM(I) = ZERO
   20 CONTINUE

C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT :
C      --------------------------------------------------
      CALL JEVECH('PDEPLAR','L',IDEPL)

C ---- RECUPERATION DE L'INSTANT DE CALCUL :
C      -----------------------------------
      CALL TECACH('NNN','PTEMPSR',1,ITEMPS,IRET)
      IF (ITEMPS.NE.0) THEN
        INSTAN = ZR(ITEMPS)
      END IF

C ---- RECUPERATION DU VECTEUR DES DEFORMATIONS EN SORTIE :
C      --------------------------------------------------
      CALL JEVECH('PDEFORR','E',IDEFO)

C ---- CALCUL DES DEFORMATIONS MECANIQUES AUX POINTS D'INTEGRATION
C ---- DE L'ELEMENT , I.E. SI ON NOTE EPSI_MECA = B*U
C ---- ON CALCULE SIMPLEMENT EPSI_MECA POUR LES OPTIONS EPSI ET EPSG
C ----                    ET EPSI_MECA - EPSI_THERMIQUES POUR LES
C ----                    OPTIONS EPME ET EPMG :
C      ---------------------------------------
      CALL TECACH('NNN','PMATERC',1,IMATE,IRET)

C ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
C      ------------------------------------------------------------
C     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )

      BARY(1) = 0.D0
      BARY(2) = 0.D0
      BARY(3) = 0.D0
      DO 150 I = 1,NNO
        DO 140 IDIM = 1,NDIM
          BARY(IDIM) = BARY(IDIM)+ZR(IGEOM+IDIM+NDIM*(I-1)-1)/NNO
 140    CONTINUE
 150  CONTINUE
      CALL ORTREP(ZI(IMATE),NDIM,BARY,REPERE)

      CALL EPSVMC('RIGI', NNO, NDIM, NBSIG, NPG, IPOIDS,IVF,
     +            IDFDE,ZR(IGEOM), ZR(IDEPL),
     +            INSTAN,
     +            ZI(IMATE), REPERE, NHARM, OPTION, EPSM)

      IF (OPTION(6:9).EQ.'ELGA') THEN
C         --------------------
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---- POINTS D'INTEGRATION :
C      --------------------
        DO 80 IGAU = 1,NPG
          DO 70 ISIG = 1,NBSIG
            ZR(IDEFO+NBSIG* (IGAU-1)+ISIG-1) = EPSM(NBSIG* (IGAU-1)+
     &        ISIG)
   70     CONTINUE
   80   CONTINUE

      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN

C ---- DEFORMATIONS AUX NOEUDS :
C      -----------------------

        CALL PPGAN2(JGANO,NBSIG,EPSM,EPSNO)
        S = 0.D0

C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---- NOEUDS :
C      ------
        DO 100 INO = 1,NNO
          DO 90 ISIG = 1,NBSIG
            ZR(IDEFO+NBSIG* (INO-1)+ISIG-1) = EPSNO(NBSIG* (INO-1)+ISIG)
            S = S + EPSNO(NBSIG* (INO-1)+ISIG)
   90     CONTINUE
  100   CONTINUE

      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      END
