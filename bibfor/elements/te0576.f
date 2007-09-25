      SUBROUTINE TE0576(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/09/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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

C FONCTIONS REALISEES:

C      CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE THERMOELASTIQUE
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 3D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ENEL_ELGA'
C      .SOIT AUX NOEUDS               : OPTION 'ENEL_ELNO_ELGA'

C      OPTIONS : 'ENEL_ELGA'
C                'ENEL_ELNO_ELGA'

C      CALCUL DE LA DENSITE D'ENERGIE TOTALE
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 3D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ETOT_ELGA'
C      .SOIT AUX NOEUDS               : OPTION 'ETOT_ELNO_ELGA'

C      OPTIONS : 'ETOT_ELGA'
C                'ETOT_ELNO_ELGA'


C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
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

      INTEGER JGANO,NBSIGM,NDIM,NNO,I,NNOS,JVAL,IPOIDS,IVF,NBNOMX,
     &        NBCONT,NPG1,NBSIG,IGAU,ISIG,INO,IGEOM,IDIM,ITEMPS,
     &        IMATE,JSIG,IDENER,IDFDE,IDEPL,IDEPLM,IDEPMM,
     &        IDSIG,IDSIGM,MXCMEL,IRET
      PARAMETER (NBNOMX=27)
      PARAMETER (NBCONT=6)
      PARAMETER (MXCMEL=162)
      REAL*8 EPSI(NBCONT),REPERE(7)
      REAL*8 INSTAN,ZERO,UNDEMI,ENELEM
      REAL*8 ENERPG(NBNOMX),ENERNO(NBNOMX)
      REAL*8 D1(NBCONT,NBCONT),XYZGAU(3),XYZ(3)
      REAL*8 NHARM,DEUX,INTEG1,INTEG2,INTEG
      REAL*8 EPSIM(NBCONT),DELTA(NBCONT),EPSS(MXCMEL)
      REAL*8 EPSSM(MXCMEL),SIGMM(NBCONT),SIGMA(NBCONT)
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS
      CHARACTER*4 FAMI
C DEB ------------------------------------------------------------------

C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG = NBSIGM()

C --- INITIALISATIONS :
C     -----------------
      ZERO = 0.0D0
      UNDEMI = 0.5D0
      DEUX = 2.0D0
      INSTAN = ZERO
      NHARM = ZERO
      ENELEM = ZERO

      DO 20 I = 1,NBNOMX
        ENERPG(I) = ZERO
        ENERNO(I) = ZERO
   20 CONTINUE

C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

      IF (OPTION(1:4).EQ.'ENEL') THEN

C ----   RECUPERATION DU MATERIAU
C        ------------------------
        CALL JEVECH('PMATERC','L',IMATE)

C ----   RECUPERATION  DES DONNEES RELATIVES AU REPERE D'ORTHOTROPIE
C        -----------------------------------------------------------
C       COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
        XYZ(1) = ZERO
        XYZ(2) = ZERO
        XYZ(3) = ZERO
        DO 150 I = 1,NNO
          DO 140 IDIM = 1,NDIM
            XYZ(IDIM) = XYZ(IDIM)+ZR(IGEOM+IDIM+NDIM*(I-1)-1)/NNO
140       CONTINUE
150     CONTINUE
        CALL ORTREP(ZI(IMATE),NDIM,XYZ,REPERE)

C ----   RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION
C        -------------------------------------------------------------
        CALL JEVECH('PCONTRR','L',IDSIG)

C ----   RECUPERATION DE L'INSTANT DE CALCUL
C        -----------------------------------
        CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
        IF (ITEMPS.NE.0) THEN
          INSTAN = ZR(ITEMPS)
        END IF

      END IF

C --- CAS DU CALCUL DE LA DENSITE D'ENERGIE TOTALE :
C     ============================================
      IF (OPTION(1:4).EQ.'ETOT') THEN

C ---   RECUPERATION DU CHAMP DE DEPLACEMENT A L'INSTANT COURANT :
C       --------------------------------------------------------
        CALL JEVECH('PDEPLR','L',IDEPL)

C ---   RECUPERATION EVENTUELLE DU CHAMP DE DEPLACEMENT A
C ---   L'INSTANT PRECEDENT :
C       -------------------
        CALL TECACH('NNN','PDEPLM',1,IDEPLM,IRET)
        IF (IDEPLM.NE.0) THEN
          CALL JEVECH('PDEPLM','L',IDEPMM)
        END IF

C ---   RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION
C ---   A L'INSTANT COURANT :
C       -------------------
        CALL JEVECH('PCONTPR','L',IDSIG)

C ---   RECUPERATION EVENTUELLE DU CHAMP DE CONTRAINTES A
C ---   L'INSTANT PRECEDENT :
C       -------------------
        CALL TECACH('NNN','PCONTMR',1,IDSIGM,IRET)
        IF (IDSIGM.NE.0) THEN
          CALL JEVECH('PCONTMR','L',IDSIGM)
        END IF

C ---   CALCUL DU CHAMP DE DEFORMATIONS AU PREMIER ORDRE
C ---   CORRESPONDANT AU CHAMP DE DEPLACEMENT COURANT :
C       ---------------------------------------------
        CALL EPS1MC(NNO,NDIM,NBSIG,NPG1,IPOIDS,IVF,IDFDE,
     +              ZR(IGEOM),ZR(IDEPL),NHARM,EPSS)

C ---   CALCUL EVENTUEL DU CHAMP DE DEFORMATIONS AU PREMIER ORDRE
C ---   CORRESPONDANT AU CHAMP DE DEPLACEMENT A L'INSTANT PRECEDENT :
C       -----------------------------------------------------------
        IF (IDEPLM.NE.0) THEN
        CALL EPS1MC(NNO,NDIM,NBSIG,NPG1,IPOIDS,IVF,IDFDE,
     +              ZR(IGEOM),ZR(IDEPMM),NHARM,EPSSM)
        END IF

      END IF

C ---- BOUCLE SUR LES POINTS D'INTEGRATION :
C      ===================================
      DO 110 IGAU = 1,NPG1

C  --    CALCUL DU JACOBIEN AU POINT D'INTEGRATION COURANT :
C        -------------------------------------------------
        CALL DFDM3D ( NNO, IGAU, IPOIDS, IDFDE,
     &                ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )

        DO 30 ISIG = 1,NBSIG
          EPSI(ISIG) = ZERO
   30   CONTINUE

C  --      COORDONNEES AU POINT D'INTEGRATION
C  --      COURANT
C          -------
        XYZGAU(1) = ZERO
        XYZGAU(2) = ZERO
        XYZGAU(3) = ZERO

        DO 50 I = 1,NNO

          DO 40 IDIM = 1,NDIM
            XYZGAU(IDIM) = XYZGAU(IDIM) +
     &                     ZR(IVF+I+NNO* (IGAU-1)-1)*ZR(IGEOM+IDIM+
     &                     NDIM* (I-1)-1)
   40     CONTINUE

   50   CONTINUE

C  --    CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE THERMOELASTIQUE :
C        ==========================================================
        IF (OPTION(1:4).EQ.'ENEL') THEN

C  --      CALCUL DE L'INVERSE DE LA MATRICE DE HOOKE (LE MATERIAU
C  --      POUVANT ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C          ---------------------------------------------------------
          CALL D1MAMC(FAMI,ZI(IMATE),INSTAN,'+',IGAU,1,
     &                REPERE,XYZGAU,NBSIG,D1)

C  --      DENSITE D'ENERGIE POTENTIELLE ELASTIQUE AU POINT
C  --      D'INTEGRATION COURANT
C          ---------------------
          DO 70 ISIG = 1,NBSIG
            DO 60 JSIG = 1,NBSIG
              EPSI(ISIG) = EPSI(ISIG) + D1(ISIG,JSIG)*
     &                     ZR(IDSIG+NBSIG* (IGAU-1)+JSIG-1)
   60       CONTINUE

            ENERPG(IGAU) = ENERPG(IGAU) +
     &                     UNDEMI*ZR(IDSIG+NBSIG* (IGAU-1)+ISIG-1)*
     &                     EPSI(ISIG)
   70     CONTINUE

C  --    CALCUL DE LA DENSITE D'ENERGIE TOTALE :
C        =====================================
        ELSE IF (OPTION(1:4).EQ.'ETOT') THEN

C  --      TENSEURS DES DEFORMATIONS  ET DES CONTRAINTES AU PAS DE
C  --      TEMPS COURANT ET AU PAS DE TEMPS PRECEDENT S'IL Y A LIEU,
C  --      AU POINT D'INTEGRATION COURANT :
C          ------------------------------
          DO 80 I = 1,NBSIG
            EPSI(I) = EPSS(I+ (IGAU-1)*NBSIG)
            IF (IDEPLM.NE.0) THEN
              EPSIM(I) = EPSSM(I+ (IGAU-1)*NBSIG)
            END IF
            SIGMA(I) = ZR(IDSIG+ (IGAU-1)*NBSIG+I-1)
            IF (IDSIGM.NE.0) THEN
              SIGMM(I) = ZR(IDSIGM+ (IGAU-1)*NBSIG+I-1)
            END IF
   80     CONTINUE

          IF (IDEPLM.NE.0) THEN
            DO 90 I = 1,NBSIG
              DELTA(I) = EPSI(I) - EPSIM(I)
   90       CONTINUE
          ELSE
            DO 100 I = 1,NBSIG
              DELTA(I) = 0.D0
  100       CONTINUE
          END IF

C  --      CALCUL DES TERMES A SOMMER POUR OBTENIR LA DENSITE
C  --      D'ENERGIE TOTALE :
C          ----------------
          INTEG1 = SIGMA(1)*DELTA(1) + SIGMA(2)*DELTA(2) +
     &             SIGMA(3)*DELTA(3) + DEUX*SIGMA(4)*DELTA(4) +
     &             DEUX*SIGMA(5)*DELTA(5) + DEUX*SIGMA(6)*DELTA(6)

          IF (IDEPLM.NE.0 .AND. IDSIGM.NE.0) THEN
            INTEG2 = SIGMM(1)*DELTA(1) + SIGMM(2)*DELTA(2) +
     &               SIGMM(3)*DELTA(3) + DEUX*SIGMM(4)*DELTA(4) +
     &               DEUX*SIGMM(5)*DELTA(5) + DEUX*SIGMM(6)*DELTA(6)

            ENERPG(IGAU) = UNDEMI* (INTEG1+INTEG2)
          ELSE

C  --        CAS D'ORDRE NUMERO 1 :
C            --------------------
            INTEG = SIGMA(1)*EPSI(1) + SIGMA(2)*EPSI(2) +
     &              SIGMA(3)*EPSI(3) + DEUX*SIGMA(4)*EPSI(4) +
     &              DEUX*SIGMA(5)*EPSI(5) + DEUX*SIGMA(6)*EPSI(6)

            ENERPG(IGAU) = UNDEMI*INTEG

          END IF

          ENELEM = ENELEM + ENERPG(IGAU)*POIDS

        END IF

  110 CONTINUE

C ---- RECUPERATION DU CHAMP DES DENSITES D'ENERGIE DE DEFORMATION
C ---- ELASTIQUE EN SORTIE
C      -------------------
      CALL JEVECH('PENERDR','E',IDENER)

C ---- OPTIONS ENEL_ELGA ET ETOT_ELGA
C      ==============================
      IF (OPTION(6:9).EQ.'ELGA') THEN
        DO 120 IGAU = 1,NPG1
          ZR(IDENER+IGAU-1) = ENERPG(IGAU)
  120   CONTINUE

C ---- OPTION ENEL_ELNO_ELGA ET ETOT_ELNO_ELGA
C      =======================================
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN

C ----   DENSITE D'ENERGIE DE DEFORMATION AUX NOEUDS
C        -------------------------------------------
        CALL PPGAN2(JGANO,1,ENERPG,ENERNO)

        DO 130 INO = 1,NNO
          ZR(IDENER+INO-1) = ENERNO(INO)
  130   CONTINUE

C ---- OPTION ETOT_ELEM
C      ================
      ELSE IF (OPTION(6:9).EQ.'ELEM') THEN
        ZR(IDENER) = ENELEM
      END IF

      END
