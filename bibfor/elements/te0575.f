      SUBROUTINE TE0575(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16      OPTION,NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C FONCTIONS REALISEES:
C
C      CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE THERMOELASTIQUE
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 2D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ENEL_ELGA'
C
C      OPTIONS : 'ENEL_ELGA'
C
C      CALCUL DE LA DENSITE D'ENERGIE TOTALE
C      A L'EQUILIBRE POUR LES ELEMENTS ISOPARAMETRIQUES 2D
C      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ETOT_ELGA'
C
C      OPTIONS : 'ETOT_ELGA'
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
C-----------------------------------------------------------------------
      INTEGER I ,IDENER ,IDEPL ,IDEPLM ,IDEPMM ,IDFDE, IDVARI
      INTEGER IDSIG ,IDSIGM ,IGAU ,IGEOM ,IMATE ,IPOIDS ,ISIG, NBVARI
      INTEGER ITEMPS ,IVF ,JGANO ,K ,MXCMEL ,NBCONT
      INTEGER NBNOMX ,NBSIG  ,NDIM ,NNO ,NNOS ,NPG,JTAB(7)

      REAL*8 DEUX ,ENELEM ,POIDS ,RAYON ,UNDEMI ,ZERO
C-----------------------------------------------------------------------
      PARAMETER          (MXCMEL = 162)
      PARAMETER          (NBNOMX = 27)
      PARAMETER          (NBCONT =  6)
      INTEGER            NBSIGM, IHARMO, NH, IDIM, IRET
      INTEGER            IDENEM
      REAL*8             EPSI(NBCONT), EPSIM(NBCONT), DELTA(NBCONT)
      REAL*8             NHARM, REPERE(7),INSTAN
      REAL*8             ENERPG(NBNOMX), EPSS(MXCMEL),R
      REAL*8             XYZGAU(3),BARY(3),F(3,3)
      REAL*8             EPSSM(MXCMEL), SIGMM(NBCONT), SIGMA(NBCONT)
      REAL*8             INTEG1, INTEG2, INTEG,EPSBID(6),DFDBID(27*3)
      REAL*8             DFDX(9), DFDY(9)
      CHARACTER*4        FAMI
      CHARACTER*16       COMPOR(3)
      LOGICAL            LTEATT,GRAND,AXI
C
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
C
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG  = NBSIGM()
C
C --- INITIALISATIONS :
C     -----------------
      ZERO        = 0.0D0
      UNDEMI      = 0.5D0
      DEUX        = 2.0D0
      NHARM       = ZERO
      ENELEM      = ZERO
C
      DO 10 I = 1, NBNOMX
         ENERPG(I) = ZERO
 10   CONTINUE
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      IF (OPTION(1:4).EQ.'ENEL') THEN
C
C ----   RECUPERATION DU MATERIAU
C        ------------------------
         CALL JEVECH('PMATERC','L',IMATE)
C
C ----   RECUPERATION  DES DONNEES RELATIVES AU REPERE D'ORTHOTROPIE
C         -----------------------------------------------------------
C        COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )

         BARY(1) = 0.D0
         BARY(2) = 0.D0
         BARY(3) = 0.D0
         DO 150 I = 1,NNO
            DO 140 IDIM = 1,NDIM
               BARY(IDIM) = BARY(IDIM)+ZR(IGEOM+IDIM+NDIM*(I-1)-1)/NNO
 140        CONTINUE
 150     CONTINUE

         CALL ORTREP(ZI(IMATE),NDIM,BARY,REPERE)
C
C ---    RECUPERATION DU CHAMP DE DEPLACEMENT A L'INSTANT COURANT :
C        --------------------------------------------------------
         CALL JEVECH('PDEPLAR','L',IDEPL)

C ----    RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION
C         -------------------------------------------------------------
         CALL JEVECH('PCONTRR','L',IDSIG)

C ----    RECUPERATION DE L'INSTANT DE CALCUL
C         -----------------------------------
         CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
         IF (ITEMPS.NE.0)  INSTAN = ZR(ITEMPS)

C ----   RECUPERATION DU CHAMP DE VARIABLES INTERNES  :
C        N'EXISTE PAS EN LINEAIRE
         CALL TECACH('ONN','PVARIGR',7,JTAB,IRET)
         IF (IRET.EQ.0) THEN
            IDVARI=JTAB(1)
            NBVARI = MAX(JTAB(6),1)*JTAB(7)
         ELSE
            IDVARI=1
            NBVARI=0
         ENDIF

      END IF

C ----RECUPERATION DU TYPE DE COMPORTEMENT  :
C     N'EXISTE PAS EN LINEAIRE
      CALL TECACH('NNN','PCOMPOR',7,JTAB,IRET)
      COMPOR(1)='ELAS'
      COMPOR(2)=' '
      COMPOR(3)='PETIT'
      IF (IRET.EQ.0) THEN
         COMPOR(1)=ZK16(JTAB(1))
         COMPOR(3)=ZK16(JTAB(1)+2)
      ENDIF

C     GRANDES DEFORMATIONS

      IF ((COMPOR(3).EQ.'SIMO_MIEHE').OR.
     &    (COMPOR(3).EQ.'GDEF_LOG').OR.
     &    (COMPOR(3).EQ.'GDEF_HYPO_ELAS')) THEN
         GRAND = .TRUE.
      ELSE
         GRAND = .FALSE.
      ENDIF


C --- CAS DU CALCUL DE LA DENSITE D'ENERGIE TOTALE :
C     ============================================
      IF (OPTION(1:4).EQ.'ETOT') THEN

         IF (GRAND) THEN
            CALL U2MESG('F','COMPOR1_79',1,COMPOR(3),0,0,0,0.D0)
         ENDIF
C
C ---   RECUPERATION DU CHAMP DE DEPLACEMENT A L'INSTANT COURANT :
C       --------------------------------------------------------
        CALL JEVECH('PDEPLR','L',IDEPL)
C
C ---   RECUPERATION EVENTUELLE DU CHAMP DE DEPLACEMENT A
C ---   L'INSTANT PRECEDENT :
C       -------------------
        CALL TECACH('NNN','PDEPLM',1,IDEPLM,IRET)
        IF (IDEPLM.NE.0) THEN
           CALL JEVECH('PDEPLM','L',IDEPMM)
        ENDIF
C
C ---   RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION
C ---   A L'INSTANT COURANT :
C       -------------------
        CALL JEVECH('PCONTPR','L',IDSIG)
C
C ---   RECUPERATION EVENTUELLE DU CHAMP DE CONTRAINTES A
C ---   L'INSTANT PRECEDENT :
C       -------------------
        CALL TECACH('NNN','PCONTMR',1,IDSIGM,IRET)
        IF (IDSIGM.NE.0) THEN
          CALL JEVECH('PCONTMR','L',IDSIGM)
        ENDIF
C
C ---   RECUPERATION EVENTUELLE DU NUMERO D'HARMONIQUE :
C       ----------------------------------------------
        CALL TECACH('NNN','PHARMON',1,IHARMO,IRET)
        IF (IHARMO.NE.0) THEN
          NH    = ZI(IHARMO)
          NHARM = DBLE(NH)
        ENDIF
C
C ---   CALCUL DU CHAMP DE DEFORMATIONS AU PREMIER ORDRE
C ---   CORRESPONDANT AU CHAMP DE DEPLACEMENT COURANT :
C       ---------------------------------------------
        CALL EPS1MC(NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     +              ZR(IGEOM),ZR(IDEPL),NHARM,EPSS)
C
C ---   CALCUL EVENTUEL DU CHAMP DE DEFORMATIONS AU PREMIER ORDRE
C ---   CORRESPONDANT AU CHAMP DE DEPLACEMENT A L'INSTANT PRECEDENT :
C       -----------------------------------------------------------
        IF (IDEPLM.NE.0) THEN
          CALL EPS1MC(NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     +                ZR(IGEOM),ZR(IDEPMM),NHARM,EPSSM)
        ENDIF
C
      ENDIF
C
C ---- BOUCLE SUR LES POINTS D'INTEGRATION :
C      ===================================
      DO 20 IGAU = 1, NPG
C
         K=(IGAU-1)*NNO
C
C  --   CALCUL DES DERIVEES DES FONCTIONS DE FORME ET DU PRODUIT
C  --   JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
C  --   AU POINT D'INTEGRATION COURANT :
C       ------------------------------
C
         CALL DFDM2D ( NNO,IGAU,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
         AXI=.FALSE.
         IF ((LTEATT(' ','AXIS','OUI')).OR.
     &       (LTEATT(' ','FOURIER','OUI'))) THEN
            RAYON = ZERO
            DO 41 I = 1, NNO
               RAYON = RAYON + ZR(IVF+K+I-1)*ZR(IGEOM+NDIM*(I-1))
  41        CONTINUE
            POIDS=POIDS*RAYON
            AXI=.TRUE.
         ENDIF
         DO 30 ISIG = 1, NBSIG
            EPSI(ISIG) = ZERO
  30     CONTINUE
C
C  --      COORDONNEES ET TEMPERATURE AU POINT D'INTEGRATION
C  --      COURANT
C          -------
         XYZGAU(1) = ZERO
         XYZGAU(2) = ZERO
         XYZGAU(3) = ZERO
C
         DO 40 I = 1, NNO
C
            DO 50 IDIM = 1, NDIM
               XYZGAU(IDIM) = XYZGAU(IDIM) +
     +                        ZR(IVF+I+NNO*(IGAU-1)-1)*
     +                        ZR(IGEOM-1+IDIM+NDIM*(I-1))
  50        CONTINUE
C
  40     CONTINUE
C
C  --    CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE THERMOELASTIQUE :
C        ==========================================================
        IF (OPTION(1:4).EQ.'ENEL') THEN

C --- TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :

          DO 51 I = 1,NBSIG
            SIGMA(I) = ZR(IDSIG+ (IGAU-1)*NBSIG+I-1)
   51     CONTINUE


C ---     CALCUL DU JACOBIEN AU POINT D'INTEGRATION COURANT :
          CALL NMGEOM(2,NNO,AXI,GRAND,ZR(IGEOM),IGAU,IPOIDS,
     &              IVF,IDFDE,ZR(IDEPL),.TRUE.,POIDS,DFDBID,F,EPSBID,R)

C ---     CALCUL DE L'ENERGIE ELASTIQUE AU POINT D'INTEGRATION COURANT

          CALL ENELPG(FAMI,ZI(IMATE),INSTAN,IGAU,REPERE,XYZGAU,COMPOR,
     &    F,SIGMA,NBVARI,ZR(IDVARI+(IGAU-1)*NBVARI),ENERPG(IGAU))


C  --    CALCUL DE LA DENSITE D'ENERGIE TOTALE :
C        =====================================
         ELSEIF (OPTION(1:4).EQ.'ETOT') THEN
C
C  --      TENSEURS DES DEFORMATIONS  ET DES CONTRAINTES AU PAS DE
C  --      TEMPS COURANT ET AU PAS DE TEMPS PRECEDENT S'IL Y A LIEU,
C  --      AU POINT D'INTEGRATION COURANT :
C          ------------------------------
           DO 80 I = 1, NBSIG
              EPSI(I) = EPSS(I+(IGAU-1)*NBSIG)
              IF (IDEPLM.NE.0) THEN
                 EPSIM(I) = EPSSM(I+(IGAU-1)*NBSIG)
              ENDIF
              SIGMA(I) = ZR(IDSIG+(IGAU-1)*NBSIG+I-1)
              IF (IDSIGM.NE.0) THEN
                 SIGMM(I) = ZR(IDSIGM+(IGAU-1)*NBSIG+I-1)
              ENDIF
  80       CONTINUE
C
           IF (IDEPLM.NE.0) THEN
              DO 90 I = 1, NBSIG
                 DELTA(I) = EPSI(I) - EPSIM(I)
  90          CONTINUE
           ELSE
              DO 91 I = 1, NBSIG
                 DELTA(I) = 0.D0
  91          CONTINUE
           ENDIF
C
C  --      CALCUL DES TERMES A SOMMER POUR OBTENIR LA DENSITE
C  --      D'ENERGIE TOTALE :
C          ----------------
           IF (IDEPLM.NE.0.AND.IDSIGM.NE.0) THEN
             INTEG1 =       SIGMA(1)*DELTA(1) +      SIGMA(2)*DELTA(2)
     +               +      SIGMA(3)*DELTA(3) + DEUX*SIGMA(4)*DELTA(4)
             IF (LTEATT(' ','FOURIER','OUI')) INTEG1 = INTEG1 +
     &                 DEUX*SIGMA(5)*DELTA(5) + DEUX*SIGMA(6)*DELTA(6)
C
             INTEG2 =       SIGMM(1)*DELTA(1) +      SIGMM(2)*DELTA(2)
     +               +      SIGMM(3)*DELTA(3) + DEUX*SIGMM(4)*DELTA(4)
             IF (LTEATT(' ','FOURIER','OUI')) INTEG2 = INTEG2 +
     &                 DEUX*SIGMM(5)*DELTA(5) + DEUX*SIGMM(6)*DELTA(6)
C
             ENERPG(IGAU) = UNDEMI*(INTEG1+INTEG2)
           ELSE
C
C  --        CAS D'ORDRE NUMERO 1 :
C            --------------------
             INTEG  =       SIGMA(1)*EPSI(1) +      SIGMA(2)*EPSI(2)
     +               +      SIGMA(3)*EPSI(3) + DEUX*SIGMA(4)*EPSI(4)
             IF (LTEATT(' ','FOURIER','OUI')) INTEG = INTEG +
     &                 DEUX*SIGMA(5)*EPSI(5) + DEUX*SIGMA(6)*EPSI(6)
C
             ENERPG(IGAU) = UNDEMI*INTEG
C
           ENDIF
C
           ENELEM  = ENELEM + ENERPG(IGAU)*POIDS
C
         ENDIF
C
  20  CONTINUE
C
C ---- RECUPERATION DU CHAMP DES DENSITES D'ENERGIE DE DEFORMATION
C ---- ELASTIQUE EN SORTIE
C      -------------------
      CALL JEVECH('PENERDR','E',IDENER)
C
C --- OPTIONS ENEL_* ET ETOT_*
C     ==============================
      IF (OPTION(1:4).EQ.'ETOT') THEN
        CALL JEVECH('PENERDM','L',IDENEM)
C
C ---   OPTION ETOT_ELGA
C       ================
        IF (OPTION.EQ.'ETOT_ELGA') THEN
          DO 100 IGAU = 1, NPG
            ZR(IDENER+IGAU-1)=ZR(IDENEM+IGAU-1)+ENERPG(IGAU)
 100      CONTINUE
C
C ---   OPTION ETOT_ELEM
C       ================
        ELSEIF (OPTION.EQ.'ETOT_ELEM') THEN
          ZR(IDENER) = ZR(IDENEM)+ENELEM
        ENDIF
      ELSE
C
C ---   OPTION ENEL_ELGA
C       ================
        IF (OPTION.EQ.'ENEL_ELGA') THEN
          DO 200 IGAU = 1, NPG
            ZR(IDENER+IGAU-1) = ENERPG(IGAU)
 200      CONTINUE
        ENDIF
      ENDIF
C
      END
