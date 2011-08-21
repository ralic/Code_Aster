      SUBROUTINE TE0528(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/08/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================

C     BUT: CALCUL DES DEFORMATIONS DUES :
C         AU FLUAGE DE DESSICCATION
C          POUR LE MODELE BETON_UMLV_FP
C         AU FLUAGE PROPRE POUR LES MODELES BETON_UMLV_FP ET GRANGER
C          AUX NOEUDS ET PG
C          ELEMENTS ISOPARAMETRIQUES 3D/D_PLAN/AXIS


C     IN   OPTION : OPTIONS DE CALCUL
C                   'EPFD_ELNO'   'EPFD_ELGA'
C                      'EPFP_ELNO'  'EPFP_ELGA'
C          NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER JGANO,MXCMEL,NBSGM,I,NDIM,NNO,NBSIGM,NBSIG,NBSP,
     &        NNOS,NPG,IPOIDS,IVF,IDFDE,IGAU,ISIG,INO,IGEOM,
     &        IDEF,ICOMPO,NBVARI,IVARI,
     &        JTAB(7),IRET,IMATE,ITEMPS
      PARAMETER (MXCMEL=162)
      PARAMETER (NBSGM=6)
      REAL*8 EPSFL(MXCMEL),EPSFLN(MXCMEL),EPSTMP(NBSGM)
      REAL*8 VALPAR,NU
      INTEGER ICODRE
      CHARACTER*8 NOMRES,NOMPAR
      CHARACTER*16 COMPO1,COMPO2,VALK(2)
      LOGICAL LFLU
C DEB ------------------------------------------------------------------

C --- INITIALISATIONS :
C     ---------------

C --- RECUPERATION DU COMPORTEMENT  :
C     -------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)

      COMPO1=ZK16(ICOMPO)
      COMPO2=ZK16(ICOMPO+7)

C    VERIFICATION DU COMPORTEMENT FLUAGE
      LFLU=.FALSE.
      IF (OPTION(1:4).EQ.'EPFD') THEN
         IF ((COMPO1(1:13).EQ.'BETON_UMLV_FP').OR.
     &        (COMPO1(1:7).EQ.'KIT_DDI'.AND.COMPO2(1:13).EQ.
     &        'BETON_UMLV_FP'))  THEN
            LFLU=.TRUE.
         ENDIF
        ELSEIF (OPTION(1:4).EQ.'EPFP') THEN
          IF ( (COMPO1(1:13).EQ.'BETON_UMLV_FP') .OR.
     &          (COMPO1(1:10).EQ.'GRANGER_FP') .OR.
     &         (COMPO1(1:7).EQ.'KIT_DDI'. AND.
     &                    COMPO2(1:10).EQ.'GRANGER_FP') .OR.
     &          (COMPO1(1:7).EQ.'KIT_DDI'. AND.
     &            COMPO2(1:13).EQ.'BETON_UMLV_FP') )
     &         LFLU= . TRUE.
       ENDIF

      IF (.NOT.LFLU)  THEN
        VALK(1)=OPTION
        VALK(2) = COMPO1
         CALL U2MESK('A','ELEMENTS4_63',2,VALK)
        GOTO 999
      END IF


      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
C      -----------------------------------------
      NBSIG = NBSIGM()

C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
C    ------------------------------------------------------------------
      CALL JEVECH('PVARIGR','L',IVARI)
      CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
          NBVARI = MAX(JTAB(6),1)*JTAB(7)

      CALL R8INIR(MXCMEL,0.D0,EPSFL,1)

C     -----------------------------------------------------------
C      CALCUL DE L'OPTION EPFD
C     -----------------------------------------------------------

      IF (OPTION(1:4).EQ.'EPFD') THEN
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------

        DO 140 IGAU = 1,NPG

C POUR BETON_UMLV_FP LE FLUAGE DE DESSICCATION VAUT
C                    [V9 V10 V11 V18 V19 V20]

         CALL LCUMVI('FD',ZR(IVARI+(IGAU-1)*NBVARI),EPSTMP)

         DO 182 I=1,NBSIG
           EPSFL(NBSIG*(IGAU-1)+I)=EPSTMP(I)
  182    CONTINUE
  140   CONTINUE

C     --------------------------------------------------------
C      CALCUL DE L'OPTION EPFP
C     --------------------------------------------------------
      ELSE IF (OPTION(1:4).EQ.'EPFP')  THEN
C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)

C --- RECUPERATION DE L'INSTANT COURANT :
C     ---------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)

        DO 180 IGAU = 1,NPG

       IF  ((COMPO1(1:13).EQ.'BETON_UMLV_FP').OR.
     &         (COMPO1(1:7).EQ.'KIT_DDI'.AND.COMPO2(1:13).EQ.
     &          'BETON_UMLV_FP') ) THEN
C      POUR BETON_UMLV LE FLUAGE PROPRE VAUT
C        EPFP11 = (V1+V2) + V3 + V4
C        EPFP22 = (V1+V2) + V5 + V6
C        EPFP33 = (V1+V2) + V7 + V8
C        EPFP12 = V12+V13
C        EPFP13 = V14+V15
C        EPFP14 = V16+V17

         CALL LCUMVI('FP',ZR(IVARI+(IGAU-1)*NBVARI),EPSTMP)

         DO 185 I=1,NBSIG
           EPSFL(NBSIG*(IGAU-1)+I)=EPSTMP(I)
  185    CONTINUE


C-------------------------------------------------------------------*
       ELSE IF  ((COMPO1(1:10).EQ.'GRANGER_FP') .OR.
     &         (COMPO1(1:7).EQ.'KIT_DDI'. AND.
     &              COMPO2(1:10).EQ.'GRANGER_FP') )  THEN
         NOMRES='NU'
         NOMPAR='INST'
         VALPAR=ZR(ITEMPS)

         CALL RCVALB('RIGI',IGAU,1,'+',ZI(IMATE),' ','ELAS',1,NOMPAR,
     &          VALPAR, 1,NOMRES,NU,ICODRE,1)

         CALL CALCGR(IGAU,NBSIG,NBVARI,ZR(IVARI),NU,EPSTMP)
         DO 187 I=1,NBSIG
           EPSFL(NBSIG*(IGAU-1)+I)=EPSTMP(I)
  187    CONTINUE

        ENDIF
  180  CONTINUE

      ENDIF
C
C --- RECUPERATION DU VECTEUR EN SORTIE:
C     -------------------------------------------------------------
      CALL JEVECH('PDEFORR','E',IDEF)

C --- AFFECTATION DU VECTEUR EN SORTIE
C     ------------------------------------------------------------
      IF (OPTION(6:9).EQ.'ELGA') THEN

C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    POINTS D'INTEGRATION :
C        --------------------
        DO 160 IGAU = 1,NPG
          DO 150 ISIG = 1,NBSIG
            ZR(IDEF+NBSIG* (IGAU-1)+ISIG-1) = EPSFL(NBSIG* (IGAU-1)+
     &        ISIG)
  150     CONTINUE
  160   CONTINUE

      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN

C ---    DEFORMATIONS AUX NOEUDS :
C        -----------------------
        NBSP = 1
        CALL PPGAN2(JGANO,NBSP,NBSIG,EPSFL,EPSFLN)

C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    NOEUDS :
C        ------
        DO 580 INO = 1,NNO
          DO 570 ISIG = 1,NBSIG
            ZR(IDEF+NBSIG* (INO-1)+ISIG-1) = EPSFLN(NBSIG* (INO-1)+
     &        ISIG)
  570     CONTINUE
  580   CONTINUE

      END IF
  999  CONTINUE
      END
