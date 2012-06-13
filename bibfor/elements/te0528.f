      SUBROUTINE TE0528(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C     BUT: CALCUL DES DEFORMATIONS DUES :
C         AU FLUAGE DE DESSICCATION
C          POUR LE MODELE BETON_UMLV_FP
C         AU FLUAGE PROPRE POUR LES MODELES BETON_UMLV_FP ET GRANGER
C          AUX NOEUDS ET PG
C          ELEMENTS ISOPARAMETRIQUES 3D/D_PLAN/AXIS


C     IN   OPTION : OPTIONS DE CALCUL
C                   'EPFD_ELGA'
C                   'EPFP_ELGA'
C          NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------

      INTEGER JGANO,MXCMEL,NBSGM,I,NDIM,NNO,NBSIGM,NBSIG,
     &        NNOS,NPG,IPOIDS,IVF,IDFDE,IGAU,ISIG,IGEOM,
     &        IDEF,ICOMPO,NBVARI,IVARI,
     &        JTAB(7),IRET,IMATE,ITEMPS
      PARAMETER (MXCMEL=162)
      PARAMETER (NBSGM=6)
      REAL*8 EPSFL(MXCMEL),EPSTMP(NBSGM)
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
     &       (COMPO1(1:15).EQ.'BETON_BURGER_FP').OR.
     &        (COMPO1(1:7).EQ.'KIT_DDI'.AND.COMPO2(1:13).EQ.
     &        'BETON_UMLV_FP'))  THEN
            LFLU=.TRUE.
         ENDIF
        ELSEIF (OPTION(1:4).EQ.'EPFP') THEN
          IF ( (COMPO1(1:13).EQ.'BETON_UMLV_FP') .OR.
     &         (COMPO1(1:15).EQ.'BETON_BURGER_FP').OR.
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

          IF  ((COMPO1(1:13).EQ.'BETON_UMLV_FP').OR.
     &         (COMPO1(1:7).EQ.'KIT_DDI'.AND.COMPO2(1:13).EQ.
     &          'BETON_UMLV_FP') ) THEN
            CALL LCUMVI('FD',ZR(IVARI+(IGAU-1)*NBVARI),EPSTMP)

          ELSEIF(COMPO1(1:15).EQ.'BETON_BURGER_FP')THEN
            CALL BURFTM('FD',ZR(IVARI+(IGAU-1)*NBVARI),EPSTMP)

          ENDIF

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

       ELSE IF  (COMPO1(1:15).EQ.'BETON_BURGER_FP') THEN
C      POUR BETON_BURGER LE FLUAGE PROPRE VAUT
C        EPFP11 = (V1+V2) + V3 + V4
C        EPFP22 = (V1+V2) + V5 + V6
C        EPFP33 = (V1+V2) + V7 + V8
C        EPFP12 = V12+V13
C        EPFP13 = V14+V15
C        EPFP14 = V16+V17

         CALL BURFTM('FP',ZR(IVARI+(IGAU-1)*NBVARI),EPSTMP)

         DO 190 I=1,NBSIG
           EPSFL(NBSIG*(IGAU-1)+I)=EPSTMP(I)
  190    CONTINUE

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
      CALL JEVECH('PDEFOPG','E',IDEF)

C --- AFFECTATION DU VECTEUR EN SORTIE
C     ------------------------------------------------------------
C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    POINTS D'INTEGRATION :
C        --------------------
      DO 160 IGAU = 1,NPG
        DO 150 ISIG = 1,NBSIG
          ZR(IDEF+NBSIG* (IGAU-1)+ISIG-1) = EPSFL(NBSIG* (IGAU-1)+
     &      ISIG)
  150   CONTINUE
  160 CONTINUE

  999 CONTINUE
      END
