      SUBROUTINE TE0333(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/09/2007   AUTEUR PELLET J.PELLET 
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

C     BUT: CALCUL DES DEFORMATIONS PLASTIQUES AUX NOEUDS ET PG
C          ELEMENTS ISOPARAMETRIQUES 3D

C     IN   OPTION : OPTIONS DE CALCUL
C                   'EPSP_ELNO'   'EPSP_ELGA'
C          NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
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

      INTEGER JGANO,MXCMEL,NBRES,NBSGM,I,NDIM,NNO,NBSIGM,NBSIG,
     &        IDSIG,NNOS,NPG,JVAL,IPOIDS,IVF,IDFDE,IGAU,
     &        ISIG,INO,IGEOM,IDEPL,ITEMPS,IMATE,IDEFA,
     &        IDEFP,ICOMPO,NBVARI,IVARI,K,
     &        NVI,NVIF,IBID,JTAB(7),IRET,IDIM
      PARAMETER (MXCMEL=162)
      PARAMETER (NBRES=3)
      PARAMETER (NBSGM=6)
      REAL*8 VALRES(NBRES)
      REAL*8 EPSM(MXCMEL),EPSANE(MXCMEL),EPSPLA(MXCMEL)
      REAL*8 EPSPLN(MXCMEL),SIGMA(NBSGM)
      REAL*8 VALPAR(2),C1,C2,TRSIG,XYZ(3)
      REAL*8 REPERE(7),NHARM,E,NU,ZERO,UN,TEMPG
      REAL*8 EPSFL(NBSGM),EPSFLF(NBSGM),TMPDMX,TMPFMX
      CHARACTER*2 CODRET(NBRES)
      CHARACTER*8 NOMRES(NBRES)
      CHARACTER*6       EPSA(6)
      CHARACTER*8 NOMPAR(2),MODELI,MOD3D
      CHARACTER*16 OPTIO2,PHENOM,CMP1,CMP2,CMP3
      LOGICAL LFLU,LTEMP
      DATA EPSA   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',
     &              'EPSAYZ'/
C DEB ------------------------------------------------------------------

C --- INITIALISATIONS :
C     ---------------
      ZERO = 0.0D0
      UN = 1.0D0
      NHARM = ZERO
      MODELI(1:2) = NOMTE(3:4)
      MOD3D = '3D'

      DO 10 I = 1,MXCMEL
        EPSANE(I) = ZERO
        EPSPLN(I) = ZERO
   10 CONTINUE
C --- CARACTERISTIQUES DU TYPE D'ELEMENT :
C --- GEOMETRIE ET INTEGRATION
C     ------------------------

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
C      -----------------------------------------
      NBSIG = NBSIGM()

C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)

C --- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
C     ------------------------------------------------------------
C     COORDONNEES DU BARYCENTRE ( POUR LE REPERE CYLINDRIQUE )
      XYZ(1) = 0.D0
      XYZ(2) = 0.D0
      XYZ(3) = 0.D0
      DO 190 I = 1,NNO
        DO 200 IDIM = 1,NDIM
          XYZ(IDIM) = XYZ(IDIM)+ZR(IGEOM+IDIM+NDIM*(I-1)-1)/NNO
 200    CONTINUE
 190  CONTINUE
      CALL ORTREP(ZI(IMATE),NDIM,XYZ,REPERE)

C --- RECUPERATION DE L'INSTANT COURANT :
C     ---------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)


C ---    RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS  :
C        ------------------------------------------------
      CALL JEVECH('PDEPLAR','L',IDEPL)

C ---    RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION :
C        -------------------------------------------------------------
      CALL JEVECH('PCONTRR','L',IDSIG)

C ---    RECUPERATION DES DEFORMATIONS ANELASTIQUES AUX NOEUDS
C ---    DE L'ELEMENT :

      DO 20 K=1,NBSIG
        DO 30 IGAU=1,NPG
          CALL RCVARC(' ',EPSA(K),'+','RIGI',IGAU,1,
     &                  EPSANE(NBSIG*(IGAU-1)+K),IRET)
          IF (IRET.EQ.1) EPSANE(NBSIG*(IGAU-1)+K)=0.D0
 30     CONTINUE
 20   CONTINUE

C ---    ON VERIFIE QUE LE MATERIAU EST ISOTROPE
C ---    (POUR L'INSTANT PAS D'ORTHOTROPIE NI D'ISOTROPIE TRANSVERSE
C ---    EN PLASTICITE) :
C        --------------
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS_ORTH' .OR. PHENOM.EQ.'ELAS_ISTR' .OR.
     &      PHENOM.EQ.'ELAS_ORTH_FO' .OR. PHENOM.EQ.'ELAS_ISTR_FO') THEN
        CALL U2MESK('F','ELEMENTS3_75',1,PHENOM(1:12))
      END IF

C ---    CALCUL DES DEFORMATIONS HORS THERMIQUES CORRESPONDANTES AU
C ---    CHAMP DE DEPLACEMENT I.E. EPSM = EPST - EPSTH - EPSRET
C ---    OU EPST  SONT LES DEFORMATIONS TOTALES
C ---       EPST = B.U
C ---    ET EPSTH SONT LES DEFORMATIONS THERMIQUES
C ---       EPSTH = ALPHA*(T-TREF) :
C ---    ET EPSRET SONT LES DEFORMATIONS LIEES AU RETRAIT
C ---       DE DESSICCATION ET  D HYDRATION
C ---       EPSRET = - B_ENDO * HYDR - K_DESSIC *(SREF-S)
C          ----------------------
      OPTIO2 = 'EPME_'//OPTION(6:9)//'_DEPL'
      CALL EPSVMC('RIGI',MODELI,NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     &            ZR(IGEOM),ZR(IDEPL),ZR(ITEMPS),
     &            ZI(IMATE),REPERE,NHARM,OPTIO2,EPSM)


C --- RECUPERATION DU COMPORTEMENT  :
C     -------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)

C --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
C    ------------------------------------------------------------------
      CALL JEVECH('PVARIGR','L',IVARI)
      CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
      NBVARI = MAX(JTAB(6),1)*JTAB(7)


C --- VERIFICATION DU COMPORTEMENT FLUAGE :
C     -------------------------------------
      CMP1 = ZK16(ICOMPO)
      CMP2 = ZK16(ICOMPO+7)
      CMP3 = ZK16(ICOMPO+8)
      IF (CMP1(1:10).NE.'GRANGER_FP' .AND.
     &    (CMP1(1:7).NE.'KIT_DDI'.OR.CMP2(1:10).NE.'GRANGER_FP')) THEN
        LFLU = .FALSE.
        DO 60 I = 1,MXCMEL
          EPSPLA(I) = ZERO
   60   CONTINUE
        DO 70 I = 1,NBSIG
          EPSFLF(I) = ZERO
   70   CONTINUE
      ELSE
        CALL GRANVI(MOD3D,IBID,IBID,NVIF)
        LFLU = .TRUE.
      END IF

C --- DEPENDANCE DES CARACTERISTIQUES MECANIQUES AVEC LA TEMPERATURE :
C     ----------------------------------------------------------------
      LTEMP = .FALSE.
      IF (CMP1(1:15).EQ.'BETON_DOUBLE_DP') THEN
        NVI = 3
        LTEMP = .TRUE.
      ELSE IF (CMP1(1:7).EQ.'KIT_DDI') THEN
        IF (CMP3(1:15).EQ.'BETON_DOUBLE_DP') THEN
          IF (CMP2(1:10).EQ.'GRANGER_FP') THEN
            NVI = NVIF + 3
            LTEMP = .TRUE.
          ELSE
            CALL U2MESS('F','ELEMENTS3_76')
          END IF
        END IF
      END IF

C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------

      DO 140 IGAU = 1,NPG

C  ---   TEMPERATURE AU POINT D'INTEGRATION COURANT :
C        ------------------------------------------
        CALL RCVARC('F','TEMP','+','RIGI',IGAU,1,TEMPG,IRET)

        IF (LTEMP) THEN
          IF (TEMPG.LT.ZR(IVARI+ (IGAU-1)*NBVARI+NVI-
     &        1)) TEMPG = ZR(IVARI+ (IGAU-1)*NBVARI+NVI-1)
        END IF

C ---    RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
C        ---------------------------------------------
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'

        NOMPAR(1) = 'TEMP'
        NOMPAR(2) = 'INST'
        VALPAR(1) = TEMPG
        VALPAR(2) = ZR(ITEMPS)

        CALL RCVALB('RIGI',IGAU,1,'+',ZI(IMATE),' ','ELAS',2,NOMPAR,
     &              VALPAR,2,NOMRES,VALRES, CODRET,'FM')

        CALL RCVALB('RIGI',IGAU,1,'+',ZI(IMATE),' ','ELAS',2,NOMPAR,
     &              VALPAR,1,NOMRES(3),VALRES(3),CODRET(3),'  ')

        E = VALRES(1)
        NU = VALRES(2)
        IF (CODRET(3).NE.'OK') THEN
          VALRES(3) = ZERO
        END IF

C ---    TENSEUR DE DEFORMATION DE FLUAGE AU PT D'INTEGRATION COURANT :
C        --------------------------------------------------------------
        IF (LFLU) THEN

         CALL CALCGR(IGAU,NBSIG,NBVARI,ZR(IVARI),NU,EPSFLF)

        END IF

C ----      TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
C           ------------------------------------------------------
        DO 120 I = 1,NBSIG
          SIGMA(I) = ZR(IDSIG+ (IGAU-1)*NBSIG+I-1)
  120   CONTINUE

        TRSIG = SIGMA(1) + SIGMA(2) + SIGMA(3)

        C1 = (UN+NU)/E
        C2 = NU/E

C ---       TENSEUR DES DEFORMATIONS PLASTIQUES AU POINT
C ---       D'INTEGRATION COURANT
C ---       I.E. EPSPLA = EPS_TOT - EPS_THERM - EPS_ELAS - EPS_ANELAS :
C ---                             - EPS_FLUAGE :
C           ---------------------------------------------------------
        EPSPLA(NBSIG* (IGAU-1)+1) = EPSM(NBSIG* (IGAU-1)+1) -
     &                                EPSANE(NBSIG* (IGAU-1)+1) -
     &                                (C1*SIGMA(1)-C2*TRSIG) - EPSFLF(1)
        EPSPLA(NBSIG* (IGAU-1)+2) = EPSM(NBSIG* (IGAU-1)+2) -
     &                                EPSANE(NBSIG* (IGAU-1)+2) -
     &                                (C1*SIGMA(2)-C2*TRSIG) - EPSFLF(2)
        EPSPLA(NBSIG* (IGAU-1)+3) = EPSM(NBSIG* (IGAU-1)+3) -
     &                                EPSANE(NBSIG* (IGAU-1)+3) -
     &                                (C1*SIGMA(3)-C2*TRSIG) - EPSFLF(3)
        EPSPLA(NBSIG* (IGAU-1)+4) = EPSM(NBSIG* (IGAU-1)+4) -
     &                                EPSANE(NBSIG* (IGAU-1)+4) -
     &                                C1*SIGMA(4) - EPSFLF(4)
        EPSPLA(NBSIG* (IGAU-1)+5) = EPSM(NBSIG* (IGAU-1)+5) -
     &                                EPSANE(NBSIG* (IGAU-1)+5) -
     &                                C1*SIGMA(5) - EPSFLF(5)
        EPSPLA(NBSIG* (IGAU-1)+6) = EPSM(NBSIG* (IGAU-1)+6) -
     &                                EPSANE(NBSIG* (IGAU-1)+6) -
     &                                C1*SIGMA(6) - EPSFLF(6)

  140 CONTINUE

C --- RECUPERATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
C     -------------------------------------------------------------
      CALL JEVECH('PDEFOPL','E',IDEFP)

C --- AFFECTATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
C     ------------------------------------------------------------
      IF (OPTION(6:9).EQ.'ELGA') THEN

C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    POINTS D'INTEGRATION :
C        --------------------
        DO 160 IGAU = 1,NPG
          DO 150 ISIG = 1,NBSIG
            ZR(IDEFP+NBSIG* (IGAU-1)+ISIG-1) = EPSPLA(NBSIG* (IGAU-1)+
     &        ISIG)
  150     CONTINUE
  160   CONTINUE

      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN

C ---    DEFORMATIONS AUX NOEUDS :
C        -----------------------
        CALL PPGAN2(JGANO,NBSIG,EPSPLA,EPSPLN)

C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    NOEUDS :
C        ------
        DO 180 INO = 1,NNO
          DO 170 ISIG = 1,NBSIG
            ZR(IDEFP+NBSIG* (INO-1)+ISIG-1) = EPSPLN(NBSIG* (INO-1)+
     &        ISIG)
  170     CONTINUE
  180   CONTINUE

      END IF

      END
