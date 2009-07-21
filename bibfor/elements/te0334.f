      SUBROUTINE TE0334(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/07/2009   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     BUT: CALCUL DES DEFORMATIONS PLASTIQUES AUX NOEUDS ET PG ET DES
C          DEFORMATIONS DE FLUAGE DE GRANGER
C          ELEMENTS ISOPARAMETRIQUES 2D

C     IN   OPTION : OPTIONS DE CALCUL
C                   'EPSP_ELNO'   'EPSP_ELGA'
C                   'EPGR_ELNO'   'EPGR_ELGA'
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

      INTEGER MXCMEL,NBRES,NBSGM,I,NDIM,NNO,NNOS,NBSIGM,NBSIG,
     &        IDSIG,ICOMPO,NPG,IPOIDS,IVF,IDFDE,
     &        IGAU,ISIG,INO,IGEOM,IDEPL,IDEFP,
     &        ITEMPS,IMATE,IDEFA,
     &        NBVARI,IVARI,K,NVIF,IBID,JTAB(7),IRET,JGANO
      PARAMETER (MXCMEL=54)
      PARAMETER (NBRES=3)
      PARAMETER (NBSGM=4)
      REAL*8 VALRES(NBRES),EPSM(MXCMEL),EPSPLA(MXCMEL)
      REAL*8 EPSPLN(MXCMEL),SIGMA(NBSGM), VALPAR,C1,C2,TRSIG
      REAL*8 EPSFL(NBSGM),EPSFLF(NBSGM)
      REAL*8 REPERE(7),NHARM,E,NU,ZERO,UN
      CHARACTER*2  CODRET(NBRES)
      CHARACTER*8  NOMRES(NBRES),NOMPAR,MOD3D
      CHARACTER*16 OPTIO2,PHENOM,CMP1,CMP2
      CHARACTER*16 COMPOR
      LOGICAL      LFLU, LTEATT
C DEB ------------------------------------------------------------------

C --- CARACTERISTIQUES DU TYPE D'ELEMENT :
C --- GEOMETRIE ET INTEGRATION
C     ------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C --- INITIALISATIONS :
C     ---------------
      ZERO = 0.0D0
      UN = 1.0D0
      NHARM = ZERO
      MOD3D = '3D'

      DO 10 I = 1,MXCMEL
        EPSPLN(I) = ZERO
   10 CONTINUE
C --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
C     -----------------------------------------
      NBSIG = NBSIGM()

C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)

C --- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
C     ------------------------------------------------------------
C     ON FOURNIT UN REPRE BIDON A EPSVMC CAR DE TOUTE FACON ON NE
C     TRAITE PAS LE CAS ORTHOTROPE
C
      DO 200 I=1,7
        REPERE(I)=0.D0
200   CONTINUE

C --- RECUPERATION DE L'INSTANT COURANT :
C     ---------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)


C ---    RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS  :
C        ------------------------------------------------
      CALL JEVECH('PDEPLAR','L',IDEPL)

C ---    RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION :
C        -------------------------------------------------------------
      CALL JEVECH('PCONTRR','L',IDSIG)

C ---    ON VERIFIE QUE LE MATERIAU EST ISOTROPE
C ---    (POUR L'INSTANT PAS D'ORTHOTROPIE NI D'ISOTROPIE TRANSVERSE
C ---    EN PLASTICITE) :
C        --------------
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)

      IF (PHENOM.EQ.'ELAS_ORTH' .OR. PHENOM.EQ.'ELAS_ISTR' .OR.
     &      PHENOM.EQ.'ELAS_ORTH_FO' .OR. PHENOM.EQ.'ELAS_ISTR_FO') THEN
        CALL U2MESK('F','ELEMENTS3_75',1,PHENOM(1:12))
      END IF

C ---    RECUPERATION DU COMPORTEMENT DANS LE CAS DES CONTRAINTES
C ---    PLANES :
C        ---------------------------------------------------------
      IF (LTEATT(' ','C_PLAN','OUI')) THEN
        CALL TECACH('ONN','PCOMPOR',1,ICOMPO,IRET)
        IF (ICOMPO.NE.0) THEN
          COMPOR = ZK16(ICOMPO)
          IF (COMPOR.NE.'VMIS_ISOT_LINE'.AND.COMPOR(1:4).NE.'ELAS'
     &         .AND.COMPOR.NE.'VMIS_ISOT_TRAC') THEN
            CALL U2MESK('A','ELEMENTS3_77',1,COMPOR)
          END IF
        END IF
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
      CALL EPSVMC('RIGI',NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     &              ZR(IGEOM),ZR(IDEPL),
     &              ZR(ITEMPS),ZI(IMATE),REPERE,NHARM,OPTIO2,EPSM)

C --- RECUPERATION DU COMPORTEMENT  :
C     -------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)

C --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
C     -----------------------------------------------------------------
      CALL JEVECH('PVARIGR','L',IVARI)
      CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
      NBVARI = MAX(JTAB(6),1)*JTAB(7)

C --- VERIFICATION DU COMPORTEMENT FLUAGE :
C     -------------------------------------
      CMP1 = ZK16(ICOMPO)
      CMP2 = ZK16(ICOMPO+7)
      IF (CMP1(1:10).NE.'GRANGER_FP' .AND.
     &    (CMP1(1:7).NE.'KIT_DDI'.OR.CMP2(1:10).NE.'GRANGER_FP')) THEN
        LFLU = .FALSE.
        DO 40 I = 1,MXCMEL
          EPSPLA(I) = ZERO
   40   CONTINUE
        DO 50 I = 1,NBSIG
          EPSFLF(I) = ZERO
   50   CONTINUE
      ELSE
        CALL GRANVI(MOD3D,IBID,IBID,NVIF)
        LFLU = .TRUE.
      END IF


C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
      DO 120 IGAU = 1,NPG



C ---    RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
C        ---------------------------------------------
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'

        NOMPAR = 'INST'
        VALPAR = ZR(ITEMPS)

        CALL RCVALB('RIGI',IGAU,1,'+',ZI(IMATE),' ','ELAS',1,NOMPAR,
     &              VALPAR,2,NOMRES,VALRES,CODRET,'FM')

        CALL RCVALB('RIGI',IGAU,1,'+',ZI(IMATE),' ','ELAS',1,NOMPAR,
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

C ---       TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
C           ------------------------------------------------------
        DO 100 I = 1,NBSIG
          SIGMA(I) = ZR(IDSIG+ (IGAU-1)*NBSIG+I-1)
  100   CONTINUE

        IF (LTEATT(' ','C_PLAN','OUI')) THEN
          TRSIG = SIGMA(1) + SIGMA(2)
        ELSE
          TRSIG = SIGMA(1) + SIGMA(2) + SIGMA(3)
        END IF

        C1 = (UN+NU)/E
        C2 = NU/E

C ---       TENSEUR DES DEFORMATIONS PLASTIQUES AU POINT
C ---       D'INTEGRATION COURANT
C ---       I.E. EPSPLA = EPS_TOT - EPS_THERM - EPS_ELAS - EPS_ANELAS :
C ---                             - EPS_FLUAGE :
C           ---------------------------------------------------------
        EPSPLA(NBSIG* (IGAU-1)+1) = EPSM(NBSIG* (IGAU-1)+1) 
     &                              - (C1*SIGMA(1)-C2*TRSIG) - EPSFLF(1)
        EPSPLA(NBSIG* (IGAU-1)+2) = EPSM(NBSIG* (IGAU-1)+2) 
     &                              - (C1*SIGMA(2)-C2*TRSIG) - EPSFLF(2)
        IF (LTEATT(' ','C_PLAN','OUI')) THEN
          EPSPLA(NBSIG* (IGAU-1)+3) = - (EPSPLA(NBSIG* (IGAU-1)+1)+
     &                                  EPSPLA(NBSIG* (IGAU-1)+2))
        ELSE
          EPSPLA(NBSIG* (IGAU-1)+3) = EPSM(NBSIG* (IGAU-1)+3) -
     &                                  (C1*SIGMA(3)-C2*TRSIG) -
     &                                  EPSFLF(3)
        END IF
        EPSPLA(NBSIG* (IGAU-1)+4) = EPSM(NBSIG* (IGAU-1)+4) -
     &                                C1*SIGMA(4) - EPSFLF(4)

  120 CONTINUE


C --- RECUPERATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
C     -------------------------------------------------------------
      CALL JEVECH('PDEFOPL','E',IDEFP)

C --- AFFECTATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
C     ------------------------------------------------------------
      IF (OPTION(6:9).EQ.'ELGA') THEN

C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    POINTS D'INTEGRATION :
C        --------------------
        DO 140 IGAU = 1,NPG
          DO 130 ISIG = 1,NBSIG
            ZR(IDEFP+NBSIG* (IGAU-1)+ISIG-1) = EPSPLA(NBSIG* (IGAU-1)+
     &        ISIG)
  130     CONTINUE
  140   CONTINUE

      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN

C ---    DEFORMATIONS AUX NOEUDS :
C        -----------------------
         CALL PPGAN2 ( JGANO, NBSIG, EPSPLA, EPSPLN )

C ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
C ---    NOEUDS :
C        ------
        DO 160 INO = 1,NNO
          DO 150 ISIG = 1,NBSIG
            ZR(IDEFP+NBSIG* (INO-1)+ISIG-1) = EPSPLN(NBSIG* (INO-1)+
     &        ISIG)
  150     CONTINUE
  160   CONTINUE

      END IF

      END
