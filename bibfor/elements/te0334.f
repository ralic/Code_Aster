      SUBROUTINE TE0334(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C          DEFORMATIONS DE FLUAGE
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

      INTEGER MXCMEL,NBRES,NBSGM,MXCMPG,I,NDIM,NNO,NNOS,NBSIGM,NBSIG,
     &        IDSIG,IFF,ICOMPO,NPG,IPOIDS,IVF,IDFDE,ICARAC,NPG1,NPG2,
     &        NPG3,NPG4,IDFDK,IGAU,ISIG,INO,IGEOM,IDEPL,IDEFP,ITEMPE,
     &        ITREF,ITEMPS,IMATE,IDEFA,IDECPG,IHYDRE,ISECHE,NBVARI,
     &        IVARI,K,NVI,NVIF,IBID,JTAB(7)
      PARAMETER (MXCMEL=54)
      PARAMETER (NBRES=3)
      PARAMETER (NBSGM=4)
      PARAMETER (MXCMPG=9)
      REAL*8 VALRES(NBRES)
      REAL*8 EPSM(MXCMEL),EPSANE(MXCMEL),EPSPLA(MXCMEL)
      REAL*8 EPSPLN(MXCMEL),SIGMA(NBSGM)
      REAL*8 VALPAR(4),C1,C2,TRSIG,HYDRG,SECHG,BIDON
      REAL*8 HYDR(MXCMPG),SECH(MXCMPG)
      REAL*8 EPSFL(NBSGM),EPSFLF(NBSGM),TMPDMX,TMPFMX
      REAL*8 REPERE(7),NHARM,E,NU,ZERO,UN,TEMPG
      CHARACTER*2 CODRET(NBRES)
      CHARACTER*8 NOMRES(NBRES)
      CHARACTER*8 NOMPAR(4),MODELI,MOD3D
      CHARACTER*8 ELREFE
      CHARACTER*16 OPTIO2,PHENOM,CMP1,CMP2,CMP3
      CHARACTER*16 COMPOR
      CHARACTER*24 CARAC,FF
      LOGICAL LFLU,LPLAS,LTEMP
C DEB ------------------------------------------------------------------

C --- INITIALISATIONS :
C     ---------------
      CALL ELREF1(ELREFE)
      ZERO = 0.0D0
      UN = 1.0D0
      NHARM = ZERO
      MODELI(1:2) = NOMTE(3:4)
      NDIM = 2
      BIDON = ZERO
      MOD3D = '3D'

      DO 10 I = 1,MXCMEL
        EPSANE(I) = ZERO
        EPSPLN(I) = ZERO
   10 CONTINUE

C --- CARACTERISTIQUES DU TYPE D'ELEMENT :
C --- GEOMETRIE ET INTEGRATION
C     ------------------------
C
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C      NPG2 = ZI(ICARAC+3)
C      NPG3 = ZI(ICARAC+4)
C      NPG4 = ZI(ICARAC+5)

      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IF (OPTION(6:9).EQ.'ELNO') THEN
CJMP CORRECTION AL 2000-240
C         IF(NOMTE(5:8).EQ.'TR3 ' .OR. NOMTE(5:8).EQ.'QU4 ') THEN
C             NNOS   = NNO
C             IPOIDS = IFF   + NPG1*(1+3*NNO)
C             IVF    = IPOIDS+ NPG2
C             IDFDE  = IVF   + NPG2*NNO
C             IDFDK  = IDFDE + NPG2*NNO
C             NPG    = NPG2
C         ELSE
C             NNOS   = NNO/2
C             IPOIDS = IFF   + (NPG1+NPG2+NPG3)*(1+3*NNO)
C             IVF    = IPOIDS+ NPG4
C             IDFDE  = IVF   + NPG4*NNO
C             IDFDK  = IDFDE + NPG4*NNO
C             NPG    = NPG4
C         ENDIF
        NPG = NPG1
        IPOIDS = IFF
        IVF = IPOIDS + NPG1
        IDFDE = IVF + NPG1*NNO
        IDFDK = IDFDE + NPG1*NNO
        IF (NOMTE(5:8).EQ.'TR3 ') THEN
          NNOS = NNO
        ELSE IF (NOMTE(5:8).EQ.'QU4 ') THEN
          NNOS = NNO
        ELSE IF (NOMTE(5:8).EQ.'TR6 ') THEN
          NNOS = 3
        ELSE IF (NOMTE(5:8).EQ.'QS8 ') THEN
          NNOS = 4
        ELSE IF (NOMTE(5:8).EQ.'QU8 ' .OR. NOMTE(5:8).EQ.'QU9 ') THEN
          NNOS = 4
        END IF
      ELSE
        IPOIDS = IFF
        IVF = IPOIDS + NPG1
        IDFDE = IVF + NPG1*NNO
        IDFDK = IDFDE + NPG1*NNO
        NPG = NPG1
      END IF

C --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
C     -----------------------------------------
      NBSIG = NBSIGM(MODELI)

C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
C     ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)

C --- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
C     ------------------------------------------------------------
      CALL ORTREP(ZI(IMATE),NDIM,REPERE)

C --- RECUPERATION DES TEMPERATURES AUX NOEUDS DE L'ELEMENT :
C     -----------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMPE)

C --- RECUPERATION DE LA TEMPERATURE DE REFERENCE :
C     -------------------------------------------
      CALL JEVECH('PTEREF','L',ITREF)

C --- RECUPERATION DE L'HYDRATATION AUX POINTS DE GAUSS DE L'ELEMENT :
C     --------------------------------------------------------------
      CALL JEVECH('PHYDRER','L',IHYDRE)
      DO 20 I = 1,NPG
        HYDR(I) = ZR(IHYDRE+I-1)
   20 CONTINUE

C --- RECUPERATION DU SECHAGE AUX NOEUDS DE L'ELEMENT :
C     --------------------------------------------------
      CALL JEVECH('PSECHER','L',ISECHE)
      DO 30 I = 1,NNO
        SECH(I) = ZR(ISECHE+I-1)
   30 CONTINUE

C --- RECUPERATION DE L'INSTANT COURANT :
C     ---------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)

      IF (OPTION(1:4).EQ.'EPSP') THEN
        LPLAS = .TRUE.

C ---    RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS  :
C        ------------------------------------------------
        CALL JEVECH('PDEPLAR','L',IDEPL)

C ---    RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION :
C        -------------------------------------------------------------
        CALL JEVECH('PCONTRR','L',IDSIG)

C ---    RECUPERATION DES DEFORMATIONS ANELASTIQUES AUX NOEUDS
C ---    DE L'ELEMENT :
C        ------------
        CALL TECACH(.TRUE.,.FALSE.,'PDEFAPR',1,IDEFA)

C ---    ON VERIFIE QUE LE MATERIAU EST ISOTROPE
C ---    (POUR L'INSTANT PAS D'ORTHOTROPIE NI D'ISOTROPIE TRANSVERSE
C ---    EN PLASTICITE) :
C        --------------
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)

        IF (PHENOM.EQ.'ELAS_ORTH' .OR. PHENOM.EQ.'ELAS_ISTR' .OR.
     &      PHENOM.EQ.'ELAS_ORTH_FO' .OR. PHENOM.EQ.'ELAS_ISTR_FO') THEN
          CALL UTMESS('F','TE0334','LE MATERIAU '//PHENOM(1:12)//
     &                ' N''EST PAS AUTORISE POUR CALCULER LES '//
     &                'DEFORMATIONS PLASTIQUES : SEULS LES '//
     &                'MATERIAUX ISOTROPES SONT TRAITES EN PLASTICITE.')
        END IF

C ---    RECUPERATION DU COMPORTEMENT DANS LE CAS DES CONTRAINTES
C ---    PLANES :
C        ---------------------------------------------------------
        IF (MODELI(1:2).EQ.'CP') THEN
          CALL TECACH(.TRUE.,.FALSE.,'PCOMPOR',1,ICOMPO)
          IF (ICOMPO.NE.0) THEN
            COMPOR = ZK16(ICOMPO)
            IF (COMPOR.NE.'VMIS_ISOT_LINE' .AND.
     &          COMPOR.NE.'VMIS_ISOT_TRAC') THEN
              CALL UTMESS('A','TE0334','ATTENTION VOUS ETES EN '//
     &                    'CONTRAINTES PLANES, ET VOUS UTILISEZ LA LOI'
     &                    //' DE COMPORTEMENT '//COMPOR//'. LA '//
     &                    ' COMPOSANTE DU TENSEUR DES DEFORMATIONS '//
     &                    'PLASTIQUES  EPZZ EST CALCULEE AVEC '//
     &                    'EPZZ = -(EPXX + EPYY) . VERIFIEZ QUE CETTE '
     &                    //'EXPRESSION EST VRAIE AVEC VOTRE LOI DE '//
     &                    'COMPORTEMENT.')
            END IF
          END IF
        END IF

C ---    CALCUL DES DEFORMATIONS HORS THERMIQUES CORRESPONDANTES AU
C ---    CHAMP DE DEPLACEMENT I.E. EPSM = EPST - EPSTH
C ---    OU EPST  SONT LES DEFORMATIONS TOTALES
C ---       EPST = B.U
C ---    ET EPSTH SONT LES DEFORMATIONS THERMIQUES
C ---       EPSTH = ALPHA*(T-TREF) :
C          ----------------------

        OPTIO2 = 'EPME_'//OPTION(6:9)//'_DEPL'
        CALL EPSVMC(MODELI,NNO,NDIM,NBSIG,NPG,ZR(IVF),ZR(IDFDE),
     &              ZR(IDFDK),BIDON,ZR(IPOIDS),ZR(IGEOM),ZR(IDEPL),
     &              ZR(ITEMPE),ZR(ITREF),HYDR,SECH,ZR(ITEMPS),ZI(IMATE),
     &              REPERE,NHARM,OPTIO2,EPSM)

C ---    AFFECTATION DU VECTEUR DES DEFORMATIONS ANELASTIQUES AUX
C ---    POINTS D'INTEGRATION DE L'ELEMENT :
C        --------------------------------
        IF (IDEFA.NE.0) THEN
          CALL EPSAMC(NNO,NPG,NBSIG,ZR(IVF),ZR(IDEFA),EPSANE)
        END IF
      ELSE
        LPLAS = .FALSE.
      END IF

C --- RECUPERATION DU COMPORTEMENT  :
C     -------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)

C --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
C     -----------------------------------------------------------------
      CALL JEVECH('PVARIGR','L',IVARI)
      CALL TECACH(.TRUE.,.TRUE.,'PVARIGR',7,JTAB)
      NBVARI = MAX(JTAB(6),1)*JTAB(7)

C --- VERIFICATION DU COMPORTEMENT FLUAGE :
C     -------------------------------------
      CMP1 = ZK16(ICOMPO)
      CMP2 = ZK16(ICOMPO+7)
      CMP3 = ZK16(ICOMPO+8)
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
            CALL UTMESS('F','TE0334',
     &           'COUPLAGE FLUAGE/FISSURATION : LA LOI BETON_DOUBLE_DP '
     &                  //
     &              'NE PEUT ETRE COUPLEE QU AVEC UNE LOI DE FLUAGE DE '
     &                  //'GRANGER.')
          END IF
        END IF
      END IF

C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
      DO 120 IGAU = 1,NPG

        IDECPG = NNO* (IGAU-1) - 1

C  ---   TEMPERATURE AU POINT D'INTEGRATION COURANT :
C        ------------------------------------------
        TEMPG = ZERO
        SECHG = ZERO
        HYDRG = ZERO
        HYDRG = HYDR(IGAU)

        DO 60 I = 1,NNO
          TEMPG = TEMPG + ZR(IVF+I+IDECPG)*ZR(ITEMPE+I-1)
          SECHG = SECHG + ZR(IVF+I+IDECPG)*SECH(I)
   60   CONTINUE
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
        NOMPAR(2) = 'HYDR'
        NOMPAR(3) = 'SECH'
        NOMPAR(4) = 'INST'
        VALPAR(1) = TEMPG
        VALPAR(2) = HYDRG
        VALPAR(3) = SECHG
        VALPAR(4) = ZR(ITEMPS)

        CALL RCVALA(ZI(IMATE),'ELAS',4,NOMPAR,VALPAR,2,NOMRES,VALRES,
     &              CODRET,'FM')

        CALL RCVALA(ZI(IMATE),'ELAS',4,NOMPAR,VALPAR,2,NOMRES(3),
     &              VALRES(3),CODRET(3),'  ')

        E = VALRES(1)
        NU = VALRES(2)
        IF (CODRET(3).NE.'OK') THEN
          VALRES(3) = ZERO
        END IF

C ---    TENSEUR DE DEFORMATION DE FLUAGE AU PT D'INTEGRATION COURANT :
C        --------------------------------------------------------------
        IF (LFLU) THEN
          DO 80 K = 1,NBSIG
            EPSFL(K) = ZR(IVARI+ (IGAU-1)*NBVARI+8*NBSIG+K-1)
            DO 70 I = 1,8
              EPSFL(K) = EPSFL(K) - ZR(IVARI+ (IGAU-1)*NBVARI+
     &                   (I-1)*NBSIG+K-1)
   70       CONTINUE
   80     CONTINUE
          C1 = (UN+NU)
          C2 = -NU
          EPSFLF(1) = EPSFL(1) + C2*EPSFL(2) + C2*EPSFL(3)
          EPSFLF(2) = C2*EPSFL(1) + EPSFL(2) + C2*EPSFL(3)
          EPSFLF(3) = C2*EPSFL(1) + C2*EPSFL(2) + EPSFL(3)
          DO 90 I = 4,NBSIG
            EPSFLF(I) = C1*EPSFL(I)
   90     CONTINUE
        END IF

        IF (LPLAS) THEN

C ---       TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
C           ------------------------------------------------------
          DO 100 I = 1,NBSIG
            SIGMA(I) = ZR(IDSIG+ (IGAU-1)*NBSIG+I-1)
  100     CONTINUE

          IF (NOMTE(3:4).EQ.'CP') THEN
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
          EPSPLA(NBSIG* (IGAU-1)+1) = EPSM(NBSIG* (IGAU-1)+1) -
     &                                EPSANE(NBSIG* (IGAU-1)+1) -
     &                                (C1*SIGMA(1)-C2*TRSIG) - EPSFLF(1)
          EPSPLA(NBSIG* (IGAU-1)+2) = EPSM(NBSIG* (IGAU-1)+2) -
     &                                EPSANE(NBSIG* (IGAU-1)+2) -
     &                                (C1*SIGMA(2)-C2*TRSIG) - EPSFLF(2)
          IF (NOMTE(3:4).EQ.'CP') THEN
            EPSPLA(NBSIG* (IGAU-1)+3) = - (EPSPLA(NBSIG* (IGAU-1)+1)+
     &                                  EPSPLA(NBSIG* (IGAU-1)+2))
          ELSE
            EPSPLA(NBSIG* (IGAU-1)+3) = EPSM(NBSIG* (IGAU-1)+3) -
     &                                  EPSANE(NBSIG* (IGAU-1)+3) -
     &                                  (C1*SIGMA(3)-C2*TRSIG) -
     &                                  EPSFLF(3)
          END IF
          EPSPLA(NBSIG* (IGAU-1)+4) = EPSM(NBSIG* (IGAU-1)+4) -
     &                                EPSANE(NBSIG* (IGAU-1)+4) -
     &                                C1*SIGMA(4) - EPSFLF(4)
        ELSE


C ---       TENSEUR DES DEFORMATIONS DE FLUAGE AU POINT
C ---       D'INTEGRATION COURANT
C           ----------------------------------------------
          DO 110 I = 1,NBSIG
            EPSPLA(NBSIG* (IGAU-1)+I) = EPSFLF(I)
  110     CONTINUE

        END IF
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
        CALL PPGANO(NNOS,NPG,NBSIG,EPSPLA,EPSPLN)

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
