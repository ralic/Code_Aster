      SUBROUTINE TE0512 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
C
C     BUT: -- CALCUL
C               - DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C               - DE LA CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT (SIGMA*)
C               - DE L'ENDOMMAGEMENT DE LEMAITRE-SERMAGE (DOMLE)
C
C        TAUX DE TRIAXIALITE : TRIAX
C        ---------------------------
C           TRIAX    = SIGMA_H / SIGMA_EQ
C
C    OU     SIG      = SIGMA - 1/3 TRACE(SIGMA) I
C           SIGMA_EQ = ( 3/2 SIG : SIG ) ** 1/2
C           SIGMA_H  = 1/3 TRACE(SIGMA)
C           SIGMA    = TENSEUR DES CONTRAINTES
C           S        = DEVIATEUR DE SIGMA
C           I        = TENSEUR IDENTITE
C
C        CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT : SIGMA*
C        -----------------------------------------------
C           SI_ENDO  = SIGMA_EQ (2/3(1+NU) + 3(1-2 NU) TRIAX**2 )**1/2
C
C     OU    NU       = COEFFICIENT DE POISSON
C
C        LOI D'ENDOMMAGEMENT DE LEMAITRE-SERMAGE: DOMLE
C        ----------------------------------------------
C           D° = PUISSANCE([Y/VAR_S];EXP_S)*p°
C
C        LOI D'ENDOMMAGEMENT DE LEMAITRE-SERMAGE INTEGREE: DOMLE
C        -------------------------------------------------------
C           DOMLE(+) = 1 - PUISSANCE([PUISSANCE(1-DOMLE(-);2*EXP_S+1)
C                    - (2*EXP_S+1/2)
C                    *(PUISSANCE(K(+);EXP_S)+PUISSANCE(K(-);EXP_S))
C                    *(p(+) - p(-))];1/2*EXP_S+1)
C     OU
C           K(+) = PUISSANCE(SIGMA*(+);2) / (E(+)*VAR_S(+))
C
C.......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      LOGICAL            ELREFA
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER  NBSIGM,NBNOEU,NBNOSO,NBPGAU,NBDIM ,NBPAR2
      INTEGER  MXCMEL,NBPGMX,NBRES, NBRES2,I,     K
      INTEGER  NNO,   NNOS,  NPG,   IRET
      INTEGER  NBSIG, IGAU,  INDIC, INO,   NDIM,  IADZI
      INTEGER  IMATE, ITEMP2,IDTRIA,ICONPG,IENDPG,IAZK24
      INTEGER  IDTRGP,IDTRNO,IVARMR,IVARPR,IENDMG
      INTEGER  MXCVAR,IEPSP, JGANO, IPOIDS,IVF,   IDFDE
      INTEGER  ICOMPO,IBID, JTAB(7), NBVARI
      PARAMETER ( MXCMEL = 162 )
      PARAMETER ( NBPGMX =  27 )
      PARAMETER ( NBRES  =   2 )
      PARAMETER ( NBRES2 =   3 )
      PARAMETER ( MXCVAR = 378 )
      REAL*8             SIGMA(MXCMEL),SIGEQ(NBPGMX),TRSIG(NBPGMX)
      REAL*8             CENDO(NBPGMX),CENDON(NBPGMX),CENDOM(NBPGMX)
      REAL*8             SENDO(NBPGMX),SENDON(NBPGMX)
      REAL*8             R8PREM
      REAL*8             DOMLE(NBPGMX),DOMLEN(NBPGMX)
      REAL*8             DOMCU(NBPGMX),DOMCUN(NBPGMX)
      REAL*8             TRIAX(NBPGMX),TRIAXN(NBPGMX)
      REAL*8             VALRES(NBRES),VALRE2(NBRES2)
      REAL*8             ZERO,UN,DEUX,TROIS,UNDEMI,UNTIER,DETIER,TRDEMI
      REAL*8             XNU,COE1,COE2,R8VIDE
      REAL*8             PETITS(27),EXPO(27),IEXPO(27),RESU1,RESU2,XTEMP
      REAL*8             KMOISS,KPLUSS,KSOMM,PDIFF,PSEUIL(27),PPLUS,VALE
      REAL*8             DOMMOI(NBPGMX)
      REAL*8             XVARI1(MXCVAR),XVARI2(MXCVAR)
      REAL*8             XES,TS
      CHARACTER*4        FAMI
      CHARACTER*2        CODRES(NBRES),CODRE2(NBRES2)
      CHARACTER*24 VALK
      CHARACTER*8        MODELI,NOMPR2,NOMRES(NBRES),NOMAIL
      CHARACTER*8        NOMRE2(NBRES2)
      CHARACTER*16       PHENO,PHENOM,PHENO2,PHENM2
C.......................................................................
      DATA NOMRES / 'E','NU' /
      DATA NOMRE2 / 'S','EPSP_SEUIL','EXP_S' /
C S          = VARIABLE S AU DENOMINATEUR DE LA LOI DE LEMAITRE-SERMAGE
C EPSP_SEUIL = VALEUR SEUIL DE LA DEFORMATION PLASTIQUE
C EXP_S      = EXPOSANT s
C.......................................................................
      ZERO   = 0.0D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      UNDEMI = 1.0D0 / 2.0D0
      UNTIER = 1.0D0 / 3.0D0
      DETIER = 2.0D0 / 3.0D0
      TRDEMI = 3.0D0 / 2.0D0
      MODELI(1:2) = NOMTE(3:4)

C
C --- RECUPERATION DES CARACTERISTIQUES DU TYPE D'ELEMENT :
C     * NDIM  = DIMENSION DE L'ELEMENT
C     * NNO   = NOMBRE DE CONNECTIVITES
C     * NNOS  = NOMBRE DE NOEUDS SOMMETS
C     * NPG   = NOMBRE DE POINTS D'INTEGRATION
C     * NBSIG = NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C     ---------------------------------------------------
      NDIM = NBDIM(NOMTE)
C      ELREFA = (NDIM.EQ.3) .AND. (NOMTE(3:4).NE.'FO')
C      IF (ELREFA) THEN
        FAMI = 'RIGI'
        CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C      ELSE
C        NNO = NBNOEU(' ')
C        NNOS = NBNOSO(NOMTE)
C      END IF
      NBSIG = NBSIGM(MODELI)
C
      DO 10 I = 1, MXCMEL
         SIGMA(I)   = ZERO
  10  CONTINUE
      DO 11 I = 1, MXCVAR
         XVARI1(I) = ZERO
         XVARI2(I) = ZERO
  11  CONTINUE
C
      DO 20 I = 1, NBPGMX
         SIGEQ(I)  = ZERO
         TRSIG(I)  = ZERO
         CENDO(I)  = ZERO
         CENDON(I) = ZERO
         CENDOM(I) = ZERO
         SENDO(I)  = ZERO
         SENDON(I) = ZERO
         TRIAX(I)  = ZERO
         TRIAXN(I) = ZERO
         DOMMOI(I) = ZERO
         DOMLE(I)  = ZERO
         DOMLEN(I) = ZERO
         DOMCU(I)  = ZERO
         DOMCUN(I) = ZERO
  20  CONTINUE
C
      IF (OPTION.EQ.'ENDO_ELGA') THEN
C
C ---    RECUPERATION DES CARACTERISTIQUES MATERIAUX
C        ----------------------------------------------
         CALL JEVECH('PMATERC','L',IMATE)

C ---    EVALUATION DES DONNEES MATERIAUX POUR LA TEMPERATURE ITEMP2
C        -----------------------------------------------------------
         PHENO = 'ELAS'
         CALL RCCOMA (ZI(IMATE),PHENO,PHENOM,CODRES(1))
         IF (CODRES(1).EQ.'NO') CALL U2MESS('F','PREPOST_42')

         PHENO2 = 'DOMMA_LEMAITRE'
         CALL RCCOMA (ZI(IMATE),PHENO2,PHENM2,CODRES(1))
         IF (CODRES(1).EQ.'NO') CALL U2MESS('F','PREPOST_41')
C
C ---    ASSIGNATION DES VALEURS DES PARAMETRES DE LA LOI DE
C        LEMAITRE-SERMAGE EVALUEES A LA TEMPERATURE ACTUELLE ITEMP2
C        ----------------------------------------------------------

C
C --- RECHERCHE DU TYPE DU CHAMP D'ENTREE DEFINI AUX POINTS DE GAUSS
C     --------------------------------------------------------------
C Tenseur des contraintes
         CALL JEVECH('PCONTGP','L',ICONPG)
         IF (ICONPG.EQ.0) THEN
         CALL U2MESS('F','ELEMENTS4_7')
         ENDIF

C Taux triaxialité, contraintes endo, dommage @[t-]
         CALL JEVECH('PTRIAGM','L',IENDMG)
         IF (IENDMG.EQ.0) THEN
         CALL U2MESS('F','ELEMENTS4_8')
         ENDIF

C Taux triaxialité, contraintes endo, dommage @[t+]
         CALL JEVECH('PTRIAGP','L',IENDPG)
         IF (IENDPG.EQ.0) THEN
         CALL U2MESS('F','ELEMENTS4_8')
         ENDIF

C Variables internes @[t-]
         CALL JEVECH('PVARIMR','L',IVARMR)
         IF (IVARMR.EQ.0) THEN
         CALL U2MESS('F','ELEMENTS4_9')
         ENDIF

C Variables internes @[t+]
         CALL JEVECH('PVARIPR','L',IVARPR)
         IF (IVARPR.EQ.0) THEN
         CALL U2MESS('F','ELEMENTS4_10')
         ENDIF

      ELSEIF (OPTION.EQ.'ENDO_ELNO_ELGA') THEN
         CALL JEVECH('PTRIAGP','L',IENDPG)
         IF (IENDPG.EQ.0) THEN
         CALL U2MESS('F','ELEMENTS4_8')
         ENDIF
      ELSE
         CALL U2MESK('F','ELEMENTS4_11',1,OPTION)
      ENDIF
C
C     ------------------------------------------------------------------
C              CHAMP DE CONTRAINTES DEFINI AUX POINTS DE GAUSS
C     ------------------------------------------------------------------
      IF (OPTION.EQ.'ENDO_ELGA') THEN
C
C ---    AFFECTATION DU VECTEUR DE TRAVAIL SIGMA REPRESENTANT
C ---    LE TENSEUR DE CONTRAINTES
C        -------------------------
         K = 0
         DO 40 IGAU = 1, NPG
            DO 30 I = 1, NBSIG
               K = K+1
               SIGMA(I+(IGAU-1)*NBSIG) = ZR(ICONPG+K-1)
  30        CONTINUE
  40     CONTINUE
C
C ---    CALCUL DU DEVIATEUR DES CONTRAINTES
C        -----------------------------------
         DO 50 IGAU = 1, NPG
            INDIC = (IGAU-1)*NBSIG
            TRSIG(IGAU) = UNTIER       * ( SIGMA(INDIC+1)
     &                  + SIGMA(INDIC+2) + SIGMA(INDIC+3) )
            SIGMA(INDIC+1) =  SIGMA(INDIC+1) - TRSIG(IGAU)
            SIGMA(INDIC+2) =  SIGMA(INDIC+2) - TRSIG(IGAU)
            SIGMA(INDIC+3) =  SIGMA(INDIC+3) - TRSIG(IGAU)
  50     CONTINUE
C
C ---    CALCUL DE SIGEQ
C        ---------------
         DO 60 IGAU = 1, NPG
            INDIC = (IGAU-1)*NBSIG
            SIGEQ(IGAU) = SIGMA(INDIC+1) * SIGMA(INDIC+1)
     &                     + SIGMA(INDIC+2) * SIGMA(INDIC+2)
     &                     + SIGMA(INDIC+3) * SIGMA(INDIC+3)
     &                     + SIGMA(INDIC+4) * SIGMA(INDIC+4) * DEUX
            IF(NDIM.EQ.3) SIGEQ(IGAU) = SIGEQ(IGAU)
     &                     + SIGMA(INDIC+5) * SIGMA(INDIC+5) * DEUX
     &                     + SIGMA(INDIC+6) * SIGMA(INDIC+6) * DEUX
            SIGEQ(IGAU) = (SIGEQ(IGAU) * TRDEMI) ** UNDEMI
  60     CONTINUE
C
C ---    CALCUL DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C        -----------------------------------------------------
         DO 70 IGAU = 1, NPG
            IF (ABS(SIGEQ(IGAU)).LE.R8PREM()) THEN
               CALL TECAEL ( IADZI, IAZK24 )
               NOMAIL = ZK24(IAZK24-1+3)(1:8)
               VALK = NOMAIL
               CALL U2MESG('A', 'ELEMENTS5_1',1,VALK,0,0,0,0.D0)
               DO 72 INO = 1, NNO
                  TRIAXN(INO) = R8VIDE()
  72           CONTINUE
               GOTO 7777
            ENDIF
            TRIAX(IGAU) = TRSIG(IGAU) / SIGEQ(IGAU)
  70     CONTINUE
C
C ---    CALCUL DE LA CONTRAINTE EQUIVALENTE D'ENDOMMAGEMENT (SENDO)
C ---    ET DU CARRE DE LA CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT
C ---    NORMALISEE (CENDO) - NORMALISATION PAR XES = 2ES
C        ---------------------------------------------------------
         DO 80 IGAU = 1, NPG
            CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ',PHENOM,0,' ',
     &                  0.D0,NBRES,NOMRES,VALRES,CODRES,'FM')
            CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ',PHENM2,0,' ',
     &                  0.D0,NBRES2,NOMRE2,VALRE2,CODRE2,'FM')

            TS    =VALRE2(1)
            PSEUIL(IGAU)=VALRE2(2)
            PETITS(IGAU)=VALRE2(3)

            EXPO(IGAU)   = DEUX*PETITS(IGAU)+UN
            IEXPO(IGAU)  = UN/EXPO(IGAU)
            XES   = VALRES(1)*TS*DEUX
            XNU   = VALRES(2)
            COE1  = DETIER * (UN + XNU)
            COE2  = TROIS * (UN - DEUX * XNU)
            SENDO(IGAU) = (COE1*SIGEQ(IGAU)**DEUX
     &                     +COE2*TRSIG(IGAU)*TRSIG(IGAU))**UNDEMI
            CENDO(IGAU) = (COE1*SIGEQ(IGAU)**DEUX
     &                     +COE2*TRSIG(IGAU)*TRSIG(IGAU))/XES
  80     CONTINUE
C
C ---    CALCUL DE L'ENDOMMAGEMENT DE LEMAITRE-SERMAGE (DOMLE)
C        -------------------------------------------------------------
C Affectation du vecteur de travail XVARI[1,2], CENDOM et DOMMOI
C representant les variables internes @[t-,t+], la contrainte
C d'endommagement normalisée @t- et le dommage de Lemaitre-Sermage @t-
C
C ---    RECUPERATION DU COMPORTEMENT
C        ----------------------------
         CALL TECACH('OON','PVARIPR',7,JTAB,IRET)
         NBVARI = MAX(JTAB(6),1)*JTAB(7)
         NBSIG  = NBVARI
         CALL JEVECH('PCOMPOR','L',ICOMPO)

         CALL PSVARI (ZK16(ICOMPO),NBVARI,'3D',IEPSP,IBID)
         K = 0

         DO 41 IGAU = 1, NPG
            DO 31 I = 1, NBSIG
               K = K+1
               XVARI1(I+(IGAU-1)*NBSIG) = ZR(IVARMR+K-1)
               XVARI2(I+(IGAU-1)*NBSIG) = ZR(IVARPR+K-1)
  31        CONTINUE
            CENDOM(IGAU) = ZR(IENDMG-1+5*(IGAU-1)+3)
            DOMMOI(IGAU) = ZR(IENDMG-1+5*(IGAU-1)+4)
            DOMCU(IGAU)  = ZR(IENDMG-1+5*(IGAU-1)+5)
  41     CONTINUE

C
C Récupération de la deformation plastique cumulee stockee dans la
C première composante de VARI_[1,2] (IEPSP = 1)

         DO 81 IGAU = 1, NPG

            PPLUS = XVARI2(IEPSP+(IGAU-1)*NBSIG)
            IF (PPLUS.GT.PSEUIL(IGAU)-ZERO) THEN
               IF (DOMMOI(IGAU).GE.UN) THEN
                  RESU1 = ZERO
               ELSE
                  RESU1  = (UN-DOMMOI(IGAU))**EXPO(IGAU)
               ENDIF
               KMOISS = CENDOM(IGAU)**PETITS(IGAU)
               KPLUSS = CENDO(IGAU)**PETITS(IGAU)
               KSOMM  = KMOISS + KPLUSS
               PDIFF  =   XVARI2(IEPSP+(IGAU-1)*NBSIG)
     &                  - XVARI1(IEPSP+(IGAU-1)*NBSIG)
               RESU2  = (EXPO(IGAU)/DEUX)*KSOMM*PDIFF
               VALE = RESU1 - RESU2
               IF (VALE.GT.ZERO) THEN
                  DOMLE(IGAU) = UN-(VALE)**IEXPO(IGAU)
               ELSE
                  DOMLE(IGAU) = UN+(-UN*VALE)**IEXPO(IGAU)
               ENDIF
            ELSE
               DOMLE(IGAU) = ZERO
            ENDIF
C La valeur de l'endommagement est bornée à 1
            IF (DOMLE(IGAU).GT.UN) THEN
               DOMLE(IGAU) = UN
            ENDIF
C Calcul du dommage cumule
            DOMCU(IGAU) = DOMCU(IGAU) + DOMLE(IGAU)
  81     CONTINUE
C
C
C     ------------------------------------------------------------------
C                 CHAMP DE CONTRAINTES DEFINI AUX NOEUDS
C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ENDO_ELNO_ELGA' ) THEN
C
C ---    RECUPERATION DES CHAMPS ENDO_ELGA AUX PTS DE GAUSS
C        --------------------------------------------------
         DO 100 IGAU = 1, NPG
            TRIAX(IGAU) = ZR(IENDPG-1+5*(IGAU-1)+1)
            SENDO(IGAU) = ZR(IENDPG-1+5*(IGAU-1)+2)
            CENDO(IGAU) = ZR(IENDPG-1+5*(IGAU-1)+3)
            DOMLE(IGAU) = ZR(IENDPG-1+5*(IGAU-1)+4)
            DOMCU(IGAU) = ZR(IENDPG-1+5*(IGAU-1)+5)
  100    CONTINUE
C
C ---    CALCUL DES VALEURS AUX NOEUDS :
C        -------------------------------
         CALL PPGAN2(JGANO, 1, TRIAX, TRIAXN)
         CALL PPGAN2(JGANO, 1, SENDO, SENDON)
         CALL PPGAN2(JGANO, 1, CENDO, CENDON)
         CALL PPGAN2(JGANO, 1, DOMLE, DOMLEN)
         CALL PPGAN2(JGANO, 1, DOMCU, DOMCUN)
C
C ---    CHAMP DE CONTRAINTES MAL DEFINI
C        -------------------------------
      ELSE
         CALL U2MESK('F','ELEMENTS4_6',1,OPTION)
      ENDIF
C
 7777 CONTINUE
C
C --- RECUPERATION ET AFFECTATION AUX NOEUDS OU PT DE GAUSS DES:
C       * TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAXN ou TRIAX)
C       * CONTRAINTE EQUIVALENTE D'ENDOMMAGEMENT NORMALISEE
C         (CENDON ou CENDO)
C       * ENDOMMAGEMENT DE LEMAITRE-SERMAGE (DOMLEN ou DOMLE)
C
C ---    TRAITEMENT DE L'OPTION ENDO_ELGA
C        -------------------------------------
      IF (OPTION.EQ.'ENDO_ELGA') THEN
         CALL JEVECH ( 'PTRIAGP' , 'E' , IDTRGP )
         DO 150 IGAU = 1, NPG
            ZR(IDTRGP-1+5*(IGAU-1)+1) = TRIAX(IGAU)
            ZR(IDTRGP-1+5*(IGAU-1)+2) = SENDO(IGAU)
            ZR(IDTRGP-1+5*(IGAU-1)+3) = CENDO(IGAU)
            ZR(IDTRGP-1+5*(IGAU-1)+4) = DOMLE(IGAU)
            ZR(IDTRGP-1+5*(IGAU-1)+5) = DOMCU(IGAU)
  150    CONTINUE

C ---    TRAITEMENT DE L'OPTION ENDO_ELNO_ELGA
C        -------------------------------------
      ELSEIF (OPTION.EQ.'ENDO_ELNO_ELGA') THEN
         CALL JEVECH ( 'PTRIANO' , 'E' , IDTRNO )
         DO 160 INO = 1, NNO
            ZR(IDTRNO-1+5*(INO-1)+1) = TRIAXN(INO)
            ZR(IDTRNO-1+5*(INO-1)+2) = SENDON(INO)
            ZR(IDTRNO-1+5*(INO-1)+3) = CENDON(INO)
            ZR(IDTRNO-1+5*(INO-1)+4) = DOMLEN(INO)
            ZR(IDTRNO-1+5*(INO-1)+5) = DOMCUN(INO)
  160    CONTINUE
      ENDIF
C
      END
