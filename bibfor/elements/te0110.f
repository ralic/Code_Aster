      SUBROUTINE TE0110 ( OPTION , NOMTE )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16  OPTION,NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE  CRP_20

C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_EVOL'
C                          CAS COQUES SURFACIQUES ET LEURS BORDS
C                         (CAS COQUES LINEIQUES NON FAIT)
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER    NBRES,NBPAR,NDIMAX
C-----------------------------------------------------------------------
      INTEGER ICOEHF ,ICOEHR ,IER ,IRET ,IVF1 ,IVF2 ,KQ
      INTEGER L ,MZR ,NBDDL ,NBNOSO ,NBV ,NBVAR
      REAL*8 DELTAT ,PK ,PM ,POI1 ,POI2 ,THETA ,UN
      REAL*8 XGAU ,YGAU ,ZERO ,ZGAU
C-----------------------------------------------------------------------
      PARAMETER (NBRES=30)
      PARAMETER (NBPAR=4)
      PARAMETER (NDIMAX=27)
      INTEGER ICODRE(NBRES)
      CHARACTER*2 NUM
      INTEGER CODMAT
      CHARACTER*8 HFMOIN,HFPLUS,HFBORD,ALIAS8,FAMI,POUM
      CHARACTER*8 NOMRES(NBRES),NOMPAR(NBPAR)
      CHARACTER*16 PHENOM
      REAL*8 VALRES(NBRES),VALPAR(NBPAR),HOM(NBRES)
      REAL*8 M(3,3),ROC,H,TPG(3),LAM,DTPGDX(3),B(3,3)
      REAL*8 COOR2D(18),DFDX(9),DFDY(9),POIDS,DTPGDY(3)
      REAL*8 AXE(3,3),ANG(2),A(3,3,2,2)
      REAL*8 MATN(3,3),MATP(3,3)
      REAL*8 MATREF(3),MATELE(3)
      REAL*8 RIGITH(NDIMAX,NDIMAX),MASSE(NDIMAX,NDIMAX)
      REAL*8 LONG,HMOIN,HPLUS,HBORD
      INTEGER I,J,NNO,KP,NPG1,NPG2,GI,PI,IVECTT,ITEMP,ICACOQ
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE,NNOS,JGANO
      INTEGER ITEMPS,K,PJ,GJ,NDIM,IBID,KPG,SPT


C --- DETERMINATION DU SECOND MEMBRE CHAR_THER_EVOL :
C --- F =   1/DT*MASSE_THER*(T-) - (1-THETA)*RIGI_THER*(T-)
C ---    - (1-THETA)*HPLUS*(T-)  - (1-THETA)*HMOIN*(T-)
C --- MASSE_THER DESIGNE LA MASSE THERMIQUE
C --- RIGI_THER EST LA RIGIDITE THERMIQUE
C --- LES AUTRES TERMES PRIS EN COMPTE SONT SEULEMENT DES TERMES
C --- D'ECHANGE.
C --- T- DESIGNE LE CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
C --- HPLUS EST LE COEFFICIENT D'ECHANGE DE LA SURFACE SUPERIEURE
C --- DE LA COQUE AVEC L'EXTERIEUR
C --- HMOIN EST LE COEFFICIENT D'ECHANGE DE LA SURFACE INFERIEURE
C --- DE LA COQUE AVEC L'EXTERIEUR
C --- EN FAIT LES EXPRESSIONS HPLUS ET HMOIN SONT
C --- DISCRETISEES ET INTEGREES SUR LES SURFACES SUR LESQUELLES
C --- ELLES S'APPLIQUENT .
C --- LES OPERATEURS DE MASSE THERMIQUE, RIGIDITE THERMIQUE ET
C --- D'ECHANGE S'APPLIQUENT SUR LE TRIPLET (T_MOY,T_INF,T_SUP)
C --- DANS CET ORDRE OU
C --- T_MOY EST LA TEMPERATURE SUR LE FEUILLET MOYEN DE LA COQUE
C --- T_INF EST LA TEMPERATURE SUR LE FEUILLET INFERIEUR DE LA COQUE
C --- T_SUP EST LA TEMPERATURE SUR LE FEUILLET SUPERIEUR DE LA COQUE
C     ==============================================================
C --- INITIALISATIONS :
C     ---------------
      ZERO = 0.0D0
      UN = 1.0D0
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      NOMPAR(1) = 'INST'
      NOMPAR(2) = 'X'
      NOMPAR(3) = 'Y'
      NOMPAR(4) = 'Z'
      VALPAR(1) = ZERO
      VALPAR(2) = ZERO
      VALPAR(3) = ZERO
      VALPAR(4) = ZERO
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      HPLUS = ZERO
      HMOIN = ZERO
      HBORD = ZERO

      MATREF(1) = ZERO
      MATREF(2) = ZERO
      MATREF(3) = ZERO
      MATELE(1) = ZERO
      MATELE(2) = ZERO
      MATELE(3) = ZERO

      DO 20 I = 1,NDIMAX
        DO 10 J = 1,NDIMAX
          RIGITH(I,J) = ZERO
          MASSE(I,J) = ZERO
   10   CONTINUE
   20 CONTINUE

C --- RECUPERATION DES COORDONNEES DES NOEUDS DE L'ELEMENT :
C     ----------------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)

C --- RECUPERATION DE L'EPAISSEUR DE LA COQUE ET DES 2 ANGLES
C --- PERMETTANT DE PASSER DU REPERE GLOBAL AU REPERE DE REFERENCE
C --- TANGENT A LA COQUE :
C     ------------------
      CALL JEVECH('PCACOQU','L',ICACOQ)

C --- RECUPERATION DE L'INSTANT DU CALCUL, DU PAS DE TEMPS ET
C --- DU PARAMETRE THETA DE LA METHODE 'THETA' UTILISEE
C --- POUR RESOUDRE L'EQUATION DIFFERENTIELLE EN TEMPS DE LA
C --- TEMPERATURE (EN STATIONNAIRE THETA =1 ) :
C     ---------------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)

C --- RECUPERATION DU CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT :
C     ----------------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMP)

      VALPAR(1) = ZR(ITEMPS)
      DELTAT = ZR(ITEMPS+1)
      THETA = ZR(ITEMPS+2)

C --- RECUPERATION EVENTUELLE DES COEFFICIENTS D'ECHANGE AVEC
C --- L'EXTERIEUR :
C     -----------
      CALL TECACH('NNN','PCOEFHR',1,ICOEHR,IRET)
      CALL TECACH('NNN','PCOEFHF',1,ICOEHF,IRET)

      IF (NOMTE.NE.'THCOSE3 ' .AND. NOMTE.NE.'THCOSE2 ') THEN
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES REELS :
        IF (ICOEHR.NE.0) THEN
          HMOIN = ZR(ICOEHR)
          HPLUS = ZR(ICOEHR+1)
        END IF
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES FONCTIONS :
        IF (ICOEHF.NE.0) THEN
          HFMOIN = ZK8(ICOEHF)
          HFPLUS = ZK8(ICOEHF+1)
        END IF
      ELSE IF (NOMTE(1:8).EQ.'THCOSE3 ' .OR.
     &         NOMTE(1:8).EQ.'THCOSE2 ') THEN
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES REELS :
        IF (ICOEHR.GT.0) THEN
          H = ZR(ICOEHR)
        END IF
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES FONCTIONS :
        IF (ICOEHF.GT.0) THEN
          HFBORD = ZK8(ICOEHF)
        END IF
      END IF

C --- NOMBRE DE NOEUDS SOMMETS :
C     ------------------------
      CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
      IF (ALIAS8(6:7).EQ.'TR') THEN
        NBNOSO = 3
      ELSE IF (ALIAS8(6:7).EQ.'QU') THEN
        NBNOSO = 4
      END IF

C..................................................................
C.    CAS DES COQUES SURFACIQUES                                  .
C..................................................................

      IF (NOMTE(1:8).NE.'THCOSE3 ' .AND. NOMTE(1:8).NE.'THCOSE2 ') THEN

C ---   RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
C       -------------------------------------------------
        CALL RCCOMA(ZI(IMATE),'THER',PHENOM,CODMAT)

C ---   DETERMINATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE
C ---   ET TRANSVERSE ET DU TENSEUR DE CAPACITE THERMIQUE
C ---

C ---   CAS DES COQUES THERMIQUES MULTI-COUCHES
C       =======================================
        IF (PHENOM.EQ.'THER_COQMU') THEN

C ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
C ---   DE REFERENCE AU REPERE DE L'ELEMENT
C       -----------------------------------
          CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     &                ANG)

C ---   NOM DES COMPOSANTES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE :
C       ----------------------------------------------------------
          DO 30 I = 1,NBRES
            CALL CODENT(I,'G',NUM)
            NOMRES(I) = 'HOM_'//NUM
   30     CONTINUE

C ---   INTERPOLATION DES TERMES DU TENSEUR DE CONDUCTIVITE
C ---   EN FONCTION DU TEMPS :
C       --------------------
          NBVAR = 1
          CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ','THER_COQMU',
     &                NBVAR,NOMPAR,VALPAR,NBRES,NOMRES,VALRES,ICODRE,1)

C ---   CONSTRUCTION DE LA MATRICE DE PASSAGE DU REPERE UTILISATEUR
C ---   AU REPERE ELEMENT :
C       -----------------
          CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     &                ANG)

C ---   VALEURS DES CARACTERISIQUES DU MATERIAU DANS LE REPERE
C ---   DE L'ELEMENT ( PARCE QUE C'EST DANS CE REPERE QUE LE
C ---   FLUX THERMIQUE EST LE PLUS SIMPLE A ECRIRE) :
C       -------------------------------------------
          DO 40 I = 1,6
            CALL REFLTH(ANG,VALRES(3* (I-1)+1),HOM(3* (I-1)+1))
   40     CONTINUE

C ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
C       -----------------------------------
          A(1,1,1,1) = HOM(1)
          A(1,1,2,2) = HOM(2)
          A(1,1,1,2) = HOM(3)
          A(2,1,1,1) = HOM(4)
          A(2,1,2,2) = HOM(5)
          A(2,1,1,2) = HOM(6)
          A(3,1,1,1) = HOM(7)
          A(3,1,2,2) = HOM(8)
          A(3,1,1,2) = HOM(9)
          A(2,2,1,1) = HOM(10)
          A(2,2,2,2) = HOM(11)
          A(2,2,1,2) = HOM(12)
          A(3,2,1,1) = HOM(13)
          A(3,2,2,2) = HOM(14)
          A(3,2,1,2) = HOM(15)
          A(3,3,1,1) = HOM(16)
          A(3,3,2,2) = HOM(17)
          A(3,3,1,2) = HOM(18)

C ---   LES TERMES DE CONDUCTIVITE TRANSVERSE NE SE TRANSFORMENT PAS
C ---   PAR CHANGEMENT DE REPERE :
C       ------------------------
          B(1,1) = VALRES(19)
          B(2,1) = VALRES(20)
          B(3,1) = VALRES(21)
          B(2,2) = VALRES(22)
          B(3,2) = VALRES(23)
          B(3,3) = VALRES(24)

C ---   LES TERMES RELATIFS A LA CAPACITE CALORIFIQUE NE SE
C ---   TRANSFORMENT PAS PAR CHANGEMENT DE REPERE :
C       -----------------------------------------
          M(1,1) = VALRES(25)
          M(2,1) = VALRES(26)
          M(3,1) = VALRES(27)
          M(2,2) = VALRES(28)
          M(3,2) = VALRES(29)
          M(3,3) = VALRES(30)

C ---   CAS DES COQUES THERMIQUES ISOTROPES
C       ===================================
        ELSE IF (PHENOM.EQ.'THER') THEN

C ---   INTERPOLATION DE LA CONDUCTIVITE EN FONCTION DU TEMPS
C ---   ET DE LA TEMPERATURE
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C       -------------------------
          NBV = 2
          NOMRES(1) = 'RHO_CP'
          NOMRES(2) = 'LAMBDA'
          NBVAR = 1
          CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ','THER',NBVAR,
     &              NOMPAR,VALPAR,NBV,NOMRES,  VALRES,ICODRE,1)

C ---   INITIALISATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE,
C ---   TRANSVERSE ET DU TENSEUR DE CAPACITE THERMIQUE :
C       ----------------------------------------------
          DO 80 L = 1,2
            DO 70 K = 1,L
              DO 60 I = 1,3
                DO 50 J = 1,I
                  A(I,J,K,L) = ZERO
                  B(I,J) = ZERO
                  M(I,J) = ZERO
   50           CONTINUE
   60         CONTINUE
   70       CONTINUE
   80     CONTINUE

C ---   CAPACITE THERMIQUE :
C       ------------------
          ROC = VALRES(1)

C ---   CONDUCTIVITE :
C       ------------
          LAM = VALRES(2)

C ---   DEMI-EPAISSEUR :
C       --------------
          H = ZR(ICACOQ)/2.D0

C ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
C       -----------------------------------
          A(1,1,1,1) = 16.D0*LAM*H/15.D0
          A(1,1,2,2) = A(1,1,1,1)
          A(2,2,1,1) = 4.D0*LAM*H/15.D0
          A(2,2,2,2) = A(2,2,1,1)
          A(3,3,1,1) = 4.D0*LAM*H/15.D0
          A(3,3,2,2) = A(2,2,1,1)
          A(2,1,1,1) = 2.D0*LAM*H/15.D0
          A(2,1,2,2) = A(2,1,1,1)
          A(3,1,1,1) = 2.D0*LAM*H/15.D0
          A(3,1,2,2) = A(3,1,1,1)
          A(3,2,1,1) = -LAM*H/15.D0
          A(3,2,2,2) = A(3,2,1,1)

C ---   MATRICE DE CONDUCTIVITE TRANSVERSE :
C       ----------------------------------
          B(1,1) = 16.D0*LAM/ (6.D0*H)
          B(2,1) = -8.D0*LAM/ (6.D0*H)
          B(3,1) = B(2,1)
          B(2,2) = 7.D0*LAM/ (6.D0*H)
          B(3,2) = LAM/ (6.D0*H)
          B(3,3) = B(2,2)
          B(1,2) = B(2,1)
          B(1,3) = B(3,1)
          B(2,3) = B(3,2)

C ---   TENSEUR DE CAPACITE THERMIQUE ISOTROPE :
C       --------------------------------------
          M(1,1) = 16.D0*ROC*H/15.D0
          M(2,1) = 2.D0*ROC*H/15.D0
          M(3,1) = 2.D0*ROC*H/15.D0
          M(2,2) = 4.D0*ROC*H/15.D0
          M(3,2) = -ROC*H/15.D0
          M(3,3) = 4.D0*ROC*H/15.D0
          M(1,2) = M(2,1)
          M(1,3) = M(3,1)
          M(2,3) = M(3,2)

C ---   CAS DES COQUES THERMIQUES HETEROGENES HOMOGENEISEES
C       ===================================================
        ELSE IF (PHENOM.EQ.'THER_COQUE') THEN

C ---   LES DIRECTIONS 1 ET 2 DESIGNENT CELLES DU PLAN DE LA PLAQUE
C ---   LA DIRECTION 3 EST PERPENDICULAIRE
C ---   ON ADMET QUE LE TENSEUR DE CONDUCTIVITE EN CHAQUE POINT
C ---   EST DIAGONAL ET QUE SES VALEURS PROPRES SONT
C ---   LAMBDA_1 , LAMBDA_2 ET LAMBDA_3
C ---   D'AUTRE PART, SOIENT P1(X3), P2(X3), P3(X3) LES POLYNOMES
C ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS
C ---   A L'INTERPOLATION DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
C ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
C ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
C ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
C ---   LES TERMES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE SONT ALORS
C ---   POUR LE TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
C       -------------------------------------------
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P1(X3)*P1(X3).DX3)
          NOMRES(1) = 'COND_LMM'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P1(X3)*P2(X3).DX3)
          NOMRES(2) = 'COND_LMP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P2(X3)*P2(X3).DX3)
          NOMRES(3) = 'COND_LPP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P2(X3)*P3(X3).DX3)
          NOMRES(4) = 'COND_LSI'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P1(X3)*P1(X3).DX3)
          NOMRES(5) = 'COND_TMM'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P1(X3)*P2(X3).DX3)
          NOMRES(6) = 'COND_TMP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P2(X3)*P2(X3).DX3)
          NOMRES(7) = 'COND_TPP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P2(X3)*P3(X3).DX3)
          NOMRES(8) = 'COND_TSI'

C ---   ON NE DONNE QUE 8 TERMES CAR ON A LES EGALITES SUIVANTES :
C ---   SOMME_EP(P2(X3)*P2(X3).DX3) = SOMME_EP(P3(X3)*P3(X3).DX3)
C ---   SOMME_EP(P1(X3)*P2(X3).DX3) = SOMME_EP(P1(X3)*P3(X3).DX3)

C ---   POUR LE TENSEUR DE CONDUCTIVITE TRANSVERSE :
C       ------------------------------------------
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P1'(X3)*P1'(X3).DX3)
          NOMRES(9) = 'COND_NMM'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P1'(X3)*P2'(X3).DX3)
          NOMRES(10) = 'COND_NMP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P2'(X3)*P2'(X3).DX3)
          NOMRES(11) = 'COND_NPP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P2'(X3)*P3'(X3).DX3)
          NOMRES(12) = 'COND_NSI'

C ---   ON N'A DONNE QUE 4 TERMES CAR ON A LES EGALITES SUIVANTES :
C ---   SOMME_EP(P2'(X3)*P2'(X3).DX3) = SOMME_EP(P3'(X3)*P3'(X3).DX3)
C ---   SOMME_EP(P1'(X3)*P2'(X3).DX3) = SOMME_EP(P1'(X3)*P3'(X3).DX3)

C ---   POUR LE TENSEUR DE CAPACITE THERMIQUE :
C       -------------------------------------
C ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P1(X3).DX3) :
          NOMRES(13) = 'CMAS_MM'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P2(X3).DX3) :
          NOMRES(14) = 'CMAS_MP'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P2(X3).DX3) :
          NOMRES(15) = 'CMAS_PP'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P3(X3).DX3) :
          NOMRES(16) = 'CMAS_SI'

C ---  ON N'A DONNE QUE 4 TERMES A CAUSE DES 2 EGALITES DONNEES
C ---  APRES LA DEFINITION DES TERMES DU TENSEUR DE CONDUCTIVITE
C ---  MEMBRANAIRE

C ---   INTERPOLATION DES TERMES DES TENSEURS DE CONDUCTIVITE ET DE
C ---   CAPACITE THERMIQUE EN FONCTION DU TEMPS :
C       ---------------------------------------
          NBV = 16
          NBVAR = 1
          CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ',PHENOM,NBVAR,
     &             NOMPAR,VALPAR,NBV,NOMRES,   VALRES,ICODRE,1)

C ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
C ---   DE REFERENCE AU REPERE DE L'ELEMENT :
C       -----------------------------------
          CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     &                ANG)

C ---   PASSAGE DU REPERE DE REFERENCE AU REPERE DE L'ELEMENT :
C       -----------------------------------------------------

C ---   TERMES DE CONDUCTIVITE MEMBRANAIRE DANS LE REPERE DE L'ELEMENT :
C       --------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P1*P1.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P1*P1.DX3))
C       --------------------------------------------------------------
          MATREF(1) = VALRES(1)
          MATREF(2) = VALRES(5)
          MATREF(3) = ZERO
          CALL REFLTH(ANG,MATREF,MATELE)

          A(1,1,1,1) = MATELE(1)
          A(1,1,2,2) = MATELE(2)
          A(1,1,1,2) = MATELE(3)
          A(1,1,2,1) = MATELE(3)
C  ------------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P1*P2.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P1*P2.DX3))
C       --------------------------------------------------------------
          MATREF(1) = VALRES(2)
          MATREF(2) = VALRES(6)
          MATREF(3) = ZERO
          CALL REFLTH(ANG,MATREF,MATELE)

          A(1,2,1,1) = MATELE(1)
          A(1,2,2,2) = MATELE(2)
          A(1,2,1,2) = MATELE(3)
          A(1,2,2,1) = MATELE(3)

          A(2,1,1,1) = A(1,2,1,1)
          A(2,1,2,2) = A(1,2,2,2)
          A(2,1,1,2) = A(1,2,1,2)
          A(2,1,2,1) = A(1,2,2,1)

          A(1,3,1,1) = MATELE(1)
          A(1,3,2,2) = MATELE(2)
          A(1,3,1,2) = MATELE(3)
          A(1,3,2,1) = MATELE(3)

          A(3,1,1,1) = A(1,3,1,1)
          A(3,1,2,2) = A(1,3,2,2)
          A(3,1,1,2) = A(1,3,1,2)
          A(3,1,2,1) = A(1,3,2,1)
C  ------------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P2*P2.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P2*P2.DX3))
C       --------------------------------------------------------------
          MATREF(1) = VALRES(3)
          MATREF(2) = VALRES(7)
          MATREF(3) = ZERO
          CALL REFLTH(ANG,MATREF,MATELE)

          A(2,2,1,1) = MATELE(1)
          A(2,2,2,2) = MATELE(2)
          A(2,2,1,2) = MATELE(3)
          A(2,2,2,1) = MATELE(3)

          A(3,3,1,1) = MATELE(1)
          A(3,3,2,2) = MATELE(2)
          A(3,3,1,2) = MATELE(3)
          A(3,3,2,1) = MATELE(3)
C  ------------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P2*P3.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P2*P3.DX3))
C       --------------------------------------------------------------
          MATREF(1) = VALRES(4)
          MATREF(2) = VALRES(8)
          MATREF(3) = ZERO
          CALL REFLTH(ANG,MATREF,MATELE)

          A(2,3,1,1) = MATELE(1)
          A(2,3,2,2) = MATELE(2)
          A(2,3,1,2) = MATELE(3)
          A(2,3,2,1) = MATELE(3)

          A(3,2,1,1) = A(2,3,1,1)
          A(3,2,2,2) = A(2,3,2,2)
          A(3,2,1,2) = A(2,3,1,2)
          A(3,2,2,1) = A(2,3,2,1)
C  ------------------------------------------------------------------

C ---   TERMES DE CONDUCTIVITE TRANSVERSE :
C       ---------------------------------
          B(1,1) = VALRES(9)
          B(1,2) = VALRES(10)
          B(1,3) = VALRES(10)
          B(2,2) = VALRES(11)
          B(2,3) = VALRES(12)
          B(3,3) = VALRES(11)
          B(2,1) = B(1,2)
          B(3,1) = B(1,3)
          B(3,2) = B(2,3)

C ---   TERMES DE MASSE :
C       ---------------
          M(1,1) = VALRES(13)
          M(1,2) = VALRES(14)
          M(1,3) = VALRES(14)
          M(2,2) = VALRES(15)
          M(2,3) = VALRES(16)
          M(3,3) = VALRES(15)
          M(2,1) = M(1,2)
          M(3,1) = M(1,3)
          M(3,2) = M(2,3)

        ELSE
          CALL U2MESK('F','ELEMENTS3_17',1,PHENOM)
        END IF

C --- PRISE EN COMPTE DANS LE TENSEUR DE CONDUCTIVITE TRANSVERSE DES
C --- ECHANGES DES PEAUX INFERIEURE ET SUPERIEURE AVEC L'EXTERIEUR :
C     ------------------------------------------------------------
C --- CAS OU LES COEFFICIENTS D'ECHANGES SONT DES FONCTIONS :
        IF (ICOEHF.GT.0) THEN
          CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOIDS,
     &                     IVF,IDFDE,JGANO)
          DO 100 KP = 1,NPG2
            K = (KP-1)*NNO
            CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR2D,DFDX,DFDY,POIDS)

C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
            XGAU = ZERO
            YGAU = ZERO
            ZGAU = ZERO
            DO 90 I = 1,NNO
              XGAU = XGAU + ZR(IGEOM+3* (I-1))*ZR(IVF+K+I-1)
              YGAU = YGAU + ZR(IGEOM+3* (I-1)+1)*ZR(IVF+K+I-1)
              ZGAU = ZGAU + ZR(IGEOM+3* (I-1)+2)*ZR(IVF+K+I-1)
   90       CONTINUE

            VALPAR(2) = XGAU
            VALPAR(3) = YGAU
            VALPAR(4) = ZGAU
            NBVAR = 4
            CALL FOINTE('FM',HFMOIN,NBVAR,NOMPAR,VALPAR,HMOIN,IER)
            CALL FOINTE('FM',HFPLUS,NBVAR,NOMPAR,VALPAR,HPLUS,IER)
  100     CONTINUE
        END IF

C ---  CONTRIBUTION AU TENSEUR DE CONDUCTIVITE TRANSVERSE B DES
C ---  ECHANGES AVEC L'EXTERIEUR
C ---             (0 0  0 )
C ---    B_ECH =  (0 H- 0 )
C ---             (0 0  H+)
C      --------------------
        B(2,2) = B(2,2) + HMOIN
        B(3,3) = B(3,3) + HPLUS

C --- CALCUL DES COORDONNEES DES CONNECTIVITES DANS LE REPERE
C --- DE L'ELEMENT :
C     ------------
        CALL CQ3D2D(NNO,ZR(IGEOM),UN,ZERO,COOR2D)
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,
     &                   IVF,IDFDE,JGANO)

C --- CALCUL DE LA RIGIDITE THERMIQUE DUE A LA PARTIE MEMBRANAIRE
C --- DU TENSEUR DE CONDUCTIVITE :
C     ==========================

C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 180 KP = 1,NPG1
          K = (KP-1)*NNO

C ---   DERIVEES DES FONCTIONS DE FORME ET PRODUIT JACOBIEN*POIDS
C ---   (DANS POIDS)  SUR L'ELEMENT :
C       ---------------------------
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR2D,DFDX,DFDY,POIDS)
          DO 110 PI = 1,3
            TPG(PI) = ZERO
            DTPGDX(PI) = ZERO
            DTPGDY(PI) = ZERO
  110     CONTINUE

C ---   TEMPERATURES ET GRADIENTS THERMIQUES AUX POINTS D'INTEGRATION :
C       -------------------------------------------------------------
          DO 130 GI = 1,NNO
            DO 120 PI = 1,3
              I = 3* (GI-1) + PI - 1 + ITEMP
              TPG(PI) = TPG(PI) + ZR(I)*ZR(IVF+K+GI-1)
              DTPGDX(PI) = DTPGDX(PI) + ZR(I)*DFDX(GI)
              DTPGDY(PI) = DTPGDY(PI) + ZR(I)*DFDY(GI)
  120       CONTINUE
  130     CONTINUE
          DO 170 GI = 1,NNO
            DO 160 GJ = 1,GI
              DO 150 PI = 1,3
                DO 140 PJ = 1,PI
                  PK = A(PI,PJ,1,1)*DFDX(GI)*DFDX(GJ) +
     &                 A(PI,PJ,2,2)*DFDY(GI)*DFDY(GJ) +
     &                 A(PI,PJ,1,2)*DFDX(GI)*DFDY(GJ) +
     &                 A(PI,PJ,1,2)*DFDY(GI)*DFDX(GJ)

                  IF ((PI.NE.PJ) .AND. (GI.NE.GJ)) THEN
                    I = 3* (GI-1) + PJ
                    J = 3* (GJ-1) + PI
                    RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
                  END IF
                  I = 3* (GI-1) + PI
                  J = 3* (GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
  140           CONTINUE
  150         CONTINUE
  160       CONTINUE
  170     CONTINUE
  180   CONTINUE

C --- CALCUL DE LA RIGIDITE THERMIQUE DUE A LA PARTIE TRANSVERSE
C --- DU TENSEUR DE CONDUCTIVITE ET CALCUL DE LA MASSE THERMIQUE :
C     ==========================================================

C --- ON PREND LA SECONDE FAMILLE DE POINTS D'INTEGRATION QUI
C --- EST D'UN ORDRE PLUS ELEVE :
C     -------------------------
        CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOIDS,
     &                   IVF,IDFDE,JGANO)
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 230 KP = 1,NPG2
          K = (KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR2D,DFDX,DFDY,POIDS)

          DO 220 GI = 1,NNO
            IVF1 = IVF + K + GI - 1
            DO 210 GJ = 1,GI
              IVF2 = IVF + K + GJ - 1
              DO 200 PI = 1,3
                DO 190 PJ = 1,PI
                  PK = B(PI,PJ)*ZR(IVF1)*ZR(IVF2)
                  PM = M(PI,PJ)*ZR(IVF1)*ZR(IVF2)

C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                  IF ((PI.NE.PJ) .AND. (GI.NE.GJ)) THEN
                    I = 3* (GI-1) + PJ
                    J = 3* (GJ-1) + PI
                    RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
                    MASSE(I,J) = MASSE(I,J) + POIDS*PM
                  END IF

C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                  I = 3* (GI-1) + PI
                  J = 3* (GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
                  MASSE(I,J) = MASSE(I,J) + POIDS*PM
  190           CONTINUE
  200         CONTINUE
  210       CONTINUE
  220     CONTINUE
  230   CONTINUE
        DO 250 I = 1,3*NNO
          DO 240 J = 1,I - 1
            RIGITH(J,I) = RIGITH(I,J)
            MASSE(J,I) = MASSE(I,J)
  240     CONTINUE
  250   CONTINUE


C..................................................................
C.    CAS DES BORDS DES COQUES SURFACIQUES                        .
C.    ILS INTERVIENNENT PAR LEUR CONTRIBUTION A L'ECHANGE LATERAL .
C..................................................................
      ELSE IF (NOMTE(1:8).EQ.'THCOSE3 ' .OR.
     &         NOMTE(1:8).EQ.'THCOSE2 ') THEN
        CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DEMR',' ',MZR)


        LONG = (ZR(IGEOM+3)-ZR(IGEOM))**2 +
     &         (ZR(IGEOM+3+1)-ZR(IGEOM+1))**2 +
     &         (ZR(IGEOM+3+2)-ZR(IGEOM+2))**2

        LONG = SQRT(LONG)/2.D0
C       EP  =EP/2.D0

C ---   DETERMINATION DE LA 'PART' DE RIGIDITE THERMIQUE DU A L'ECHANGE
C ---   AVEC L'EXTERIEUR POUR LES COQUES LINEIQUES
C ---   ON RAPPELLE QUE LE TERME GENERIQUE DE CETTE MATRICE A POUR
C ---   EXPRESSION :
C ---   B(I,J) = SOMME_VOLUME(H*NI(X,Y,Z)*NJ(X,Y,Z).DX.DY.DZ)
C ---   SOIT
C ---   B(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY)
C ---           *SOMME_EPAISSEUR(PK(Z)*PL(Z).DZ)
C ---   OU LES PK ET PL SONT LES FONCTIONS D'INTERPOLATION DANS
C ---   L'EPAISSEUR
C ---   PLUS EXACTEMENT P1(Z), P2(Z), P3(Z) SONT LES POLYNOMES
C ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
C ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
C ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
C ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
C ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
C ---   (I.E. T(X,Y,Z) =    P1(Z)*TMOY(X,Y)
C ---                     + P2(Z)*TINF(X,Y)
C ---                     + P3(Z)*TSUP(X,Y)) :
C       ------------------------------------
        CALL R8INIR(9,ZERO,MATP,1)
        CALL R8INIR(9,ZERO,MATN,1)

C ---   DETERMINATION DE LA MATRICE MATP DONT LE TERME GENERIQUE
C ---   EST MATP(I,J) = SOMME_EPAISSEUR(PI(Z)*PJ(Z).DZ) :
C       -----------------------------------------------
        DO 260 KP = 1,NPG1
          KQ = (KP-1)*3

          POI1 = ZR(MZR-1+12+KP)

          MATP(1,1) = MATP(1,1) + POI1*ZR(MZR-1+KQ+1)**2
          MATP(1,2) = MATP(1,2) + POI1*ZR(MZR-1+KQ+1)*ZR(MZR-1+KQ+2)
          MATP(1,3) = MATP(1,3) + POI1*ZR(MZR-1+KQ+1)*ZR(MZR-1+KQ+3)
          MATP(2,1) = MATP(1,2)
          MATP(2,2) = MATP(2,2) + POI1*ZR(MZR-1+KQ+2)**2
          MATP(2,3) = MATP(2,3) + POI1*ZR(MZR-1+KQ+2)*ZR(MZR-1+KQ+3)
          MATP(3,1) = MATP(1,3)
          MATP(3,2) = MATP(2,3)
          MATP(3,3) = MATP(3,3) + POI1*ZR(MZR-1+KQ+3)**2
  260   CONTINUE

C ---   DETERMINATION DE LA MATRICE MATN DONT LE TERME GENERIQUE
C ---   EST MATN(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY) :
C       --------------------------------------------------------
        DO 280 KP = 1,NPG1
          K = (KP-1)*NNO

          POI2 = ZR(IPOIDS-1+KP)

          IF (ICOEHF.GT.0) THEN

C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
            XGAU = ZERO
            YGAU = ZERO
            ZGAU = ZERO
            DO 270 I = 1,NNO
              XGAU = XGAU + ZR(IGEOM+3* (I-1))*ZR(IVF+K+I-1)
              YGAU = YGAU + ZR(IGEOM+3* (I-1)+1)*ZR(IVF+K+I-1)
              ZGAU = ZGAU + ZR(IGEOM+3* (I-1)+2)*ZR(IVF+K+I-1)
  270       CONTINUE

            VALPAR(2) = XGAU
            VALPAR(3) = YGAU
            VALPAR(4) = ZGAU
            NBVAR = 4

            CALL FOINTE('FM',HFBORD,NBVAR,NOMPAR,VALPAR,HBORD,IER)
          END IF


C      IMPORTANT: LAMB = CONV * EPAISSEUR

C          LAMB=LAMB*LONG*THETA*EP
          HBORD = HBORD*LONG*THETA/2.D0

          MATN(1,1) = POI2*HBORD*ZR(IVF-1+K+1)**2
          MATN(1,2) = POI2*HBORD*ZR(IVF-1+K+1)*ZR(IVF-1+K+2)
          MATN(2,1) = MATN(1,2)
          MATN(2,2) = POI2*HBORD*ZR(IVF-1+K+2)**2

          IF (NOMTE(1:8).EQ.'THCOSE3 ') THEN
            MATN(1,3) = POI2*HBORD*ZR(IVF-1+K+1)*ZR(IVF-1+K+3)
            MATN(2,3) = POI2*HBORD*ZR(IVF-1+K+2)*ZR(IVF-1+K+3)
            MATN(3,1) = MATN(1,3)
            MATN(3,2) = MATN(2,3)
            MATN(3,3) = POI2*HBORD*ZR(IVF-1+K+3)**2
          END IF

          RIGITH(1,1) = RIGITH(1,1) + MATN(1,1)*MATP(1,1)
          RIGITH(1,2) = RIGITH(1,2) + MATN(1,1)*MATP(1,2)
          RIGITH(1,3) = RIGITH(1,3) + MATN(1,1)*MATP(1,3)
          RIGITH(1,4) = RIGITH(1,4) + MATN(1,2)*MATP(1,1)
          RIGITH(1,5) = RIGITH(1,5) + MATN(1,2)*MATP(1,2)
          RIGITH(1,6) = RIGITH(1,6) + MATN(1,2)*MATP(1,3)

          RIGITH(2,1) = RIGITH(1,2)
          RIGITH(2,2) = RIGITH(2,2) + MATN(1,1)*MATP(2,2)
          RIGITH(2,3) = RIGITH(2,3) + MATN(1,1)*MATP(2,3)
          RIGITH(2,4) = RIGITH(2,4) + MATN(1,2)*MATP(2,1)
          RIGITH(2,5) = RIGITH(2,5) + MATN(1,2)*MATP(2,2)
          RIGITH(2,6) = RIGITH(2,6) + MATN(1,2)*MATP(2,3)

          RIGITH(3,1) = RIGITH(1,3)
          RIGITH(3,2) = RIGITH(2,3)
          RIGITH(3,3) = RIGITH(3,3) + MATN(1,1)*MATP(3,3)
          RIGITH(3,4) = RIGITH(3,4) + MATN(1,2)*MATP(3,1)
          RIGITH(3,5) = RIGITH(3,5) + MATN(1,2)*MATP(3,2)
          RIGITH(3,6) = RIGITH(3,6) + MATN(1,2)*MATP(3,3)

          RIGITH(4,1) = RIGITH(1,4)
          RIGITH(4,2) = RIGITH(2,4)
          RIGITH(4,3) = RIGITH(3,4)
          RIGITH(4,4) = RIGITH(4,4) + MATN(2,2)*MATP(1,1)
          RIGITH(4,5) = RIGITH(4,5) + MATN(2,2)*MATP(1,2)
          RIGITH(4,6) = RIGITH(4,6) + MATN(2,2)*MATP(1,3)

          RIGITH(5,1) = RIGITH(1,5)
          RIGITH(5,2) = RIGITH(2,5)
          RIGITH(5,3) = RIGITH(3,5)
          RIGITH(5,4) = RIGITH(4,5)
          RIGITH(5,5) = RIGITH(5,5) + MATN(2,2)*MATP(2,2)
          RIGITH(5,6) = RIGITH(5,6) + MATN(2,2)*MATP(2,3)

          RIGITH(6,1) = RIGITH(1,6)
          RIGITH(6,2) = RIGITH(2,6)
          RIGITH(6,3) = RIGITH(3,6)
          RIGITH(6,4) = RIGITH(4,6)
          RIGITH(6,5) = RIGITH(5,6)
          RIGITH(6,6) = RIGITH(6,6) + MATN(2,2)*MATP(3,3)

          IF (NOMTE(1:8).EQ.'THCOSE3 ') THEN

            RIGITH(1,7) = RIGITH(1,7) + MATN(1,3)*MATP(1,1)
            RIGITH(1,8) = RIGITH(1,8) + MATN(1,3)*MATP(1,2)
            RIGITH(1,9) = RIGITH(1,9) + MATN(1,3)*MATP(1,3)

            RIGITH(2,7) = RIGITH(2,7) + MATN(1,3)*MATP(2,1)
            RIGITH(2,8) = RIGITH(2,8) + MATN(1,3)*MATP(2,2)
            RIGITH(2,9) = RIGITH(2,9) + MATN(1,3)*MATP(2,3)

            RIGITH(3,7) = RIGITH(3,7) + MATN(1,3)*MATP(3,1)
            RIGITH(3,8) = RIGITH(3,8) + MATN(1,3)*MATP(3,2)
            RIGITH(3,9) = RIGITH(3,9) + MATN(1,3)*MATP(3,3)

            RIGITH(4,7) = RIGITH(4,7) + MATN(2,3)*MATP(1,1)
            RIGITH(4,8) = RIGITH(4,8) + MATN(2,3)*MATP(1,2)
            RIGITH(4,9) = RIGITH(4,9) + MATN(2,3)*MATP(1,3)

            RIGITH(5,7) = RIGITH(5,7) + MATN(2,3)*MATP(2,1)
            RIGITH(5,8) = RIGITH(5,8) + MATN(2,3)*MATP(2,2)
            RIGITH(5,9) = RIGITH(5,9) + MATN(2,3)*MATP(2,3)

            RIGITH(6,7) = RIGITH(6,7) + MATN(2,3)*MATP(3,1)
            RIGITH(6,8) = RIGITH(6,8) + MATN(2,3)*MATP(3,2)
            RIGITH(6,9) = RIGITH(6,9) + MATN(2,3)*MATP(3,3)

            RIGITH(7,1) = RIGITH(1,7)
            RIGITH(7,2) = RIGITH(2,7)
            RIGITH(7,3) = RIGITH(3,7)
            RIGITH(7,4) = RIGITH(4,7)
            RIGITH(7,5) = RIGITH(5,7)
            RIGITH(7,6) = RIGITH(6,7)
            RIGITH(7,7) = RIGITH(7,7) + MATN(3,3)*MATP(1,1)
            RIGITH(7,8) = RIGITH(7,8) + MATN(3,3)*MATP(1,2)
            RIGITH(7,9) = RIGITH(7,9) + MATN(3,3)*MATP(1,3)

            RIGITH(8,1) = RIGITH(1,8)
            RIGITH(8,2) = RIGITH(2,8)
            RIGITH(8,3) = RIGITH(3,8)
            RIGITH(8,4) = RIGITH(4,8)
            RIGITH(8,5) = RIGITH(5,8)
            RIGITH(8,6) = RIGITH(6,8)
            RIGITH(8,7) = RIGITH(7,8)
            RIGITH(8,8) = RIGITH(8,8) + MATN(3,3)*MATP(2,2)
            RIGITH(8,9) = RIGITH(8,9) + MATN(3,3)*MATP(2,3)

            RIGITH(9,1) = RIGITH(1,9)
            RIGITH(9,2) = RIGITH(2,9)
            RIGITH(9,3) = RIGITH(3,9)
            RIGITH(9,4) = RIGITH(4,9)
            RIGITH(9,5) = RIGITH(5,9)
            RIGITH(9,6) = RIGITH(6,9)
            RIGITH(9,7) = RIGITH(7,9)
            RIGITH(9,8) = RIGITH(8,9)
            RIGITH(9,9) = RIGITH(9,9) + MATN(3,3)*MATP(3,3)
          END IF

  280   CONTINUE

C..................................................................
      END IF
C..................................................................

C --- RECUPERATION DU VECTEUR SECOND MEMBRE EN SORTIE DE CHAR_THER_EVOL:
C     ----------------------------------------------------------------
      CALL JEVECH('PVECTTR','E',IVECTT)

C --- AFFECTATION DU VECTEUR SECOND MEMBRE EN SORTIE DE
C --- CHAR_THER_EVOL :
C     --------------
      NBDDL = 3*NNO
      DO 300 I = 1,NBDDL
        DO 290 J = 1,NBDDL
          ZR(IVECTT+I-1) = ZR(IVECTT+I-1) +
     &                     (MASSE(J,I)/DELTAT- (UN-THETA)*RIGITH(J,I))*
     &                     ZR(ITEMP+J-1)
  290   CONTINUE
  300 CONTINUE

      END
