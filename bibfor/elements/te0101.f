      SUBROUTINE TE0101(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_THER      '
C                          CAS COQUE
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

C-----------------------------------------------------------------------
      INTEGER I ,IND ,ITEMPS ,J ,L ,NBDDL ,NBNOSO
      INTEGER NBRES ,NBV ,NBVAR ,NDIMAX
      REAL*8 UN
C-----------------------------------------------------------------------
      PARAMETER (NDIMAX=27)
      PARAMETER (NBRES=24)
      PARAMETER (NBVAR=2)
      INTEGER ICODRE(NBRES),KPG,SPT
      CHARACTER*2 NUM
      CHARACTER*8 NOMRES(NBRES),NOMPAR(NBVAR),ALIAS8,FAMI,POUM
      CHARACTER*16 PHENOM
      REAL*8 B(3,3),A(3,3,2,2),CONDUC,H,THETA
      REAL*8 VALRES(NBRES),AXE(3,3),ANG(2),HOM(NBRES)
      REAL*8 DFDX(9),DFDY(9),POIDS,PK,COOR2D(18)
      REAL*8 MUN,ZERO,DEUX,QUATRE,SIX,SEPT,HUIT
      REAL*8 QUINZE,SEIZE,R
      REAL*8 COUR,COSA,SINA
      REAL*8 MATREF(3),MATELE(3)
      REAL*8 VALPAR(NBVAR),TEMPE,INSTAN
      REAL*8 RIGITH(NDIMAX,NDIMAX)
      INTEGER IMATE,ICACOQ,IBID
      INTEGER NNO,KP,NPG1,NPG2,GI,PI,GJ,PJ,K,IMATTT,NDIM,NNOS
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,JGANO,JGANO2
      INTEGER NDIM2,NNO2,NNOS2

C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
C --- INITIALISATIONS :
C     ---------------
      MUN = -1.0D0
      ZERO = 0.0D0
      UN = 1.0D0
      DEUX = 2.0D0
      QUATRE = 4.0D0
      SIX = 6.0D0
      SEPT = 7.0D0
      HUIT = 8.0D0
      QUINZE = 15.0D0
      SEIZE = 16.0D0
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'

      TEMPE = ZERO
      INSTAN = ZERO
      NOMPAR(1) = 'INST'
      NOMPAR(2) = 'TEMP'
      VALPAR(1) = INSTAN
      VALPAR(2) = TEMPE

      MATREF(1) = ZERO
      MATREF(2) = ZERO
      MATREF(3) = ZERO
      MATELE(1) = ZERO
      MATELE(2) = ZERO
      MATELE(3) = ZERO

      DO 20 I = 1,NDIMAX
        DO 10 J = 1,NDIMAX
          RIGITH(I,J) = ZERO
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

C --- RECUPERATION DE L'INSTANT DU CALCUL ET
C --- DU PARAMETRE THETA DE LA METHODE 'THETA' UTILISEE
C --- POUR RESOUDRE L'EQUATION DIFFERENTIELLE EN TEMPS DE LA
C --- TEMPERATURE (EN STATIONNAIRE THETA =1 ) :
C     ---------------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      VALPAR(1) = ZR(ITEMPS)
      THETA = ZR(ITEMPS+2)

C --- NOMBRE DE NOEUDS SOMMETS :
C     ------------------------
      CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
      IF (ALIAS8(6:7).EQ.'TR') THEN
        NBNOSO = 3
      ELSE IF (ALIAS8(6:7).EQ.'QU') THEN
        NBNOSO = 4
      END IF

C --- RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
C     -------------------------------------------------
      CALL RCCOMA(ZI(IMATE),'THER',1,PHENOM,ICODRE)

C --- DETERMINATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE
C --- ET TRANSVERSE :
C     =============

C --- CAS DES COQUES MULTICOUCHES :
C     ---------------------------
      IF (PHENOM.EQ.'THER_COQMU') THEN

C ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
C ---   DE REFERENCE AU REPERE DE L'ELEMENT :
C       -----------------------------------
        CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     &              ANG)

C ---   NOM DES COMPOSANTES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE :
C       ----------------------------------------------------------
        DO 30 I = 1,NBRES
          CALL CODENT(I,'G',NUM)
          NOMRES(I) = 'HOM_'//NUM
   30   CONTINUE

C ---   INTERPOLATION DES TERMES DU TENSEUR DE CONDUCTIVITE
C ---   EN FONCTION DU TEMPS ET DE LA TEMPERATURE
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C       -------------------------
        CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ','THER_COQMU',NBVAR,
     &             NOMPAR,VALPAR,NBRES, NOMRES,VALRES,ICODRE,1)

C ---   VALEURS DES CARACTERISIQUES DU MATERIAU DANS LE REPERE
C ---   DE L'ELEMENT ( PARCE QUE C'EST DANS CE REPERE QUE LE
C ---   FLUX THERMIQUE EST LE PLUS SIMPLE A ECRIRE) :
C       -------------------------------------------
        DO 40 I = 1,6
          CALL REFLTH(ANG,VALRES(3* (I-1)+1),HOM(3* (I-1)+1))
   40   CONTINUE

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

C ---   TENSEUR DE CONDUCTIVITE TRANSVERSE :
C       ----------------------------------
        B(1,1) = VALRES(19)
        B(2,1) = VALRES(20)
        B(3,1) = VALRES(21)
        B(2,2) = VALRES(22)
        B(3,2) = VALRES(23)
        B(3,3) = VALRES(24)

C --- CAS DES COQUES ISOTROPES :
C     ------------------------
      ELSE IF (PHENOM.EQ.'THER') THEN

C ---   INTERPOLATION DE LA CONDUCTIVITE EN FONCTION DU TEMPS
C ---   ET DE LA TEMPERATURE
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C       -------------------------
        NBV = 1
        NOMRES(1) = 'LAMBDA'
        CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ','THER',NBVAR,NOMPAR,
     &              VALPAR,NBV,NOMRES,VALRES,ICODRE,1)

C ---   CONDUCTIVITE  :
C       ------------
        CONDUC = VALRES(1)

C ---   DEMI-EPAISSEUR  :
C       --------------
        H = ZR(ICACOQ)/DEUX

C ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
C       -----------------------------------
        DO 80 L = 1,2
          DO 70 K = 1,L
            DO 60 I = 1,3
              DO 50 J = 1,I
                A(I,J,K,L) = ZERO
   50         CONTINUE
   60       CONTINUE
   70     CONTINUE
   80   CONTINUE

        A(1,1,1,1) = SEIZE*CONDUC*H/QUINZE
        A(1,1,2,2) = A(1,1,1,1)
        A(2,2,1,1) = QUATRE*CONDUC*H/QUINZE
        A(2,2,2,2) = A(2,2,1,1)
        A(3,3,1,1) = QUATRE*CONDUC*H/QUINZE
        A(3,3,2,2) = A(2,2,1,1)
        A(2,1,1,1) = DEUX*CONDUC*H/QUINZE
        A(2,1,2,2) = A(2,1,1,1)
        A(3,1,1,1) = DEUX*CONDUC*H/QUINZE
        A(3,1,2,2) = A(3,1,1,1)
        A(3,2,1,1) = MUN*CONDUC*H/QUINZE
        A(3,2,2,2) = A(3,2,1,1)

C ---   TENSEUR DE CONDUCTIVITE TRANSVERSE :
C       ----------------------------------
        B(1,1) = SEIZE*CONDUC/ (SIX*H)
        B(2,1) = MUN*HUIT*CONDUC/ (SIX*H)
        B(3,1) = B(2,1)
        B(2,2) = SEPT*CONDUC/ (SIX*H)
        B(3,2) = CONDUC/ (SIX*H)
        B(3,3) = B(2,2)

C --- CAS DES COQUES HETEROGENES :
C     --------------------------
      ELSE IF (PHENOM.EQ.'THER_COQUE') THEN

C ---   LES DIRECTIONS 1 ET 2 DESIGNENT CELLES DU PLAN DE LA PLAQUE
C ---   LA DIRECTION 3 EST PERPENDICULAIRE
C ---   ON ADMET QUE LE TENSEUR DE CONDUCTIVITE EN CHAQUE POINT
C ---   EST DIAGONAL ET QUE SES VALEURS PROPRES SONT
C ---   LAMBDA_1 , LAMBDA_2 ET LAMBDA_3
C ---   D'AUTRE PART, SOIENT P1(X3), P2(X3), P3(X3) LES POLYNOMES
C ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
C ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
C ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
C ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
C ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
C ---   (I.E. T(X1,X2,X3) =    P1(X3)*TMOY(X1,X2)
C ---                        + P2(X3)*TINF(X1,X2)
C ---                        + P3(X3)*TSUP(X1,X2)
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

C ---  INTERPOLATION DES TERMES DU TENSEUR DE CONDUCTIVITE
C ---  EN FONCTION DU TEMPS ET DE LA TEMPERATURE :
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C      --------------------------
        NBV = 12
        CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ',PHENOM,NBVAR,NOMPAR,
     &              VALPAR,NBV,NOMRES,VALRES,ICODRE,1)

C ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
C ---   DE REFERENCE AU REPERE DE L'ELEMENT :
C       -----------------------------------
        CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     &              ANG)

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

      ELSE
        CALL U2MESK('F','ELEMENTS3_17',1,PHENOM)
      END IF

C======================================
C --- CALCUL DE LA RIGIDITE THERMIQUE =
C======================================

C --- CAS DES COQUES SURFACIQUES :
C     --------------------------
      IF (NOMTE.NE.'THCPSE3 ' .AND. NOMTE.NE.'THCASE3 ') THEN

C ---   CALCUL DES COORDONNEES DES CONNECTIVITES DANS LE REPERE
C ---   DE L'ELEMENT :
C       ------------
        CALL CQ3D2D(NNO,ZR(IGEOM),UN,ZERO,COOR2D)

C ---  CALCUL DE LA RIGIDITE THERMIQUE MEMBRANAIRE :
C      -------------------------------------------

C ---  BOUCLE SUR LES POINTS D'INTEGRATION :
C      -----------------------------------
        DO 130 KP = 1,NPG1
          K = (KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR2D,DFDX,DFDY,POIDS)
          DO 120 GI = 1,NNO
            DO 110 GJ = 1,GI
              DO 100 PI = 1,3
                DO 90 PJ = 1,PI
                  PK = A(PI,PJ,1,1)*DFDX(GI)*DFDX(GJ) +
     &                 A(PI,PJ,2,2)*DFDY(GI)*DFDY(GJ) +
     &                 A(PI,PJ,1,2)*DFDX(GI)*DFDY(GJ) +
     &                 A(PI,PJ,1,2)*DFDY(GI)*DFDX(GJ)
                  PK = PK*POIDS*THETA

C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                  IF ((PI.NE.PJ) .AND. (GI.NE.GJ)) THEN
                    I = 3* (GI-1) + PJ
                    J = 3* (GJ-1) + PI
                    RIGITH(I,J) = RIGITH(I,J) + PK
                  END IF

C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                  I = 3* (GI-1) + PI
                  J = 3* (GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + PK
   90           CONTINUE
  100         CONTINUE
  110       CONTINUE
  120     CONTINUE
  130   CONTINUE

C ---  CALCUL DE LA RIGIDITE THERMIQUE TRANSVERSE :
C      ------------------------------------------

C ---  UTILISATION D'UNE INTEGRATION AVEC UN NOMBRE DE POINTS
C ---  SUPERIEUR OU EGAL AU NOMBRE DE POINTS UTILISES POUR LA
C ---  RIGIDITE MEMBRANAIRE :
C      --------------------
        CALL ELREF4(' ','MASS',NDIM2,NNO2,NNOS2,NPG2,
     &                   IPOIDS,IVF,IDFDE,JGANO2)

C ---  BOUCLE SUR LES POINTS D'INTEGRATION :
C      -----------------------------------
        DO 180 KP = 1,NPG2
          K = (KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR2D,DFDX,DFDY,POIDS)
          DO 170 GI = 1,NNO
            DO 160 GJ = 1,GI
              DO 150 PI = 1,3
                DO 140 PJ = 1,PI
                  PK = B(PI,PJ)*ZR(IVF+K+GI-1)*ZR(IVF+K+GJ-1)*
     &            POIDS*THETA

C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                  IF ((PI.NE.PJ) .AND. (GI.NE.GJ)) THEN
                    I = 3* (GI-1) + PJ
                    J = 3* (GJ-1) + PI
                    RIGITH(I,J) = RIGITH(I,J) + PK
                  END IF

C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                  I = 3* (GI-1) + PI
                  J = 3* (GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + PK
  140           CONTINUE
  150         CONTINUE
  160       CONTINUE
  170     CONTINUE
  180   CONTINUE

C --- CAS DES COQUES LINEIQUES :
C     ------------------------
      ELSE

C ---  CALCUL DE LA RIGIDITE THERMIQUE MEMBRANAIRE :
C      -------------------------------------------

C ---  BOUCLE SUR LES POINTS D'INTEGRATION :
C      -----------------------------------
        DO 240 KP = 1,NPG1
          K = (KP-1)*NNO
          CALL DFDM1D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),
     &               DFDX,COUR,POIDS,COSA,SINA)

          IF (NOMTE.EQ.'THCASE3') THEN
            R = ZERO
            DO 190 I = 1,NNO
              R = R + ZR(IGEOM+2* (I-1))*ZR(IVF+K+I-1)
  190       CONTINUE
            POIDS = POIDS*R
          END IF

          DO 230 GI = 1,NNO
            DO 220 GJ = 1,GI
              DO 210 PI = 1,3
                DO 200 PJ = 1,PI
                  PK = A(PI,PJ,1,1)*DFDX(GI)*DFDX(GJ)
                  PK = PK*POIDS*THETA

C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                  IF ((PI.NE.PJ) .AND. (GI.NE.GJ)) THEN
                    I = 3* (GI-1) + PJ
                    J = 3* (GJ-1) + PI
                    RIGITH(I,J) = RIGITH(I,J) + PK
                  END IF

C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                  I = 3* (GI-1) + PI
                  J = 3* (GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + PK
  200           CONTINUE
  210         CONTINUE
  220       CONTINUE
  230     CONTINUE
  240   CONTINUE

C ---  CALCUL DE LA RIGIDITE THERMIQUE TRANSVERSE :
C      ------------------------------------------

C ---  BOUCLE SUR LES POINTS D'INTEGRATION :
C      -----------------------------------
        DO 290 KP = 1,NPG1
          K = (KP-1)*NNO
          CALL DFDM1D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),
     &                DFDX,COUR,POIDS,COSA,SINA)
          DO 280 GI = 1,NNO
            DO 270 GJ = 1,GI
              DO 260 PI = 1,3
                DO 250 PJ = 1,PI
                  PK = B(PI,PJ)*ZR(IVF+K+GI-1)*ZR(IVF+K+GJ-1)*POIDS*
     &                 THETA

C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                  IF ((PI.NE.PJ) .AND. (GI.NE.GJ)) THEN
                    I = 3* (GI-1) + PJ
                    J = 3* (GJ-1) + PI
                    RIGITH(I,J) = RIGITH(I,J) + PK
                  END IF

C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                  I = 3* (GI-1) + PI
                  J = 3* (GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + PK
  250           CONTINUE
  260         CONTINUE
  270       CONTINUE
  280     CONTINUE
  290   CONTINUE

      END IF

C --- RECUPERATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
C     ----------------------------------------------------------------
      CALL JEVECH('PMATTTR','E',IMATTT)

C --- AFFECTATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
C     ---------------------------------------------------------------
      NBDDL = 3*NNO
      IND = 0
      DO 310 I = 1,NBDDL
        DO 300 J = 1,I
          IND = IND + 1
          ZR(IMATTT+IND-1) = RIGITH(I,J)
  300   CONTINUE
  310 CONTINUE

      END
