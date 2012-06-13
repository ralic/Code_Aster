      SUBROUTINE TE0104 ( OPTION , NOMTE )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16  OPTION, NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_THER_COEH_R'
C                          CAS COQUE
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
      INTEGER    NDIMAX,IND,NBDDL,KP,KQ
      PARAMETER (NDIMAX=27)
      REAL*8     B(3,3),LAMB,THETA,RIGITH(NDIMAX,NDIMAX)
      REAL*8     COOR2D(18),COUR,COSA,SINA,R,ZERO
      REAL*8     DFDX(9),DFDY(9),POIDS,POI1,POI2,HPLUS,HMOINS,PK
      REAL*8     LONG,MATN(3,3),MATP(3,3)
      INTEGER    NNO,NNOS,NPG2,GI,PI,GJ,PJ,K,IMATTT,JGANO,NDIM
      INTEGER    IPOIDS,IVF,IDFDE,IGEOM,NPG1,I,J,ICOEFH,ITEMPS,MZR


      IF (NOMTE.NE.'THCPSE3 ' .AND. NOMTE.NE.'THCASE3 ' .AND.
     &    NOMTE.NE.'THCOSE3 ' .AND. NOMTE.NE.'THCOSE2 ') THEN
        CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,
     &                         IDFDE,JGANO)
      ELSE
        CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,
     &                         IDFDE,JGANO)
        NPG2 = 3
      END IF
C
C --- INITIALISATIONS :
C     ---------------
      ZERO = 0.0D0

      DO 20 I = 1,NDIMAX
        DO 10 J = 1,NDIMAX
          RIGITH(I,J) = ZERO
   10   CONTINUE
   20 CONTINUE

C --- RECUPERATION DES COORDONNEES DES NOEUDS DE L'ELEMENT :
C     ----------------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C --- RECUPERATION DU COEFFICIENT D'ECHANGE :
C     -------------------------------------
      CALL JEVECH('PCOEFHR','L',ICOEFH)

C --- RECUPERATION DE L'INSTANT DU CALCUL ET
C --- DU PARAMETRE THETA DE LA METHODE 'THETA' UTILISEE
C --- POUR RESOUDRE L'EQUATION DIFFERENTIELLE EN TEMPS DE LA
C --- TEMPERATURE (EN STATIONNAIRE THETA =1 ) :
C     ---------------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      THETA = ZR(ITEMPS+2)

C --- DETERMINATION DE LA CONTRIBUTION A LA RIGIDITE THERMIQUE
C --- DES ECHANGES DE LA COQUE AVEC L'EXTERIEUR AU NIVEAU DES
C --- FEUILLETS INFERIEUR ET SUPERIEUR :
C     ================================

C --- CAS DES COQUES SURFACIQUES :
C     --------------------------
      IF (NOMTE(1:8).NE.'THCOSE3 ' .AND. NOMTE(1:8).NE.'THCOSE2 ') THEN

C ---   CALCUL DU COEFFICIENT D'ECHANGE DU FEUILLET INFERIEUR
C ---   DE LA COQUE AVEC L'EXTERIEUR :
C       ----------------------------
        HMOINS = ZR(ICOEFH)

C ---   CALCUL DU COEFFICIENT D'ECHANGE DU FEUILLET SUPERIEUR
C ---   DE LA COQUE AVEC L'EXTERIEUR :
C       ----------------------------
        HPLUS = ZR(ICOEFH+1)

C ---   CONTRIBUTION AU TENSEUR DE CONDUCTIVITE TRANSVERSE B DES
C ---   ECHANGES AVEC L'EXTERIEUR
C ---            (0 0  0 )
C ---       B =  (0 H- 0 )
C ---            (0 0  H+)
C       -------------------
        B(1,1) = ZERO
        B(2,1) = ZERO
        B(2,2) = HMOINS
        B(3,1) = ZERO
        B(3,2) = ZERO
        B(3,3) = HPLUS
      END IF

      IF (NOMTE(1:8).NE.'THCPSE3 ' .AND. NOMTE(1:8).NE.'THCASE3 ' .AND.
     &    NOMTE(1:8).NE.'THCOSE3 ' .AND. NOMTE(1:8).NE.'THCOSE2 ') THEN

C ---  CALCUL DE LA RIGIDITE THERMIQUE DUE AU TERME D'ECHANGE B :
C      ========================================================

C ---   DETERMINATION DES COORDONNEES COOR2D DES NOEUDS DE L'ELEMENT
C ---   DANS LE REPERE DE L'ELEMENT :
C       ---------------------------
        CALL CQ3D2D(NNO,ZR(IGEOM),1.D0,ZERO,COOR2D)

C ---   BOUCLE SUR LES POINTS D'INTEGRATION :
C       -----------------------------------
        DO 70 KP = 1,NPG2
          K = (KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COOR2D,DFDX,DFDY,POIDS)
          DO 60 GI = 1,NNO
            DO 50 GJ = 1,GI
              DO 40 PI = 1,3
                DO 30 PJ = 1,PI
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
   30           CONTINUE
   40         CONTINUE
   50       CONTINUE
   60     CONTINUE
   70   CONTINUE

C --- CAS DES COQUES LINEIQUES (EN CONTRAINTES PLANES ET AXI) :
C     -------------------------------------------------------
      ELSE IF (NOMTE(1:8).EQ.'THCPSE3 ' .OR.
     &         NOMTE(1:8).EQ.'THCASE3 ') THEN

C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 130 KP = 1,NPG2
          K = (KP-1)*NNO
          CALL DFDM1D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),DFDX,
     &                COUR,POIDS,COSA,SINA)

          IF (NOMTE.EQ.'THCASE3') THEN
            R = ZERO
            DO 80 I = 1,NNO
              R = R + ZR(IGEOM+2* (I-1))*ZR(IVF+K+I-1)
   80       CONTINUE
            POIDS = POIDS*R
          END IF

          DO 120 GI = 1,NNO
            DO 110 GJ = 1,GI
              DO 100 PI = 1,3
                DO 90 PJ = 1,PI
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
   90           CONTINUE
  100         CONTINUE
  110       CONTINUE
  120     CONTINUE
  130   CONTINUE

C --- CAS DES COQUES LINEIQUES (AUTRES QUE CONTRAINTES PLANES ET AXI) :
C     --------------------------------------------------------------
      ELSE IF (NOMTE(1:8).EQ.'THCOSE3 ' .OR.
     &         NOMTE(1:8).EQ.'THCOSE2 ') THEN


        CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DEMR',' ',MZR)
CCC     CALL JEVECH('PCACOQU','L',ICACOQ)

CCC     EP=ZR(ICACOQ)

        LONG = (ZR(IGEOM+3)-ZR(IGEOM))**2 +
     &         (ZR(IGEOM+3+1)-ZR(IGEOM+1))**2 +
     &         (ZR(IGEOM+3+2)-ZR(IGEOM+2))**2
        LONG = SQRT(LONG)/2.D0
C       EP  =EP/2.D0

C      IMPORTANT: LAMB = CONV * EPAISSEUR

        LAMB = ZR(ICOEFH)/2.D0

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
        DO 150 I = 1,3
          DO 140 J = 1,3
            MATP(I,J) = ZERO
            MATN(I,J) = ZERO
  140     CONTINUE
  150   CONTINUE

C ---   DETERMINATION DE LA MATRICE MATP DONT LE TERME GENERIQUE
C ---   EST MATP(I,J) = SOMME_EPAISSEUR(PI(Z)*PJ(Z).DZ) :
C       -----------------------------------------------
        DO 160 KP = 1,NPG2
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
  160   CONTINUE

C ---   DETERMINATION DE LA MATRICE MATN DONT LE TERME GENERIQUE
C ---   EST MATN(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY) :
C       --------------------------------------------------------
        DO 170 KP = 1,NPG1
          KQ = (KP-1)*NNO

          POI2 = ZR(IPOIDS-1+KP)

          MATN(1,1) = MATN(1,1) + POI2*ZR(IVF-1+KQ+1)**2
          MATN(1,2) = MATN(1,2) + POI2*ZR(IVF-1+KQ+1)*ZR(IVF-1+KQ+2)
          MATN(2,1) = MATN(1,2)
          MATN(2,2) = MATN(2,2) + POI2*ZR(IVF-1+KQ+2)**2

          IF (NOMTE(1:8).EQ.'THCOSE3 ') THEN
            MATN(1,3) = MATN(1,3) + POI2*ZR(IVF-1+KQ+1)*ZR(IVF-1+KQ+3)
            MATN(2,3) = MATN(2,3) + POI2*ZR(IVF-1+KQ+2)*ZR(IVF-1+KQ+3)
            MATN(3,1) = MATN(1,3)
            MATN(3,2) = MATN(2,3)
            MATN(3,3) = MATN(3,3) + POI2*ZR(IVF-1+KQ+3)**2
          END IF

  170   CONTINUE

        RIGITH(1,1) = MATN(1,1)*MATP(1,1)
        RIGITH(1,2) = MATN(1,1)*MATP(1,2)
        RIGITH(1,3) = MATN(1,1)*MATP(1,3)
        RIGITH(1,4) = MATN(1,2)*MATP(1,1)
        RIGITH(1,5) = MATN(1,2)*MATP(1,2)
        RIGITH(1,6) = MATN(1,2)*MATP(1,3)

        RIGITH(2,1) = RIGITH(1,2)
        RIGITH(2,2) = MATN(1,1)*MATP(2,2)
        RIGITH(2,3) = MATN(1,1)*MATP(2,3)
        RIGITH(2,4) = MATN(1,2)*MATP(2,1)
        RIGITH(2,5) = MATN(1,2)*MATP(2,2)
        RIGITH(2,6) = MATN(1,2)*MATP(2,3)

        RIGITH(3,1) = RIGITH(1,3)
        RIGITH(3,2) = RIGITH(2,3)
        RIGITH(3,3) = MATN(1,1)*MATP(3,3)
        RIGITH(3,4) = MATN(1,2)*MATP(3,1)
        RIGITH(3,5) = MATN(1,2)*MATP(3,2)
        RIGITH(3,6) = MATN(1,2)*MATP(3,3)

        RIGITH(4,1) = RIGITH(1,4)
        RIGITH(4,2) = RIGITH(2,4)
        RIGITH(4,3) = RIGITH(3,4)
        RIGITH(4,4) = MATN(2,2)*MATP(1,1)
        RIGITH(4,5) = MATN(2,2)*MATP(1,2)
        RIGITH(4,6) = MATN(2,2)*MATP(1,3)

        RIGITH(5,1) = RIGITH(1,5)
        RIGITH(5,2) = RIGITH(2,5)
        RIGITH(5,3) = RIGITH(3,5)
        RIGITH(5,4) = RIGITH(4,5)
        RIGITH(5,5) = MATN(2,2)*MATP(2,2)
        RIGITH(5,6) = MATN(2,2)*MATP(2,3)

        RIGITH(6,1) = RIGITH(1,6)
        RIGITH(6,2) = RIGITH(2,6)
        RIGITH(6,3) = RIGITH(3,6)
        RIGITH(6,4) = RIGITH(4,6)
        RIGITH(6,5) = RIGITH(5,6)
        RIGITH(6,6) = MATN(2,2)*MATP(3,3)

        IF (NOMTE(1:8).EQ.'THCOSE3 ') THEN

          RIGITH(1,7) = MATN(1,3)*MATP(1,1)
          RIGITH(1,8) = MATN(1,3)*MATP(1,2)
          RIGITH(1,9) = MATN(1,3)*MATP(1,3)

          RIGITH(2,7) = MATN(1,3)*MATP(2,1)
          RIGITH(2,8) = MATN(1,3)*MATP(2,2)
          RIGITH(2,9) = MATN(1,3)*MATP(2,3)

          RIGITH(3,7) = MATN(1,3)*MATP(3,1)
          RIGITH(3,8) = MATN(1,3)*MATP(3,2)
          RIGITH(3,9) = MATN(1,3)*MATP(3,3)

          RIGITH(4,7) = MATN(2,3)*MATP(1,1)
          RIGITH(4,8) = MATN(2,3)*MATP(1,2)
          RIGITH(4,9) = MATN(2,3)*MATP(1,3)

          RIGITH(5,7) = MATN(2,3)*MATP(2,1)
          RIGITH(5,8) = MATN(2,3)*MATP(2,2)
          RIGITH(5,9) = MATN(2,3)*MATP(2,3)

          RIGITH(6,7) = MATN(2,3)*MATP(3,1)
          RIGITH(6,8) = MATN(2,3)*MATP(3,2)
          RIGITH(6,9) = MATN(2,3)*MATP(3,3)

          RIGITH(7,1) = RIGITH(1,7)
          RIGITH(7,2) = RIGITH(2,7)
          RIGITH(7,3) = RIGITH(3,7)
          RIGITH(7,4) = RIGITH(4,7)
          RIGITH(7,5) = RIGITH(5,7)
          RIGITH(7,6) = RIGITH(6,7)
          RIGITH(7,7) = MATN(3,3)*MATP(1,1)
          RIGITH(7,8) = MATN(3,3)*MATP(1,2)
          RIGITH(7,9) = MATN(3,3)*MATP(1,3)

          RIGITH(8,1) = RIGITH(1,8)
          RIGITH(8,2) = RIGITH(2,8)
          RIGITH(8,3) = RIGITH(3,8)
          RIGITH(8,4) = RIGITH(4,8)
          RIGITH(8,5) = RIGITH(5,8)
          RIGITH(8,6) = RIGITH(6,8)
          RIGITH(8,7) = RIGITH(7,8)
          RIGITH(8,8) = MATN(3,3)*MATP(2,2)
          RIGITH(8,9) = MATN(3,3)*MATP(2,3)

          RIGITH(9,1) = RIGITH(1,9)
          RIGITH(9,2) = RIGITH(2,9)
          RIGITH(9,3) = RIGITH(3,9)
          RIGITH(9,4) = RIGITH(4,9)
          RIGITH(9,5) = RIGITH(5,9)
          RIGITH(9,6) = RIGITH(6,9)
          RIGITH(9,7) = RIGITH(7,9)
          RIGITH(9,8) = RIGITH(8,9)
          RIGITH(9,9) = MATN(3,3)*MATP(3,3)
        END IF

C       LAMB=LAMB*LONG*THETA*EP
        LAMB = LAMB*LONG*THETA

        DO 190 I = 1,3*NNO
          DO 180 J = 1,I
            RIGITH(I,J) = RIGITH(I,J)*LAMB
  180     CONTINUE
  190   CONTINUE

      END IF

C --- RECUPERATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
C     ----------------------------------------------------------------
      CALL JEVECH('PMATTTR','E',IMATTT)

C --- AFFECTATION DE LA MATRICE DE RIGIDITE THERMIQUE EN SORTIE DU TE :
C     ---------------------------------------------------------------
      NBDDL = 3*NNO
      IND = 0
      DO 210 I = 1,NBDDL
        DO 200 J = 1,I
          IND = IND + 1
          ZR(IMATTT+IND-1) = RIGITH(I,J)
  200   CONTINUE
  210 CONTINUE

      END
